{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.Exec where

import Control.Lens
import Control.Monad (when)
import DB.Options
import Data.ByteString.Char8 qualified as BS8
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy.IO qualified as TLIO
import Data.Time (getCurrentTime, utctDay)
import Org.DB
import Org.Data
import Org.Dot
import Org.Types

execDb :: Config -> DbOptions -> Collection -> IO ()
execDb _cfg opts coll = do
  let dbCfg = DBConfig (BS8.pack (opts ^. dbConnStr))
  case opts ^. dbCommand of
    DBInit dims -> withDB dbCfg $ \db -> do
      initDB db dims
      result <- runMigrations db
      case result of
        MigrationsApplied n ->
          putStrLn $ "Database initialized (dimensions=" ++ show dims ++ "). " ++ show n ++ " migration(s) applied."
        AlreadyCurrent ->
          putStrLn $ "Database initialized (dimensions=" ++ show dims ++ ", schema up to date)."
        MigrationFailed v msg ->
          putStrLn $ "Database initialized, but migration " ++ show v ++ " failed: " ++ T.unpack msg
    DBUnstore -> withDB dbCfg $ \db -> do
      unstoreDB db
      putStrLn "All data tables dropped. Run 'org db init --dimensions <N>' to reinitialize."
    DBStore sopts -> withDB dbCfg $ \db -> do
      dims <- requireEmbeddingDimensions db
      _ <- runMigrations db
      sr <- storeCollection db coll
      putStrLn $
        "Stored: "
          ++ show (storeFilesProcessed sr)
          ++ " file(s) processed, "
          ++ show (storeFilesInserted sr)
          ++ " inserted, "
          ++ show (storeFilesUpdated sr)
          ++ " updated, "
          ++ show (storeFilesSkipped sr)
          ++ " skipped"
      putStrLn $
        "Entries: "
          ++ show (storeEntriesCreated sr)
          ++ " created, "
          ++ show (storeEntriesUpdated sr)
          ++ " updated, "
          ++ show (storeEntriesDeleted sr)
          ++ " deleted"
      mapM_ (\e -> putStrLn $ "Error: " ++ T.unpack e) (storeErrors sr)
      if sopts ^. storeNoEmbed
        then pure ()
        else do
          let eopts = sopts ^. storeEmbedOpts
          when (eopts ^. embedForce) $ do
            putStrLn "Clearing all embedding hashes (--force)..."
            dbExecute_ db "UPDATE entries SET embedding_hash = NULL, title_embedding = NULL" []
          let ecfg =
                EmbedConfig
                  { embedBaseUrl = T.pack (eopts ^. embedBaseUrlOpt)
                  , embedModel = T.pack (eopts ^. embedModelOpt)
                  , embedApiKey = T.pack (eopts ^. embedApiKeyOpt)
                  , embedBatchSize = eopts ^. embedBatchSizeOpt
                  , embedDimensions = dims
                  , embedChunkSize = eopts ^. embedChunkSizeOpt
                  }
              progress done total =
                putStrLn $
                  "Embedded " ++ show done ++ " / " ++ show total ++ " chunks"
          result <- embedEntries db ecfg progress
          case embedProcessed result of
            0 -> pure ()
            n -> do
              putStrLn $
                "Embedding done. Processed: "
                  ++ show n
                  ++ ", Failed: "
                  ++ show (embedFailed result)
              mapM_ (\e -> putStrLn $ "Error: " ++ T.unpack e) (embedErrors result)
          let titleProgress done total =
                putStrLn $
                  "Title embeddings: " ++ show done ++ " / " ++ show total
          titleResult <- embedTitles db ecfg titleProgress
          case embedProcessed titleResult of
            0 -> pure ()
            n -> do
              putStrLn $
                "Title embedding done. Processed: "
                  ++ show n
                  ++ ", Failed: "
                  ++ show (embedFailed titleResult)
              mapM_ (\e -> putStrLn $ "Error: " ++ T.unpack e) (embedErrors titleResult)
    DBQuery qopts -> withDB dbCfg $ \db -> do
      rows <- case qopts ^. queryOrgQl of
        Just ql -> do
          today <- utctDay <$> getCurrentTime
          queryByOrgQl db today (T.pack ql)
        Nothing -> case (qopts ^. queryKeyword, qopts ^. queryTag) of
          (Just kw, _) -> queryEntriesByKeyword db (T.pack kw)
          (_, Just tag) -> queryEntriesByTag db (T.pack tag)
          _ -> queryEntries db
      let limited = case qopts ^. queryLimit of
            Just n -> take n rows
            Nothing -> rows
      case qopts ^. queryFormat of
        TextFormat -> mapM_ printEntryRow limited
        CsvFormat -> do
          putStrLn "id,file_id,depth,keyword,title"
          mapM_ printEntryRowCsv limited
        JsonFormat -> mapM_ printEntryRowJson limited
    DBSync sopts -> withDB dbCfg $ \db -> do
      _ <- requireEmbeddingDimensions db
      case sopts ^. syncDirection of
        "from-db" -> do
          dbColl <- syncFromDb db
          let count = length (dbColl ^.. items . traverse . _OrgItem)
          putStrLn $ "Loaded " ++ show count ++ " file(s) from database."
        "bidirectional" -> do
          result <- syncBidirectional db coll
          printSyncResult result
        _ -> do
          result <- syncToDb db coll
          printSyncResult result
    DBDump dopts -> withDB dbCfg $ \db -> do
      let table = T.pack (dopts ^. dumpTable)
          limitClause = case dopts ^. dumpLimit of
            Just n -> " LIMIT " <> T.pack (show n)
            Nothing -> ""
      rows <- dbQuery db ("SELECT * FROM " <> table <> " ORDER BY 1" <> limitClause) [] :: IO [[SqlValue]]
      mapM_ (putStrLn . showSqlRow) rows
    DBEmbed eopts -> withDB dbCfg $ \db -> do
      dims <- requireEmbeddingDimensions db
      _ <- runMigrations db
      when (eopts ^. embedForce) $ do
        putStrLn "Clearing all embedding hashes (--force)..."
        dbExecute_ db "UPDATE entries SET embedding_hash = NULL, title_embedding = NULL" []
      let ecfg =
            EmbedConfig
              { embedBaseUrl = T.pack (eopts ^. embedBaseUrlOpt)
              , embedModel = T.pack (eopts ^. embedModelOpt)
              , embedApiKey = T.pack (eopts ^. embedApiKeyOpt)
              , embedBatchSize = eopts ^. embedBatchSizeOpt
              , embedDimensions = dims
              , embedChunkSize = eopts ^. embedChunkSizeOpt
              }
          progress done total =
            putStrLn $
              "Embedded " ++ show done ++ " / " ++ show total ++ " chunks"
      result <- embedEntries db ecfg progress
      putStrLn $
        "Done. Processed: "
          ++ show (embedProcessed result)
          ++ ", Failed: "
          ++ show (embedFailed result)
      mapM_ (\e -> putStrLn $ "Error: " ++ T.unpack e) (embedErrors result)
      let titleProgress done total =
            putStrLn $
              "Title embeddings: " ++ show done ++ " / " ++ show total
      titleResult <- embedTitles db ecfg titleProgress
      putStrLn $
        "Title embedding done. Processed: "
          ++ show (embedProcessed titleResult)
          ++ ", Failed: "
          ++ show (embedFailed titleResult)
      mapM_ (\e -> putStrLn $ "Error: " ++ T.unpack e) (embedErrors titleResult)
    DBDot dopts -> withDB dbCfg $ \db -> do
      rels <- dbQuery db "SELECT * FROM entry_relationships" [] :: IO [RelationshipRow]
      entryRows <- queryEntries db
      let cfg =
            defaultDotConfig
              { dotRelationshipTypes =
                  case dopts ^. dotRelTypes of
                    [] -> Nothing
                    ts -> Just (map T.pack ts)
              , dotFilterFiles =
                  case dopts ^. dotFilterFile of
                    [] -> Nothing
                    fs -> Just (map T.pack fs)
              , dotFilterKeywords =
                  case dopts ^. dotFilterKw of
                    [] -> Nothing
                    ks -> Just (map T.pack ks)
              , dotMaxDepth = dopts ^. dotMaxDepthOpt
              }
          dotText = relationshipsDotText cfg rels entryRows
      case dopts ^. dotOutput of
        Just path -> TLIO.writeFile path dotText
        Nothing -> TLIO.putStr dotText
    DBSearch sopts -> withDB dbCfg $ \db -> do
      dims <- requireEmbeddingDimensions db
      let ecfg =
            EmbedConfig
              { embedBaseUrl = T.pack (sopts ^. searchBaseUrlOpt)
              , embedModel = T.pack (sopts ^. searchModelOpt)
              , embedApiKey = T.pack (sopts ^. searchApiKeyOpt)
              , embedBatchSize = 1
              , embedDimensions = dims
              , embedChunkSize = 8000
              }
      vec <- embedQuery ecfg (T.pack (sopts ^. searchQuery))
      results <- querySimilar db vec (sopts ^. searchLimit)
      case sopts ^. searchFormat of
        TextFormat -> mapM_ (\(row, src, _, _) -> printSearchRow db row src) results
        CsvFormat -> do
          putStrLn "id,file_id,depth,keyword,title"
          mapM_ (\(row, _, _, _) -> printEntryRowCsv row) results
        JsonFormat -> mapM_ (\(row, src, chunk, dist) -> printSearchRowJson db row src chunk dist) results
    DBReview ropts -> withDB dbCfg $ \db -> do
      _ <- requireEmbeddingDimensions db
      _ <- runMigrations db
      pairs <- queryReviewGroups db (ropts ^. reviewThreshold) (ropts ^. reviewLimit)
      if null pairs
        then putStrLn "No similar title groups found."
        else do
          let groups = clusterPairs pairs
          case ropts ^. reviewFormat of
            TextFormat -> printReviewGroups groups
            JsonFormat -> printReviewGroupsJson groups
            CsvFormat -> printReviewGroupsCsv groups

printEntryRow :: EntryRow -> IO ()
printEntryRow row =
  putStrLn $
    maybe "" T.unpack (erPath row)
      ++ ":"
      ++ show (erByteOffset row)
      ++ " "
      ++ maybe "" (\kw -> T.unpack kw ++ " ") (erKeywordValue row)
      ++ T.unpack (erTitle row)

printEntryRowCsv :: EntryRow -> IO ()
printEntryRowCsv row =
  TIO.putStrLn $
    erId row
      <> ","
      <> erFileId row
      <> ","
      <> T.pack (show (erDepth row))
      <> ","
      <> fromMaybe "" (erKeywordValue row)
      <> ","
      <> csvEscape (erTitle row)

printEntryRowJson :: EntryRow -> IO ()
printEntryRowJson row =
  TIO.putStrLn $
    "{\"id\":\""
      <> erId row
      <> "\",\"title\":\""
      <> jsonEscape (erTitle row)
      <> "\",\"keyword\":"
      <> maybe "null" (\k -> "\"" <> k <> "\"") (erKeywordValue row)
      <> ",\"depth\":"
      <> T.pack (show (erDepth row))
      <> "}"

printSearchRow :: DBHandle -> EntryRow -> T.Text -> IO ()
printSearchRow db row src = do
  fpath <- fromMaybe (erFileId row) <$> queryFilePath db (erFileId row)
  ancestors <- queryAncestorTitles db row
  let hierarchy = case ancestors of
        [] -> ""
        ts -> T.intercalate " > " ts <> " > "
      srcSuffix = if T.null src then "" else " [" ++ T.unpack src ++ "]"
  putStrLn $
    T.unpack fpath
      ++ ":"
      ++ show (erByteOffset row)
      ++ " "
      ++ maybe "" (\kw -> T.unpack kw ++ " ") (erKeywordValue row)
      ++ T.unpack hierarchy
      ++ T.unpack (erTitle row)
      ++ srcSuffix

printSearchRowJson :: DBHandle -> EntryRow -> T.Text -> T.Text -> Double -> IO ()
printSearchRowJson db row src chunk dist = do
  fpath <- fromMaybe (erFileId row) <$> queryFilePath db (erFileId row)
  TIO.putStrLn $
    "{\"id\":\""
      <> erId row
      <> "\",\"file\":\""
      <> jsonEscape fpath
      <> "\",\"title\":\""
      <> jsonEscape (erTitle row)
      <> "\",\"headline\":\""
      <> jsonEscape (erHeadline row)
      <> "\",\"keyword\":"
      <> maybe "null" (\k -> "\"" <> k <> "\"") (erKeywordValue row)
      <> ",\"depth\":"
      <> T.pack (show (erDepth row))
      <> ",\"byte_offset\":"
      <> T.pack (show (erByteOffset row))
      <> ",\"distance\":"
      <> T.pack (show dist)
      <> ",\"matched_source\":\""
      <> jsonEscape src
      <> "\",\"matched_text\":\""
      <> jsonEscape chunk
      <> "\"}"

-- | Walk up parent_id chain to collect ancestor titles (root first).
queryAncestorTitles :: DBHandle -> EntryRow -> IO [T.Text]
queryAncestorTitles db row = go [] (erParentId row)
 where
  go acc Nothing = pure acc
  go acc (Just pid) = do
    mParent <-
      dbQueryOne
        db
        "SELECT id, file_id, parent_id, depth, position, byte_offset, \
        \keyword_type, keyword_value, priority, headline, title, verb, \
        \context, locator, hash, mod_time, created_time, path::text \
        \FROM entries WHERE id = ?"
        [SqlText pid] ::
        IO (Maybe EntryRow)
    case mParent of
      Nothing -> pure acc
      Just p -> go (erTitle p : acc) (erParentId p)

csvEscape :: T.Text -> T.Text
csvEscape t
  | T.any (\c -> c == ',' || c == '"' || c == '\n') t =
      "\"" <> T.replace "\"" "\"\"" t <> "\""
  | otherwise = t

jsonEscape :: T.Text -> T.Text
jsonEscape =
  T.replace "\n" "\\n"
    . T.replace "\r" "\\r"
    . T.replace "\t" "\\t"
    . T.replace "\"" "\\\""
    . T.replace "\\" "\\\\"

showSqlRow :: [SqlValue] -> String
showSqlRow = unwords . map showSqlValue

showSqlValue :: SqlValue -> String
showSqlValue (SqlText t) = T.unpack t
showSqlValue (SqlInt i) = show i
showSqlValue (SqlDouble d) = show d
showSqlValue (SqlBool b) = show b
showSqlValue (SqlBlob _) = "<blob>"
showSqlValue (SqlUTCTime t) = show t
showSqlValue (SqlDay d) = show d
showSqlValue SqlNull = "NULL"

printSyncResult :: SyncResult -> IO ()
printSyncResult r = do
  putStrLn $
    "Processed: "
      ++ show (syncFilesProcessed r)
      ++ ", Updated: "
      ++ show (syncFilesUpdated r)
      ++ ", Skipped: "
      ++ show (syncFilesSkipped r)
  mapM_ printConflict (syncConflicts r)
  mapM_ (\e -> putStrLn $ "Error: " ++ T.unpack e) (syncErrors r)

printConflict :: SyncConflict -> IO ()
printConflict c =
  putStrLn $
    "Conflict: "
      ++ T.unpack (conflictFile c)
      ++ " - "
      ++ T.unpack (conflictReason c)

------------------------------------------------------------------------
-- Review: union-find clustering and display
------------------------------------------------------------------------

-- | Cluster similar-title pairs into connected components.
clusterPairs :: [(EntryRow, EntryRow, Double)] -> [[(EntryRow, Double)]]
clusterPairs pairs =
  let entryMap =
        Map.fromList $
          concatMap (\(a, b, _) -> [(erId a, a), (erId b, b)]) pairs
      edges = concatMap (\(a, b, d) -> [(erId a, erId b, d), (erId b, erId a, d)]) pairs
      parentMap = buildComponents (Map.keys entryMap) [(erId a, erId b) | (a, b, _) <- pairs]
      -- Build minimum distance per entry from its edges
      distMap = Map.fromListWith min [(src, d) | (src, _, d) <- edges]
      componentMap =
        Map.fromListWith
          (++)
          [ (rep, [(eid, Map.findWithDefault 1.0 eid distMap)])
          | eid <- Map.keys entryMap
          , let rep = findRoot parentMap eid
          ]
      groups =
        [ [(er, dist) | (eid, dist) <- members, Just er <- [Map.lookup eid entryMap]]
        | members <- Map.elems componentMap
        , length members > 1
        ]
   in sortBy (comparing groupMinDist) groups
 where
  groupMinDist grp = minimum [d | (_, d) <- grp]

buildComponents :: [T.Text] -> [(T.Text, T.Text)] -> Map.Map T.Text T.Text
buildComponents nodes =
  foldl (\m (a, b) -> unionNodes m a b) initial
 where
  initial = Map.fromList [(n, n) | n <- nodes]

findRoot :: Map.Map T.Text T.Text -> T.Text -> T.Text
findRoot m x = case Map.lookup x m of
  Just p | p /= x -> findRoot m p
  _ -> x

unionNodes :: Map.Map T.Text T.Text -> T.Text -> T.Text -> Map.Map T.Text T.Text
unionNodes m a b =
  let ra = findRoot m a
      rb = findRoot m b
   in if ra == rb then m else Map.insert ra rb m

printReviewGroups :: [[(EntryRow, Double)]] -> IO ()
printReviewGroups groups =
  mapM_ (uncurry printGroup) (zip [1 :: Int ..] groups)

printGroup :: Int -> [(EntryRow, Double)] -> IO ()
printGroup groupNum members = do
  let minDist = minimum [d | (_, d) <- members]
  putStrLn $ "\nGroup " ++ show groupNum ++ " (distance: " ++ showDist minDist ++ "):"
  mapM_ printGroupMember members

printGroupMember :: (EntryRow, Double) -> IO ()
printGroupMember (row, _) = do
  let fpath = fromMaybe (erFileId row) (erPath row)
  putStrLn $
    "  "
      ++ T.unpack fpath
      ++ ":"
      ++ show (erByteOffset row)
      ++ " "
      ++ maybe "" (\kw -> T.unpack kw ++ " ") (erKeywordValue row)
      ++ T.unpack (erTitle row)

printReviewGroupsJson :: [[(EntryRow, Double)]] -> IO ()
printReviewGroupsJson groups =
  mapM_ (uncurry printGroupJson) (zip [1 :: Int ..] groups)

printGroupJson :: Int -> [(EntryRow, Double)] -> IO ()
printGroupJson groupNum members = do
  let minDist = minimum [d | (_, d) <- members]
  TIO.putStr $
    "{\"group\":"
      <> T.pack (show groupNum)
      <> ",\"min_distance\":"
      <> T.pack (show minDist)
      <> ",\"entries\":["
  mapM_
    ( \(idx, (row, dist)) -> do
        when (idx > 0) $ TIO.putStr ","
        let fpath = fromMaybe (erFileId row) (erPath row)
        TIO.putStr $
          "{\"id\":\""
            <> erId row
            <> "\",\"file\":\""
            <> jsonEscape fpath
            <> "\",\"title\":\""
            <> jsonEscape (erTitle row)
            <> "\",\"keyword\":"
            <> maybe "null" (\k -> "\"" <> k <> "\"") (erKeywordValue row)
            <> ",\"distance\":"
            <> T.pack (show dist)
            <> "}"
    )
    (zip [0 :: Int ..] members)
  TIO.putStrLn "]}"

printReviewGroupsCsv :: [[(EntryRow, Double)]] -> IO ()
printReviewGroupsCsv groups = do
  putStrLn "group,file,id,keyword,title,distance"
  mapM_
    ( \(i, grp) ->
        mapM_
          ( \(row, dist) -> do
              let fpath = fromMaybe (erFileId row) (erPath row)
              TIO.putStrLn $
                T.pack (show i)
                  <> ","
                  <> csvEscape fpath
                  <> ","
                  <> erId row
                  <> ","
                  <> fromMaybe "" (erKeywordValue row)
                  <> ","
                  <> csvEscape (erTitle row)
                  <> ","
                  <> T.pack (show dist)
          )
          grp
    )
    (zip [1 :: Int ..] groups)

showDist :: Double -> String
showDist d = show (fromIntegral (round (d * 1000) :: Int) / 1000 :: Double)

-- | Read embedding dimensions from db_settings or fail with guidance.
requireEmbeddingDimensions :: DBHandle -> IO Int
requireEmbeddingDimensions db = do
  dims <- getEmbeddingDimensions db
  case dims of
    Just n -> pure n
    Nothing -> fail "Database not initialized. Run: org db init --dimensions <N>"
