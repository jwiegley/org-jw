{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.Exec where

import Control.Lens
import DB.Options
import Data.ByteString.Char8 qualified as BS8
import Data.Maybe (fromMaybe)
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
    DBInit -> withDB dbCfg $ \db -> do
      initDB db
      result <- runMigrations db
      case result of
        MigrationsApplied n ->
          putStrLn $ "Database initialized. " ++ show n ++ " migration(s) applied."
        AlreadyCurrent ->
          putStrLn "Database initialized (schema up to date)."
        MigrationFailed v msg ->
          putStrLn $ "Database initialized, but migration " ++ show v ++ " failed: " ++ T.unpack msg
    DBStore -> withDB dbCfg $ \db -> do
      initDB db
      storeCollection db coll
      putStrLn "Collection stored."
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
      initDB db
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

csvEscape :: T.Text -> T.Text
csvEscape t
  | T.any (\c -> c == ',' || c == '"' || c == '\n') t =
      "\"" <> T.replace "\"" "\"\"" t <> "\""
  | otherwise = t

jsonEscape :: T.Text -> T.Text
jsonEscape = T.replace "\\" "\\\\" . T.replace "\"" "\\\"" . T.replace "\n" "\\n"

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
