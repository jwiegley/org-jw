{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Org.DB.Store (
  storeCollection,
  storeOrgFile,
  deleteFile,
  queryFiles,
  queryFileByPath,
  queryFilePath,
  queryEntries,
  queryEntriesByFile,
  queryEntriesByKeyword,
  queryEntriesByTag,
  queryEntryProperties,
  queryEntryTags,
  queryEntryStamps,

  -- * Vector embedding operations
  querySimilar,
) where

import Control.Lens hiding ((<.>))
import Control.Monad (foldM, zipWithM_)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Numeric (showHex)
import Org.DB.Types
import Org.Data
import Org.Types
import System.Directory (getModificationTime)
import System.FilePath (takeFileName)

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

{- | Store an entire collection into the database.
Each file is stored in its own transaction for atomicity.
-}
storeCollection :: DBHandle -> Collection -> IO ()
storeCollection db coll =
  mapM_ (storeOrgFile db) (coll ^.. items . traverse . _OrgItem)

{- | Store a single OrgFile, skipping if unchanged since last store.
A file is skipped when its filesystem mtime is not newer than the
recorded mtime, or when the mtime is newer but the content hash
has not changed (in which case only the mtime is updated).
When the file has changed, entries are compared individually by hash
so that unchanged entries (and their embeddings) are preserved.
-}
storeOrgFile :: DBHandle -> OrgFile -> IO ()
storeOrgFile db org = do
  let path = org ^. orgFilePath
      pathText = T.pack path
  mtime <- getModificationTime path
  existing <- queryFileByPath db pathText
  case existing of
    Just row
      | Just mt <- frModTime row, mt >= mtime -> pure ()
      | otherwise -> do
          fileHash <- hashFileText path
          if frHash row == Just fileHash
            then
              dbExecute_ db "UPDATE files SET mod_time = ?, updated_at = now() WHERE id = ?" [SqlUTCTime mtime, SqlText (frId row)]
            else
              dbTransaction db $ updateOrgFile db org (frId row) pathText mtime fileHash
    Nothing -> do
      fileHash <- hashFileText path
      dbTransaction db $ insertNewFile db org pathText mtime fileHash

-- | Insert a brand new file (no previous version exists).
insertNewFile :: DBHandle -> OrgFile -> Text -> UTCTime -> Text -> IO ()
insertNewFile db org pathText mtime fileHash = do
  fileId <- toText <$> nextRandom
  insertFileRow db fileId org pathText mtime fileHash
  insertFileProps db fileId org
  let fileCat = fileCategory org
  zipWithM_
    (insertEntry db fileId Nothing fileCat)
    [0 ..]
    (org ^. orgFileEntries)

{- | Update an existing file, preserving unchanged entries.
Entries with a stable :ID: property are matched by ID; their content
hash is compared so unchanged entries (and their embeddings) are kept.
Entries without :ID: are always re-inserted.
-}
updateOrgFile :: DBHandle -> OrgFile -> Text -> Text -> UTCTime -> Text -> IO ()
updateOrgFile db org fileId _pathText mtime fileHash = do
  -- Update file metadata
  updateFileRow db fileId org mtime fileHash
  -- Replace file properties (always, since they're few)
  dbExecute_ db "DELETE FROM file_properties WHERE file_id = ?" [SqlText fileId]
  insertFileProps db fileId org
  -- Build map of existing entries: id -> hash
  existingEntries <- queryEntriesByFile db fileId
  let existingMap = Map.fromList [(erId row, erHash row) | row <- existingEntries]
  -- Process entries incrementally, collecting all live entry IDs
  let fileCat = fileCategory org
  liveIds <-
    processEntriesIncremental db fileId Nothing fileCat existingMap [0 ..] (org ^. orgFileEntries)
  -- Delete stale entries (no longer in the file)
  let staleIds = Map.keysSet existingMap `Set.difference` liveIds
  mapM_ (\eid -> dbExecute_ db "DELETE FROM entries WHERE id = ?" [SqlText eid]) staleIds

-- | Process entries incrementally, returning the set of all live entry IDs.
processEntriesIncremental ::
  DBHandle ->
  Text ->
  Maybe Text ->
  Text ->
  Map.Map Text (Maybe Text) ->
  [Int] ->
  [Entry] ->
  IO (Set.Set Text)
processEntriesIncremental db fileId parentId fileCat existingMap positions entries =
  foldM
    ( \acc (position, entry) -> do
        entryId <- resolveEntryId entry
        let contentHash = computeEntryHash entry
        case Map.lookup entryId existingMap of
          Just (Just h) | h == contentHash -> do
            -- Entry unchanged — keep it, but update position/parent if needed
            dbExecute_
              db
              "UPDATE entries SET position = ?, parent_id = ? WHERE id = ?"
              [ SqlInt (fromIntegral position)
              , maybe SqlNull SqlText parentId
              , SqlText entryId
              ]
            -- Recurse into children
            childIds <-
              processEntriesIncremental
                db
                fileId
                (Just entryId)
                fileCat
                existingMap
                [0 ..]
                (entry ^. entryItems)
            pure (Set.insert entryId acc <> childIds)
          Just _ -> do
            -- Entry exists but changed — delete children, update, re-insert children
            deleteEntryChildren db entryId
            updateEntryRow db entryId fileId parentId position entry contentHash
            insertEntryChildren db entryId entry fileCat
            -- Null out embedding_hash so db embed will re-embed
            dbExecute_ db "UPDATE entries SET embedding_hash = NULL WHERE id = ?" [SqlText entryId]
            -- Recurse into children (all new since we can't match sub-entries)
            childIds <-
              processEntriesIncremental
                db
                fileId
                (Just entryId)
                fileCat
                existingMap
                [0 ..]
                (entry ^. entryItems)
            pure (Set.insert entryId acc <> childIds)
          Nothing -> do
            -- New entry — insert everything
            insertEntryFull db entryId fileId parentId fileCat position entry contentHash
            -- Recurse into children
            childIds <-
              processEntriesIncremental
                db
                fileId
                (Just entryId)
                fileCat
                existingMap
                [0 ..]
                (entry ^. entryItems)
            pure (Set.insert entryId acc <> childIds)
    )
    Set.empty
    (zip positions entries)

{- | Resolve the stable ID for an entry.
Uses the :ID: property when present; generates a random UUID otherwise.
-}
resolveEntryId :: Entry -> IO Text
resolveEntryId entry =
  case entry ^? entryId of
    Just eid -> pure (T.pack eid)
    Nothing -> toText <$> nextRandom

{- | Compute a content hash for an entry (excluding children).
Used to detect whether an entry's own content has changed.
-}
computeEntryHash :: Entry -> Text
computeEntryHash entry =
  bytesToHex . MD5.hash . TE.encodeUtf8 $
    T.intercalate
      "\0"
      [ T.pack (entry ^. entryHeadline)
      , maybe "" (\kw -> kwTypeText kw <> ":" <> kwText kw) (entry ^. entryKeyword)
      , maybe "" T.pack (entry ^. entryPriority)
      , T.intercalate "," [T.pack t | PlainTag t <- entry ^. entryTags]
      , T.intercalate "\n" [T.pack (p ^. name) <> "=" <> T.pack (p ^. value) | p <- entry ^. entryProperties]
      , T.pack (show (entry ^. entryBody))
      , T.pack (show (entry ^. entryStamps))
      , T.pack (show (entry ^. entryLogEntries))
      ]

-- | Insert the file row and properties for a new file.
insertFileRow :: DBHandle -> Text -> OrgFile -> Text -> UTCTime -> Text -> IO ()
insertFileRow db fileId org pathText mtime fileHash = do
  let hdr = org ^. orgFileHeader
      title = listToMaybe [T.pack (prop ^. value) | prop <- hdr ^. headerFileProperties, prop ^. name == "TITLE"]
      preamble = bodyTextMaybe (hdr ^. headerPreamble)
  dbExecute_
    db
    insertFileSQL
    [ SqlText fileId
    , SqlText pathText
    , maybe SqlNull SqlText title
    , maybe SqlNull SqlText preamble
    , SqlText fileHash
    , SqlUTCTime mtime
    ]

-- | Update an existing file row's metadata.
updateFileRow :: DBHandle -> Text -> OrgFile -> UTCTime -> Text -> IO ()
updateFileRow db fileId org mtime fileHash = do
  let hdr = org ^. orgFileHeader
      title = listToMaybe [T.pack (prop ^. value) | prop <- hdr ^. headerFileProperties, prop ^. name == "TITLE"]
      preamble = bodyTextMaybe (hdr ^. headerPreamble)
  dbExecute_
    db
    "UPDATE files SET title = ?, preamble = ?, hash = ?, mod_time = ?, updated_at = now() WHERE id = ?"
    [ maybe SqlNull SqlText title
    , maybe SqlNull SqlText preamble
    , SqlText fileHash
    , SqlUTCTime mtime
    , SqlText fileId
    ]

-- | Insert file-level properties.
insertFileProps :: DBHandle -> Text -> OrgFile -> IO ()
insertFileProps db fileId org = do
  let hdr = org ^. orgFileHeader
  zipWithM_ (insertFileProperty db fileId "drawer") [0 ..] (hdr ^. headerPropertiesDrawer)
  zipWithM_ (insertFileProperty db fileId "file") [0 ..] (hdr ^. headerFileProperties)

-- | Compute the file-level category.
fileCategory :: OrgFile -> Text
fileCategory org =
  fromMaybe
    (T.pack (takeFileName (org ^. orgFilePath)))
    ( listToMaybe
        [ T.pack (prop ^. value)
        | prop <- org ^. orgFileHeader . headerFileProperties
        , prop ^. name == "CATEGORY"
        ]
    )

{- | Delete all child data for an entry (tags, properties, stamps, etc.)
but keep the entry row itself.
-}
deleteEntryChildren :: DBHandle -> Text -> IO ()
deleteEntryChildren db entryId = do
  dbExecute_ db "DELETE FROM entry_tags WHERE entry_id = ?" [SqlText entryId]
  dbExecute_ db "DELETE FROM entry_properties WHERE entry_id = ?" [SqlText entryId]
  dbExecute_ db "DELETE FROM entry_stamps WHERE entry_id = ?" [SqlText entryId]
  dbExecute_ db "DELETE FROM entry_log_entries WHERE entry_id = ?" [SqlText entryId]
  dbExecute_ db "DELETE FROM entry_body_blocks WHERE entry_id = ?" [SqlText entryId]
  dbExecute_ db "DELETE FROM entry_categories WHERE entry_id = ?" [SqlText entryId]
  dbExecute_ db "DELETE FROM entry_links WHERE entry_id = ?" [SqlText entryId]

-- | Update an existing entry row with new content.
updateEntryRow :: DBHandle -> Text -> Text -> Maybe Text -> Int -> Entry -> Text -> IO ()
updateEntryRow db entryId fileId parentId position entry contentHash = do
  let (kwType, kwValue) = keywordInfo (entry ^. entryKeyword)
  dbExecute_
    db
    "UPDATE entries SET file_id = ?, parent_id = ?, depth = ?, position = ?, \
    \byte_offset = ?, keyword_type = ?, keyword_value = ?, priority = ?, \
    \headline = ?, title = ?, verb = ?, context = ?, locator = ?, \
    \hash = ?, updated_at = now() WHERE id = ?"
    [ SqlText fileId
    , maybe SqlNull SqlText parentId
    , SqlInt (fromIntegral (entry ^. entryDepth))
    , SqlInt (fromIntegral position)
    , SqlInt (fromIntegral (entry ^. entryLoc . pos))
    , maybe SqlNull SqlText kwType
    , maybe SqlNull SqlText kwValue
    , maybe SqlNull (SqlText . T.pack) (entry ^. entryPriority)
    , SqlText (T.pack (entry ^. entryHeadline))
    , SqlText (T.pack (entry ^. entryTitle))
    , maybe SqlNull (SqlText . T.pack) (entry ^. entryVerb)
    , maybe SqlNull (SqlText . T.pack) (entry ^. entryContext)
    , maybe SqlNull (SqlText . T.pack) (entry ^. entryLocator)
    , SqlText contentHash
    , SqlText entryId
    ]

-- | Insert all child data for an entry (tags, properties, stamps, etc.).
insertEntryChildren :: DBHandle -> Text -> Entry -> Text -> IO ()
insertEntryChildren db entryId entry fileCat = do
  insertTagsBatch db entryId (entry ^. entryTags)
  insertPropertiesBatch db entryId (entry ^. entryProperties)
  mapM_ (insertStamp db entryId) (entry ^. entryStamps)
  zipWithM_ (insertLogEntry db entryId Nothing) [0 ..] (entry ^. entryLogEntries)
  insertBody db entryId (entry ^. entryBody)
  insertExtractedLinks db entryId entry
  let entryCat = listToMaybe [T.pack (prop ^. value) | prop <- entry ^. entryProperties, prop ^. name == "CATEGORY"]
  case entryCat of
    Just cat -> dbExecute_ db insertCategorySQL [SqlText entryId, SqlText cat, SqlBool True, SqlNull]
    Nothing -> dbExecute_ db insertCategorySQL [SqlText entryId, SqlText fileCat, SqlBool False, SqlNull]

-- | Insert a complete new entry with all its children.
insertEntryFull :: DBHandle -> Text -> Text -> Maybe Text -> Text -> Int -> Entry -> Text -> IO ()
insertEntryFull db entryId fileId parentId fileCat position entry contentHash = do
  let (kwType, kwValue) = keywordInfo (entry ^. entryKeyword)
  dbExecute_
    db
    insertEntryWithHashSQL
    [ SqlText entryId
    , SqlText fileId
    , maybe SqlNull SqlText parentId
    , SqlInt (fromIntegral (entry ^. entryDepth))
    , SqlInt (fromIntegral position)
    , SqlInt (fromIntegral (entry ^. entryLoc . pos))
    , maybe SqlNull SqlText kwType
    , maybe SqlNull SqlText kwValue
    , maybe SqlNull (SqlText . T.pack) (entry ^. entryPriority)
    , SqlText (T.pack (entry ^. entryHeadline))
    , SqlText (T.pack (entry ^. entryTitle))
    , maybe SqlNull (SqlText . T.pack) (entry ^. entryVerb)
    , maybe SqlNull (SqlText . T.pack) (entry ^. entryContext)
    , maybe SqlNull (SqlText . T.pack) (entry ^. entryLocator)
    , SqlText contentHash
    ]
  insertEntryChildren db entryId entry fileCat

-- | Delete a file and all its associated data (cascading).
deleteFile :: DBHandle -> Text -> IO ()
deleteFile db path =
  dbExecute_ db "DELETE FROM files WHERE path = ?" [SqlText path]

------------------------------------------------------------------------
-- Queries
------------------------------------------------------------------------

-- | Query all files in the database.
queryFiles :: DBHandle -> IO [FileRow]
queryFiles db =
  dbQuery
    db
    "SELECT id, path, title, preamble, hash, mod_time, \
    \created_time, created_at, updated_at FROM files"
    []

-- | Query a single file by path.
queryFileByPath :: DBHandle -> Text -> IO (Maybe FileRow)
queryFileByPath db path =
  dbQueryOne
    db
    "SELECT id, path, title, preamble, hash, mod_time, \
    \created_time, created_at, updated_at FROM files WHERE path = ?"
    [SqlText path]

-- | Look up a file's path by its ID.
queryFilePath :: DBHandle -> Text -> IO (Maybe Text)
queryFilePath db fileId = do
  rows <- dbQuery db "SELECT path FROM files WHERE id = ?" [SqlText fileId] :: IO [[SqlValue]]
  case rows of
    ([SqlText p] : _) -> pure (Just p)
    _ -> pure Nothing

-- | Query all entries in the database.
queryEntries :: DBHandle -> IO [EntryRow]
queryEntries db =
  dbQuery db entryColumnsSQL []

-- | Query entries by keyword (e.g., "TODO", "DONE").
queryEntriesByKeyword :: DBHandle -> Text -> IO [EntryRow]
queryEntriesByKeyword db kw =
  dbQuery
    db
    (entryColumnsSQL <> " WHERE keyword_value = ?")
    [SqlText kw]

-- | Query entries for a specific file.
queryEntriesByFile :: DBHandle -> Text -> IO [EntryRow]
queryEntriesByFile db fileId =
  dbQuery
    db
    (entryColumnsSQL <> " WHERE file_id = ?")
    [SqlText fileId]

-- | Query entries that have a specific tag.
queryEntriesByTag :: DBHandle -> Text -> IO [EntryRow]
queryEntriesByTag db tag =
  dbQuery
    db
    "SELECT e.id, e.file_id, e.parent_id, e.depth, e.position, \
    \e.byte_offset, e.keyword_type, e.keyword_value, e.priority, \
    \e.headline, e.title, e.verb, e.context, e.locator, \
    \e.hash, e.mod_time, e.created_time, e.path::text \
    \FROM entries e JOIN entry_tags t ON e.id = t.entry_id \
    \WHERE t.tag = ?"
    [SqlText tag]

-- | Query properties for an entry.
queryEntryProperties :: DBHandle -> Text -> IO [EntryPropertyRow]
queryEntryProperties db entryId =
  dbQuery
    db
    "SELECT entry_id, name, value, is_inherited, byte_offset \
    \FROM entry_properties WHERE entry_id = ?"
    [SqlText entryId]

-- | Query tags for an entry.
queryEntryTags :: DBHandle -> Text -> IO [EntryTagRow]
queryEntryTags db entryId =
  dbQuery
    db
    "SELECT entry_id, tag, is_inherited, source_id \
    \FROM entry_tags WHERE entry_id = ?"
    [SqlText entryId]

-- | Query stamps for an entry.
queryEntryStamps :: DBHandle -> Text -> IO [EntryStampRow]
queryEntryStamps db entryId =
  dbQuery
    db
    "SELECT id, entry_id, byte_offset, stamp_type, time_kind, day, \
    \day_end, time_start, time_end, suffix_kind, suffix_num, \
    \suffix_span, suffix_larger_num, suffix_larger_span \
    \FROM entry_stamps WHERE entry_id = ?"
    [SqlText entryId]

-- | Common SELECT columns for entries (18 columns matching EntryRow).
entryColumnsSQL :: Text
entryColumnsSQL =
  "SELECT id, file_id, parent_id, depth, position, byte_offset, \
  \keyword_type, keyword_value, priority, headline, title, verb, \
  \context, locator, hash, mod_time, created_time, path::text \
  \FROM entries"

------------------------------------------------------------------------
-- Vector embeddings
------------------------------------------------------------------------

{- | Find entries similar to a given embedding vector, ordered by cosine distance.
Searches the entry_embeddings table and returns distinct parent entries,
ranked by the best-matching chunk per entry. Includes the matching chunk text,
its source path within the entry, and the cosine distance (lower = more similar).
-}
querySimilar :: DBHandle -> [Double] -> Int -> IO [(EntryRow, Text, Text, Double)]
querySimilar db vec limit = do
  let vecText = "[" <> T.intercalate "," (map (T.pack . show) vec) <> "]"
  rows <-
    dbQuery
      db
      "WITH best_chunks AS (\
      \  SELECT DISTINCT ON (entry_id) entry_id, chunk_source, chunk_text, \
      \    embedding <=> ?::vector AS distance \
      \  FROM entry_embeddings \
      \  WHERE embedding IS NOT NULL \
      \  ORDER BY entry_id, embedding <=> ?::vector\
      \) \
      \SELECT e.id, e.file_id, e.parent_id, e.depth, \
      \e.position, e.byte_offset, e.keyword_type, e.keyword_value, \
      \e.priority, e.headline, e.title, e.verb, e.context, e.locator, \
      \e.hash, e.mod_time, e.created_time, e.path::text, \
      \bc.chunk_source, bc.chunk_text, bc.distance \
      \FROM entries e \
      \JOIN best_chunks bc ON bc.entry_id = e.id \
      \ORDER BY bc.distance \
      \LIMIT ?"
      [SqlText vecText, SqlText vecText, SqlInt (fromIntegral limit)] ::
      IO [[SqlValue]]
  pure [quad | row <- rows, Just quad <- [parseSearchRow row]]

parseSearchRow :: [SqlValue] -> Maybe (EntryRow, Text, Text, Double)
parseSearchRow vals
  | length vals == 21 =
      let entryVals = take 18 vals
          srcVal = vals !! 18
          chunkVal = vals !! 19
          distVal = vals !! 20
       in case (fromRow entryVals, extractText srcVal, extractText chunkVal, extractDouble distVal) of
            (Right entry, Right src, Right chunk, Right dist) -> Just (entry, src, chunk, dist)
            _ -> Nothing
  | otherwise = Nothing

------------------------------------------------------------------------
-- Internal: Insert helpers
------------------------------------------------------------------------

insertFileProperty :: DBHandle -> Text -> Text -> Int -> Property -> IO ()
insertFileProperty db fileId source pos prop =
  dbExecute_
    db
    insertFilePropertySQL
    [ SqlText fileId
    , SqlInt (fromIntegral pos)
    , SqlText (T.pack (prop ^. name))
    , SqlText (T.pack (prop ^. value))
    , SqlText source
    ]

-- | Insert a new entry and all its children (for initial file insert).
insertEntry :: DBHandle -> Text -> Maybe Text -> Text -> Int -> Entry -> IO ()
insertEntry db fileId parentId fileCat position entry = do
  entryId <- resolveEntryId entry
  let contentHash = computeEntryHash entry
  insertEntryFull db entryId fileId parentId fileCat position entry contentHash
  -- Sub-entries with position tracking
  zipWithM_
    (insertEntry db fileId (Just entryId) fileCat)
    [0 ..]
    (entry ^. entryItems)

-- | Batch insert tags using multi-row VALUES for fewer round-trips.
insertTagsBatch :: DBHandle -> Text -> [Tag] -> IO ()
insertTagsBatch _ _ [] = pure ()
insertTagsBatch db entryId tags =
  let rows = map tagParams tags
      tagParams (PlainTag tag) = [SqlText entryId, SqlText (T.pack tag), SqlBool False, SqlNull]
      vals = T.intercalate ", " (replicate (length rows) "(?, ?, ?, ?)")
      sql = "INSERT INTO entry_tags (entry_id, tag, is_inherited, source_id) VALUES " <> vals
   in dbExecute_ db sql (concat rows)

-- | Batch insert properties using multi-row VALUES for fewer round-trips.
insertPropertiesBatch :: DBHandle -> Text -> [Property] -> IO ()
insertPropertiesBatch _ _ [] = pure ()
insertPropertiesBatch db entryId props =
  let rows = map propParams props
      propParams prop =
        [ SqlText entryId
        , SqlText (T.pack (prop ^. name))
        , SqlText (T.pack (prop ^. value))
        , SqlBool (prop ^. inherited)
        , SqlInt (fromIntegral (prop ^. propertyLoc . pos))
        ]
      vals = T.intercalate ", " (replicate (length rows) "(?, ?, ?, ?, ?)")
      sql = "INSERT INTO entry_properties (entry_id, name, value, is_inherited, byte_offset) VALUES " <> vals
   in dbExecute_ db sql (concat rows)

insertStamp :: DBHandle -> Text -> Stamp -> IO ()
insertStamp db entryId stamp = do
  let (stampType, time) = stampInfo stamp
      stampLoc = stamp ^. _stampLoc
  dbExecute_ db insertStampSQL $
    [ SqlText entryId
    , SqlInt (fromIntegral (stampLoc ^. pos))
    , SqlText stampType
    , SqlText (timeKindText (time ^. timeKind))
    , SqlInt (fromIntegral (time ^. timeDay))
    , maybe SqlNull (SqlInt . fromIntegral) (time ^. timeDayEnd)
    , maybe SqlNull (SqlInt . fromIntegral) (time ^. timeStart)
    , maybe SqlNull (SqlInt . fromIntegral) (time ^. timeEnd)
    ]
      ++ timeSuffixParams (time ^. timeSuffix)

insertLogEntry :: DBHandle -> Text -> Maybe Int64 -> Int -> LogEntry -> IO ()
insertLogEntry db entryId mLogbookId position le = case le of
  LogClosing loc t mb ->
    execLog "closing" (loc ^. pos) (Just t) Nothing Nothing Nothing Nothing mb
  LogState loc toKw mFromKw t mb ->
    -- toKw (pos 2) is the keyword we transitioned TO
    -- mFromKw (pos 3) is the keyword we transitioned FROM
    execLog "state" (loc ^. pos) (Just t) mFromKw (Just toKw) Nothing Nothing mb
  LogNote loc t mb ->
    execLog "note" (loc ^. pos) (Just t) Nothing Nothing Nothing Nothing mb
  LogRescheduled loc t oldT mb ->
    execLog "rescheduled" (loc ^. pos) (Just t) Nothing Nothing (Just oldT) Nothing mb
  LogNotScheduled loc t oldT mb ->
    execLog "not_scheduled" (loc ^. pos) (Just t) Nothing Nothing (Just oldT) Nothing mb
  LogDeadline loc t oldT mb ->
    execLog "deadline" (loc ^. pos) (Just t) Nothing Nothing (Just oldT) Nothing mb
  LogNoDeadline loc t oldT mb ->
    execLog "no_deadline" (loc ^. pos) (Just t) Nothing Nothing (Just oldT) Nothing mb
  LogRefiling loc t mb ->
    execLog "refiling" (loc ^. pos) (Just t) Nothing Nothing Nothing Nothing mb
  LogClock loc t mDur ->
    execLog "clock" (loc ^. pos) (Just t) Nothing Nothing Nothing mDur Nothing
  LogBook loc entries -> do
    let params =
          [ SqlText entryId
          , SqlInt (fromIntegral position)
          , SqlInt (fromIntegral (loc ^. pos))
          , SqlText "logbook"
          ]
            ++ replicate 4 SqlNull -- time
            ++ replicate 4 SqlNull -- keywords
            ++ replicate 8 SqlNull -- orig time + suffix
            ++ replicate 2 SqlNull -- duration
            ++ [maybe SqlNull (SqlInt . fromIntegral) mLogbookId]
    rows <- dbQuery db insertLogEntryReturningSQL params :: IO [[SqlValue]]
    case rows of
      ([SqlInt newId] : _) ->
        zipWithM_
          (insertLogEntry db entryId (Just newId))
          [0 ..]
          entries
      _ -> pure ()
 where
  execLog logType byteOff mTime mFromKw mToKw mOrigTime mDur mBody = do
    let params =
          [ SqlText entryId
          , SqlInt (fromIntegral position)
          , SqlInt (fromIntegral byteOff)
          , SqlText logType
          ]
            ++ timeMjdParams mTime
            ++ keywordParams mFromKw mToKw
            ++ origTimeParams mOrigTime
            ++ durationParams mDur
            ++ [maybe SqlNull (SqlInt . fromIntegral) mLogbookId]
    case mBody of
      Just body | not (null (body ^. blocks)) -> do
        rows <- dbQuery db insertLogEntryReturningSQL params :: IO [[SqlValue]]
        case rows of
          ([SqlInt logId] : _) ->
            insertLogEntryBody db logId body
          _ -> pure ()
      _ -> dbExecute_ db insertLogEntrySQL params

insertExtractedLinks :: DBHandle -> Text -> Entry -> IO ()
insertExtractedLinks db entryId entry = do
  let allText =
        [entry ^. entryHeadline, entry ^. entryTitle]
          ++ map (^. value) (entry ^. entryProperties)
          ++ bodyStrings (entry ^. entryBody)
          ++ logBodyStrings (entry ^. entryLogEntries)
      links = concatMap extractOrgLinks allText
  zipWithM_ (insertLink db entryId) [0 ..] links

insertLink :: DBHandle -> Text -> Int -> (Text, Text, Maybe Text) -> IO ()
insertLink db entryId pos (linkType, target, desc) =
  dbExecute_
    db
    insertLinkSQL
    [ SqlText entryId
    , SqlText linkType
    , SqlText target
    , maybe SqlNull SqlText desc
    , SqlInt (fromIntegral pos)
    ]

bodyStrings :: Body -> [String]
bodyStrings body = concatMap blockStrings (body ^. blocks)
 where
  blockStrings (Whitespace _ _) = []
  blockStrings (Paragraph _ ls) = ls
  blockStrings (Drawer _ _ ls) = ls
  blockStrings (InlineTask _ _) = []

logBodyStrings :: [LogEntry] -> [String]
logBodyStrings = concatMap go
 where
  go (LogClosing _ _ mb) = maybe [] bodyStrings mb
  go (LogState _ _ _ _ mb) = maybe [] bodyStrings mb
  go (LogNote _ _ mb) = maybe [] bodyStrings mb
  go (LogRescheduled _ _ _ mb) = maybe [] bodyStrings mb
  go (LogNotScheduled _ _ _ mb) = maybe [] bodyStrings mb
  go (LogDeadline _ _ _ mb) = maybe [] bodyStrings mb
  go (LogNoDeadline _ _ _ mb) = maybe [] bodyStrings mb
  go (LogRefiling _ _ mb) = maybe [] bodyStrings mb
  go (LogClock _ _ _) = []
  go (LogBook _ entries) = logBodyStrings entries

{- | Extract org-mode links from a string. Returns (linkType, target, description).
Recognizes [[URL]] and [[URL][DESC]] patterns.
For id:UUID links, linkType is "id" and target is the UUID.
For other links, linkType is the scheme (e.g. "file", "http") or "unknown".
-}
extractOrgLinks :: String -> [(Text, Text, Maybe Text)]
extractOrgLinks [] = []
extractOrgLinks ('[' : '[' : rest) = case parseLink rest of
  Just (url, desc, remaining) ->
    classifyLink url desc : extractOrgLinks remaining
  Nothing -> extractOrgLinks rest
extractOrgLinks (_ : rest) = extractOrgLinks rest

parseLink :: String -> Maybe (String, Maybe String, String)
parseLink s = case break (== ']') s of
  (_, []) -> Nothing
  (url, ']' : ']' : remaining) -> Just (url, Nothing, remaining)
  (url, ']' : '[' : rest2) -> case break (== ']') rest2 of
    (desc, ']' : ']' : remaining) -> Just (url, Just desc, remaining)
    _ -> Nothing
  _ -> Nothing

classifyLink :: String -> Maybe String -> (Text, Text, Maybe Text)
classifyLink url desc
  | "id:" `isPrefixOf` url =
      ("id", T.pack (drop 3 url), T.pack <$> desc)
  | otherwise =
      let (scheme, rest) = break (== ':') url
       in if null rest
            then ("unknown", T.pack url, T.pack <$> desc)
            else (T.pack scheme, T.pack url, T.pack <$> desc)

insertBody :: DBHandle -> Text -> Body -> IO ()
insertBody db entryId body =
  mapM_ insertBlock (zip [0 :: Int ..] (body ^. blocks))
 where
  insertBlock (pos, block) = do
    let (blockType, content, byteOff, dType, dName) = blockInfo block
    dbExecute_
      db
      insertBodyBlockSQL
      [ SqlText entryId
      , SqlInt (fromIntegral pos)
      , SqlInt (fromIntegral byteOff)
      , SqlText blockType
      , maybe SqlNull SqlText content
      , maybe SqlNull SqlText dType
      , maybe SqlNull SqlText dName
      ]

insertLogEntryBody :: DBHandle -> Int64 -> Body -> IO ()
insertLogEntryBody db logEntryId body =
  mapM_ insertBlock (zip [0 :: Int ..] (body ^. blocks))
 where
  insertBlock (pos, block) = do
    let (blockType, content, byteOff, dType, dName) = blockInfo block
    dbExecute_
      db
      insertLogBodyBlockSQL
      [ SqlInt (fromIntegral logEntryId)
      , SqlInt (fromIntegral pos)
      , SqlInt (fromIntegral byteOff)
      , SqlText blockType
      , maybe SqlNull SqlText content
      , maybe SqlNull SqlText dType
      , maybe SqlNull SqlText dName
      ]

------------------------------------------------------------------------
-- SQL statements
------------------------------------------------------------------------

insertFileSQL :: Text
insertFileSQL =
  "INSERT INTO files (id, path, title, preamble, hash, mod_time) \
  \VALUES (?, ?, ?, ?, ?, ?)"

insertFilePropertySQL :: Text
insertFilePropertySQL =
  "INSERT INTO file_properties (file_id, position, name, value, source) \
  \VALUES (?, ?, ?, ?, ?)"

insertEntryWithHashSQL :: Text
insertEntryWithHashSQL =
  "INSERT INTO entries (id, file_id, parent_id, depth, position, \
  \byte_offset, keyword_type, keyword_value, priority, headline, \
  \title, verb, context, locator, hash) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertStampSQL :: Text
insertStampSQL =
  "INSERT INTO entry_stamps (entry_id, byte_offset, stamp_type, \
  \time_kind, day, day_end, time_start, time_end, suffix_kind, \
  \suffix_num, suffix_span, suffix_larger_num, suffix_larger_span) \
  \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertLogEntrySQL :: Text
insertLogEntrySQL =
  "INSERT INTO entry_log_entries (entry_id, position, byte_offset, \
  \log_type, time_day, time_start, time_end, time_kind, from_keyword, \
  \from_keyword_type, to_keyword, to_keyword_type, orig_time_day, \
  \orig_time_day_end, orig_time_start, orig_time_end, orig_time_kind, \
  \orig_suffix_kind, orig_suffix_num, orig_suffix_span, \
  \duration_hours, duration_mins, logbook_id) \
  \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertLogEntryReturningSQL :: Text
insertLogEntryReturningSQL = insertLogEntrySQL <> " RETURNING id"

insertBodyBlockSQL :: Text
insertBodyBlockSQL =
  "INSERT INTO entry_body_blocks (entry_id, position, byte_offset, \
  \block_type, content, drawer_type, drawer_name) \
  \VALUES (?, ?, ?, ?, ?, ?, ?)"

insertLogBodyBlockSQL :: Text
insertLogBodyBlockSQL =
  "INSERT INTO log_entry_body_blocks (log_entry_id, position, byte_offset, \
  \block_type, content, drawer_type, drawer_name) \
  \VALUES (?, ?, ?, ?, ?, ?, ?)"

insertCategorySQL :: Text
insertCategorySQL =
  "INSERT INTO entry_categories (entry_id, category, is_explicit, \
  \source_id) VALUES (?, ?, ?, ?)"

insertLinkSQL :: Text
insertLinkSQL =
  "INSERT INTO entry_links (entry_id, link_type, target, description, \
  \position) VALUES (?, ?, ?, ?, ?)"

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

keywordInfo :: Maybe Keyword -> (Maybe Text, Maybe Text)
keywordInfo Nothing = (Nothing, Nothing)
keywordInfo (Just (OpenKeyword _ s)) = (Just "open", Just (T.pack s))
keywordInfo (Just (ClosedKeyword _ s)) = (Just "closed", Just (T.pack s))

kwText :: Keyword -> Text
kwText (OpenKeyword _ s) = T.pack s
kwText (ClosedKeyword _ s) = T.pack s

kwTypeText :: Keyword -> Text
kwTypeText (OpenKeyword _ _) = "open"
kwTypeText (ClosedKeyword _ _) = "closed"

stampInfo :: Stamp -> (Text, Time)
stampInfo (ClosedStamp _ t) = ("closed", t)
stampInfo (ScheduledStamp _ t) = ("scheduled", t)
stampInfo (DeadlineStamp _ t) = ("deadline", t)
stampInfo (ActiveStamp _ t) = ("active", t)

_stampLoc :: (Functor f) => (Loc -> f Loc) -> Stamp -> f Stamp
_stampLoc f (ClosedStamp loc t) = (`ClosedStamp` t) <$> f loc
_stampLoc f (ScheduledStamp loc t) = (`ScheduledStamp` t) <$> f loc
_stampLoc f (DeadlineStamp loc t) = (`DeadlineStamp` t) <$> f loc
_stampLoc f (ActiveStamp loc t) = (`ActiveStamp` t) <$> f loc

timeKindText :: TimeKind -> Text
timeKindText ActiveTime = "active"
timeKindText InactiveTime = "inactive"

suffixKindText :: TimeSuffixKind -> Text
suffixKindText TimeRepeat = "repeat"
suffixKindText TimeRepeatPlus = "repeat_plus"
suffixKindText TimeDottedRepeat = "dotted_repeat"
suffixKindText TimeWithin = "within"

timeSpanText :: TimeSpan -> Text
timeSpanText DaySpan = "day"
timeSpanText WeekSpan = "week"
timeSpanText MonthSpan = "month"
timeSpanText YearSpan = "year"

drawerTypeText :: DrawerType -> Text
drawerTypeText (PlainDrawer _) = "plain"
drawerTypeText (BeginDrawer _) = "begin"

drawerName :: DrawerType -> String
drawerName (PlainDrawer s) = s
drawerName (BeginDrawer s) = s

blockInfo :: Block -> (Text, Maybe Text, Int, Maybe Text, Maybe Text)
blockInfo (Whitespace loc s) = ("whitespace", Just (T.pack s), loc ^. pos, Nothing, Nothing)
blockInfo (Paragraph loc ls) = ("paragraph", Just (T.pack (unlines ls)), loc ^. pos, Nothing, Nothing)
blockInfo (Drawer loc dt ls) =
  ( "drawer"
  , Just (T.pack (unlines ls))
  , loc ^. pos
  , Just (drawerTypeText dt)
  , Just (T.pack (drawerName dt))
  )
blockInfo (InlineTask loc _) = ("inline_task", Nothing, loc ^. pos, Nothing, Nothing)

-- | Convert a Body to Maybe Text, Nothing if empty.
bodyTextMaybe :: Body -> Maybe Text
bodyTextMaybe body = case body ^. blocks of
  [] -> Nothing
  bs ->
    let t = T.pack (concatMap blockToString bs)
     in if T.null t then Nothing else Just t
 where
  blockToString (Whitespace _ s) = s
  blockToString (Paragraph _ ls) = unlines ls
  blockToString (Drawer _ _ ls) = unlines ls
  blockToString (InlineTask _ _) = ""

-- | MJD time parameters: time_day, time_start, time_end, time_kind (4 values).
timeMjdParams :: Maybe Time -> [SqlValue]
timeMjdParams Nothing = [SqlNull, SqlNull, SqlNull, SqlNull]
timeMjdParams (Just t) =
  [ SqlInt (fromIntegral (t ^. timeDay))
  , maybe SqlNull (SqlInt . fromIntegral) (t ^. timeStart)
  , maybe SqlNull (SqlInt . fromIntegral) (t ^. timeEnd)
  , SqlText (timeKindText (t ^. timeKind))
  ]

-- | Keyword parameters: from_keyword, from_keyword_type, to_keyword, to_keyword_type (4 values).
keywordParams :: Maybe Keyword -> Maybe Keyword -> [SqlValue]
keywordParams mFrom mTo =
  [ maybe SqlNull (SqlText . kwText) mFrom
  , maybe SqlNull (SqlText . kwTypeText) mFrom
  , maybe SqlNull (SqlText . kwText) mTo
  , maybe SqlNull (SqlText . kwTypeText) mTo
  ]

-- | Original time parameters for rescheduled/deadline changes (8 values).
origTimeParams :: Maybe Time -> [SqlValue]
origTimeParams Nothing = replicate 8 SqlNull
origTimeParams (Just t) =
  [ SqlInt (fromIntegral (t ^. timeDay))
  , maybe SqlNull (SqlInt . fromIntegral) (t ^. timeDayEnd)
  , maybe SqlNull (SqlInt . fromIntegral) (t ^. timeStart)
  , maybe SqlNull (SqlInt . fromIntegral) (t ^. timeEnd)
  , SqlText (timeKindText (t ^. timeKind))
  ]
    ++ case t ^. timeSuffix of
      Nothing -> [SqlNull, SqlNull, SqlNull]
      Just s ->
        [ SqlText (suffixKindText (s ^. suffixKind))
        , SqlInt (fromIntegral (s ^. suffixNum))
        , SqlText (timeSpanText (s ^. suffixSpan))
        ]

-- | Duration parameters: duration_hours, duration_mins (2 values).
durationParams :: Maybe Duration -> [SqlValue]
durationParams Nothing = [SqlNull, SqlNull]
durationParams (Just d) =
  [ SqlInt (fromIntegral (d ^. hours))
  , SqlInt (fromIntegral (d ^. mins))
  ]

{- | TimeSuffix parameters: suffix_kind, suffix_num, suffix_span,
suffix_larger_num, suffix_larger_span (5 values).
-}
timeSuffixParams :: Maybe TimeSuffix -> [SqlValue]
timeSuffixParams Nothing = [SqlNull, SqlNull, SqlNull, SqlNull, SqlNull]
timeSuffixParams (Just s) =
  [ SqlText (suffixKindText (s ^. suffixKind))
  , SqlInt (fromIntegral (s ^. suffixNum))
  , SqlText (timeSpanText (s ^. suffixSpan))
  ]
    ++ case s ^. suffixLargerSpan of
      Nothing -> [SqlNull, SqlNull]
      Just (n, sp) -> [SqlInt (fromIntegral n), SqlText (timeSpanText sp)]

-- | Hash a file and return hex-encoded MD5.
hashFileText :: FilePath -> IO Text
hashFileText path = bytesToHex . MD5.hash <$> BS.readFile path

bytesToHex :: BS.ByteString -> Text
bytesToHex = T.pack . concatMap byte . BS.unpack
 where
  byte w = case showHex w "" of
    [c] -> ['0', c]
    cs -> cs
