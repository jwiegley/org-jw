{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Org.DB.Store (
  storeCollection,
  storeOrgFile,
  deleteFile,
  queryFiles,
  queryEntries,
  queryEntriesByKeyword,
  queryEntriesByTag,
) where

import Control.Lens hiding ((<.>))
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Org.DB.Types
import Org.Data
import Org.Types
import System.Directory (getModificationTime)
import System.FilePath (takeFileName)

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Store an entire collection into the database.
storeCollection :: DBHandle -> Collection -> IO ()
storeCollection db coll =
  dbTransaction db $
    mapM_ (storeOrgFile db) (coll ^.. items . traverse . _OrgItem)

-- | Store a single OrgFile, replacing any previous version.
storeOrgFile :: DBHandle -> OrgFile -> IO ()
storeOrgFile db org = do
  let path = org ^. orgFilePath
  fileId <- toText <$> nextRandom
  mtime <- getModificationTime path
  fileHash <- hashFile path
  -- Delete old data for this file path
  deleteFile db (T.pack path)
  -- Insert file row
  dbExecute_ db insertFileSQL [SqlText fileId, SqlText (T.pack path), SqlUTCTime mtime, SqlBlob fileHash]
  -- Insert file-level properties from header
  mapM_ (insertFileProperty db fileId) (org ^. orgFileHeader . headerPropertiesDrawer)
  -- Insert category from file properties
  let cats =
        [ prop ^. value
        | prop <- org ^. orgFileHeader . headerFileProperties
        , prop ^. name == "CATEGORY"
        ]
  case cats of
    (c : _) -> dbExecute_ db insertCategorySQL [SqlText fileId, SqlText (T.pack c)]
    [] ->
      dbExecute_
        db
        insertCategorySQL
        [SqlText fileId, SqlText (T.pack (takeFileName path))]
  -- Insert entries recursively
  mapM_ (insertEntry db fileId Nothing (T.pack path)) (org ^. orgFileEntries)

-- | Delete a file and all its associated data (cascading).
deleteFile :: DBHandle -> Text -> IO ()
deleteFile db path =
  dbExecute_ db "DELETE FROM files WHERE path = ?" [SqlText path]

------------------------------------------------------------------------
-- Queries
------------------------------------------------------------------------

-- | Query all files in the database.
queryFiles :: DBHandle -> IO [FileRow]
queryFiles db = dbQuery db "SELECT id, path, mtime, hash FROM files" []

-- | Query all entries in the database.
queryEntries :: DBHandle -> IO [EntryRow]
queryEntries db =
  dbQuery
    db
    "SELECT entry_id, file_id, parent_id, depth, keyword, keyword_closed, \
    \priority, headline, verb, title, context, locator, file_line, path \
    \FROM entries"
    []

-- | Query entries by keyword (e.g., "TODO", "DONE").
queryEntriesByKeyword :: DBHandle -> Text -> IO [EntryRow]
queryEntriesByKeyword db kw =
  dbQuery
    db
    "SELECT entry_id, file_id, parent_id, depth, keyword, keyword_closed, \
    \priority, headline, verb, title, context, locator, file_line, path \
    \FROM entries WHERE keyword = ?"
    [SqlText kw]

-- | Query entries that have a specific tag.
queryEntriesByTag :: DBHandle -> Text -> IO [EntryRow]
queryEntriesByTag db tag =
  dbQuery
    db
    "SELECT e.entry_id, e.file_id, e.parent_id, e.depth, e.keyword, \
    \e.keyword_closed, e.priority, e.headline, e.verb, e.title, \
    \e.context, e.locator, e.file_line, e.path \
    \FROM entries e JOIN entry_tags t ON e.entry_id = t.entry_id \
    \WHERE t.tag = ?"
    [SqlText tag]

------------------------------------------------------------------------
-- Internal: Insert helpers
------------------------------------------------------------------------

insertFileProperty :: DBHandle -> Text -> Property -> IO ()
insertFileProperty db fileId prop =
  dbExecute_
    db
    insertFilePropertySQL
    [ SqlText fileId
    , SqlText (T.pack (prop ^. name))
    , SqlText (T.pack (prop ^. value))
    , SqlBool (prop ^. inherited)
    ]

insertEntry :: DBHandle -> Text -> Maybe Text -> Text -> Entry -> IO ()
insertEntry db fileId parentId filePath entry = do
  entryId <- toText <$> nextRandom
  let (kw, kwClosed) = keywordInfo (entry ^. entryKeyword)
  dbExecute_
    db
    insertEntrySQL
    [ SqlText entryId
    , SqlText fileId
    , maybe SqlNull SqlText parentId
    , SqlInt (fromIntegral (entry ^. entryDepth))
    , maybe SqlNull SqlText kw
    , SqlBool kwClosed
    , maybe SqlNull (SqlText . T.pack) (entry ^. entryPriority)
    , SqlText (T.pack (entry ^. entryHeadline))
    , maybe SqlNull (SqlText . T.pack) (entry ^. entryVerb)
    , SqlText (T.pack (entry ^. entryTitle))
    , maybe SqlNull (SqlText . T.pack) (entry ^. entryContext)
    , maybe SqlNull (SqlText . T.pack) (entry ^. entryLocator)
    , SqlInt (fromIntegral (entry ^. entryLoc . pos))
    , SqlText filePath
    ]
  -- Tags
  mapM_ (insertTag db entryId) (entry ^. entryTags)
  -- Properties
  mapM_ (insertProperty db entryId) (entry ^. entryProperties)
  -- Stamps
  mapM_ (insertStamp db entryId) (entry ^. entryStamps)
  -- Log entries
  mapM_ (insertLogEntry db entryId) (entry ^. entryLogEntries)
  -- Body blocks
  insertBody db entryId (entry ^. entryBody)
  -- Sub-entries
  mapM_ (insertEntry db fileId (Just entryId) filePath) (entry ^. entryItems)

insertTag :: DBHandle -> Text -> Tag -> IO ()
insertTag db entryId (PlainTag tag) =
  dbExecute_ db insertTagSQL [SqlText entryId, SqlText (T.pack tag)]

insertProperty :: DBHandle -> Text -> Property -> IO ()
insertProperty db entryId prop =
  dbExecute_
    db
    insertPropertySQL
    [ SqlText entryId
    , SqlText (T.pack (prop ^. name))
    , SqlText (T.pack (prop ^. value))
    , SqlBool (prop ^. inherited)
    , SqlInt (fromIntegral (prop ^. propertyLoc . pos))
    ]

insertStamp :: DBHandle -> Text -> Stamp -> IO ()
insertStamp db entryId stamp = do
  let (stampType, time) = stampInfo stamp
      stampLoc = stamp ^. _stampLoc
  dbExecute_
    db
    insertStampSQL
    [ SqlText entryId
    , SqlText stampType
    , SqlUTCTime (timeStartToUTCTime time)
    , maybe SqlNull SqlUTCTime (timeEndToUTCTime time)
    , SqlText (timeKindText (time ^. timeKind))
    , SqlInt (fromIntegral (stampLoc ^. pos))
    ]

insertLogEntry :: DBHandle -> Text -> LogEntry -> IO ()
insertLogEntry db entryId le = case le of
  LogClosing loc t _ ->
    insertLogRow "closing" (loc ^. pos) (timeStartToUTCTime t) Nothing Nothing Nothing Nothing
  LogState loc _ toKw t _ -> do
    let (fromSt, toSt) = stateInfo toKw (entry'sKeyword le)
    insertLogRow "state" (loc ^. pos) (timeStartToUTCTime t) fromSt toSt Nothing Nothing
  LogNote loc t _ ->
    insertLogRow "note" (loc ^. pos) (timeStartToUTCTime t) Nothing Nothing Nothing Nothing
  LogRescheduled loc t oldT _ ->
    insertLogRow "rescheduled" (loc ^. pos) (timeStartToUTCTime t) Nothing Nothing (Just (timeStartToUTCTime oldT)) Nothing
  LogNotScheduled loc t oldT _ ->
    insertLogRow "not-scheduled" (loc ^. pos) (timeStartToUTCTime t) Nothing Nothing (Just (timeStartToUTCTime oldT)) Nothing
  LogDeadline loc t oldT _ ->
    insertLogRow "deadline" (loc ^. pos) (timeStartToUTCTime t) Nothing Nothing (Just (timeStartToUTCTime oldT)) Nothing
  LogNoDeadline loc t oldT _ ->
    insertLogRow "no-deadline" (loc ^. pos) (timeStartToUTCTime t) Nothing Nothing (Just (timeStartToUTCTime oldT)) Nothing
  LogRefiling loc t _ ->
    insertLogRow "refiling" (loc ^. pos) (timeStartToUTCTime t) Nothing Nothing Nothing Nothing
  LogClock loc t mDur ->
    insertLogRow "clock" (loc ^. pos) (timeStartToUTCTime t) Nothing Nothing Nothing (durationToMins <$> mDur)
  LogBook _ entries ->
    mapM_ (insertLogEntry db entryId) entries
 where
  insertLogRow ::
    Text -> Int -> UTCTime -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Int -> IO ()
  insertLogRow logType fileLine logTime fromState toState oldTime durMins =
    dbExecute_
      db
      insertLogEntrySQL
      [ SqlText entryId
      , SqlText logType
      , SqlUTCTime logTime
      , maybe SqlNull SqlText fromState
      , maybe SqlNull SqlText toState
      , maybe SqlNull SqlUTCTime oldTime
      , maybe SqlNull (SqlInt . fromIntegral) durMins
      , SqlInt (fromIntegral fileLine)
      ]

insertBody :: DBHandle -> Text -> Body -> IO ()
insertBody db entryId body =
  mapM_ insertBlock (zip [0 :: Int ..] (body ^. blocks))
 where
  insertBlock (seqNum, block) = do
    let (blockType, content, fileLine) = blockInfo block
    dbExecute_
      db
      insertBodyBlockSQL
      [ SqlText entryId
      , SqlText blockType
      , SqlText content
      , SqlInt (fromIntegral seqNum)
      , SqlInt (fromIntegral fileLine)
      ]

------------------------------------------------------------------------
-- SQL statements
------------------------------------------------------------------------

insertFileSQL :: Text
insertFileSQL =
  "INSERT INTO files (id, path, mtime, hash) VALUES (?, ?, ?, ?)"

insertFilePropertySQL :: Text
insertFilePropertySQL =
  "INSERT INTO file_properties (file_id, name, value, inherited) \
  \VALUES (?, ?, ?, ?)"

insertCategorySQL :: Text
insertCategorySQL =
  "INSERT INTO categories (file_id, category) VALUES (?, ?)"

insertEntrySQL :: Text
insertEntrySQL =
  "INSERT INTO entries (entry_id, file_id, parent_id, depth, keyword, \
  \keyword_closed, priority, headline, verb, title, context, locator, \
  \file_line, path) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertTagSQL :: Text
insertTagSQL =
  "INSERT INTO entry_tags (entry_id, tag) VALUES (?, ?)"

insertPropertySQL :: Text
insertPropertySQL =
  "INSERT INTO entry_properties (entry_id, name, value, inherited, file_line) \
  \VALUES (?, ?, ?, ?, ?)"

insertStampSQL :: Text
insertStampSQL =
  "INSERT INTO entry_stamps (entry_id, stamp_type, time_start, time_end, \
  \time_kind, file_line) VALUES (?, ?, ?, ?, ?, ?)"

insertLogEntrySQL :: Text
insertLogEntrySQL =
  "INSERT INTO log_entries (entry_id, log_type, log_time, from_state, \
  \to_state, old_time, duration_mins, file_line) \
  \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"

insertBodyBlockSQL :: Text
insertBodyBlockSQL =
  "INSERT INTO body_blocks (entry_id, block_type, content, seq_num, \
  \file_line) VALUES (?, ?, ?, ?, ?)"

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

keywordInfo :: Maybe Keyword -> (Maybe Text, Bool)
keywordInfo Nothing = (Nothing, False)
keywordInfo (Just (OpenKeyword _ s)) = (Just (T.pack s), False)
keywordInfo (Just (ClosedKeyword _ s)) = (Just (T.pack s), True)

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

stateInfo :: Maybe Keyword -> Maybe Keyword -> (Maybe Text, Maybe Text)
stateInfo toKw fromKw =
  ( kwText <$> fromKw
  , kwText <$> toKw
  )
 where
  kwText (OpenKeyword _ s) = T.pack s
  kwText (ClosedKeyword _ s) = T.pack s

-- The LogState constructor stores (toKeyword, fromKeyword) but we want
-- the other way around for the database row.
entry'sKeyword :: LogEntry -> Maybe Keyword
entry'sKeyword (LogState _ kw _ _ _) = Just kw
entry'sKeyword _ = Nothing

blockInfo :: Block -> (Text, Text, Int)
blockInfo (Whitespace loc s) = ("whitespace", T.pack s, loc ^. pos)
blockInfo (Paragraph loc ls) = ("paragraph", T.pack (unlines ls), loc ^. pos)
blockInfo (Drawer loc dt ls) =
  ("drawer", T.pack (drawerName dt <> "\n" <> unlines ls), loc ^. pos)
blockInfo (InlineTask loc _) = ("inline-task", "", loc ^. pos)

drawerName :: DrawerType -> String
drawerName (PlainDrawer s) = s
drawerName (BeginDrawer s) = s

durationToMins :: Duration -> Int
durationToMins d = fromIntegral (d ^. hours * 60 + d ^. mins)

hashFile :: FilePath -> IO ByteString
hashFile path = MD5.hash <$> BS.readFile path
