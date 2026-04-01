{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Org.DB.Deserialize (
  -- * Single-entity reconstruction
  rowToTime,
  rowToTimeSuffix,
  rowToKeyword,
  rowToStamp,
  rowToLogEntry,
  rowToBlock,
  rowToProperty,
  rowToTag,
  rowToEntry,
  rowToOrgFile,

  -- * Bulk loading
  loadEntry,
  loadOrgFile,
  loadCollection,
) where

import Data.Int (Int64)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Org.DB.Types
import Org.Types

------------------------------------------------------------------------
-- Primitive reconstruction
------------------------------------------------------------------------

-- | Reconstruct a Time from MJD integer fields stored in the database.
rowToTime ::
  Int ->
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe TimeSuffix ->
  Time
rowToTime day mDayEnd mStart mEnd mKind mSuffix =
  Time
    { _timeKind = parseTimeKind mKind
    , _timeDay = fromIntegral day
    , _timeDayEnd = fromIntegral <$> mDayEnd
    , _timeStart = fromIntegral <$> mStart
    , _timeEnd = fromIntegral <$> mEnd
    , _timeSuffix = mSuffix
    }

parseTimeKind :: Maybe Text -> TimeKind
parseTimeKind (Just "inactive") = InactiveTime
parseTimeKind _ = ActiveTime

-- | Reconstruct a TimeSuffix from database columns.
rowToTimeSuffix ::
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Maybe TimeSuffix
rowToTimeSuffix (Just kindT) (Just num) (Just spanT) mLargerNum mLargerSpan =
  Just
    TimeSuffix
      { _suffixKind = parseSuffixKind kindT
      , _suffixNum = fromIntegral num
      , _suffixSpan = parseTimeSpan spanT
      , _suffixLargerSpan = do
          n <- mLargerNum
          s <- mLargerSpan
          pure (fromIntegral n, parseTimeSpan s)
      }
rowToTimeSuffix _ _ _ _ _ = Nothing

parseSuffixKind :: Text -> TimeSuffixKind
parseSuffixKind "repeat_plus" = TimeRepeatPlus
parseSuffixKind "dotted_repeat" = TimeDottedRepeat
parseSuffixKind "within" = TimeWithin
parseSuffixKind _ = TimeRepeat

parseTimeSpan :: Text -> TimeSpan
parseTimeSpan "week" = WeekSpan
parseTimeSpan "month" = MonthSpan
parseTimeSpan "year" = YearSpan
parseTimeSpan _ = DaySpan

-- | Reconstruct a Keyword from type and value strings.
rowToKeyword :: Text -> Text -> Keyword
rowToKeyword "closed" val = ClosedKeyword noLoc (T.unpack val)
rowToKeyword _ val = OpenKeyword noLoc (T.unpack val)

noLoc :: Loc
noLoc = Loc "" 0

------------------------------------------------------------------------
-- Row type reconstruction
------------------------------------------------------------------------

-- | Reconstruct a Stamp from an EntryStampRow.
rowToStamp :: EntryStampRow -> Stamp
rowToStamp r =
  let suffix =
        rowToTimeSuffix
          (esrSuffixKind r)
          (esrSuffixNum r)
          (esrSuffixSpan r)
          (esrSuffixLargerNum r)
          (esrSuffixLargerSpan r)
      time =
        rowToTime
          (esrDay r)
          (esrDayEnd r)
          (esrTimeStart r)
          (esrTimeEnd r)
          (Just (esrTimeKind r))
          suffix
      loc = Loc "" (fromMaybe 0 (esrByteOffset r))
   in case esrStampType r of
        "closed" -> ClosedStamp loc time
        "scheduled" -> ScheduledStamp loc time
        "deadline" -> DeadlineStamp loc time
        _ -> ActiveStamp loc time

-- | Reconstruct a Tag from an EntryTagRow.
rowToTag :: EntryTagRow -> Tag
rowToTag r = PlainTag (T.unpack (etrTag r))

-- | Reconstruct a Property from an EntryPropertyRow.
rowToProperty :: EntryPropertyRow -> Property
rowToProperty r =
  Property
    { _propertyLoc = Loc "" (fromMaybe 0 (eprByteOffset r))
    , _inherited = eprIsInherited r
    , _name = T.unpack (eprName r)
    , _value = T.unpack (eprValue r)
    }

-- | Reconstruct a Block from a BodyBlockRow.
rowToBlock :: BodyBlockRow -> Block
rowToBlock r =
  let loc = Loc "" (fromMaybe 0 (bbrByteOffset r))
      content = maybe "" T.unpack (bbrContent r)
   in case bbrBlockType r of
        "whitespace" -> Whitespace loc content
        "paragraph" -> Paragraph loc (lines content)
        "drawer" ->
          let dName = maybe "" T.unpack (bbrDrawerName r)
              dt = case bbrDrawerType r of
                Just "begin" -> BeginDrawer dName
                _ -> PlainDrawer dName
           in Drawer loc dt (lines content)
        -- InlineTask reconstruction is lossy — stored as empty entry
        _ -> Paragraph loc []

{- | Reconstruct a LogEntry from a LogEntryRow and its nested children.
Children are LogEntryRows whose lerLogbookId matches this row's lerId.
Body blocks are looked up from the map keyed by log entry id.
-}
rowToLogEntry :: LogEntryRow -> [LogEntryRow] -> M.Map Int64 [LogBodyBlockRow] -> LogEntry
rowToLogEntry r children bodyBlockMap =
  let loc = Loc "" (fromMaybe 0 (lerByteOffset r))
      myBlocks = M.findWithDefault [] (lerId r) bodyBlockMap
      mBody = if null myBlocks then Nothing else Just (Body (map rowToLogBodyBlock myBlocks))
      mTime = mkLogTime r
      mOrigTime = mkOrigTime r
      mDur = mkDuration r
      mFromKw = mkKeyword (lerFromKeyword r) (lerFromKeywordType r)
      mToKw = mkKeyword (lerToKeyword r) (lerToKeywordType r)
   in case lerLogType r of
        "closing" -> LogClosing loc (fromMaybe defaultTime mTime) mBody
        "state" ->
          LogState
            loc
            (fromMaybe (OpenKeyword noLoc "") mToKw)
            mFromKw
            (fromMaybe defaultTime mTime)
            mBody
        "note" -> LogNote loc (fromMaybe defaultTime mTime) mBody
        "rescheduled" ->
          LogRescheduled loc (fromMaybe defaultTime mTime) (fromMaybe defaultTime mOrigTime) mBody
        "not_scheduled" ->
          LogNotScheduled loc (fromMaybe defaultTime mTime) (fromMaybe defaultTime mOrigTime) mBody
        "deadline" ->
          LogDeadline loc (fromMaybe defaultTime mTime) (fromMaybe defaultTime mOrigTime) mBody
        "no_deadline" ->
          LogNoDeadline loc (fromMaybe defaultTime mTime) (fromMaybe defaultTime mOrigTime) mBody
        "refiling" -> LogRefiling loc (fromMaybe defaultTime mTime) mBody
        "clock" -> LogClock loc (fromMaybe defaultTime mTime) mDur
        "logbook" ->
          LogBook loc (map (\c -> rowToLogEntry c [] bodyBlockMap) children)
        _ -> LogNote loc (fromMaybe defaultTime mTime) mBody

mkLogTime :: LogEntryRow -> Maybe Time
mkLogTime r = case lerTimeDay r of
  Just day ->
    Just $
      rowToTime
        day
        Nothing
        (lerTimeStart r)
        (lerTimeEnd r)
        (lerTimeKind r)
        Nothing
  Nothing -> Nothing

mkOrigTime :: LogEntryRow -> Maybe Time
mkOrigTime r = case lerOrigTimeDay r of
  Just day ->
    let suffix =
          rowToTimeSuffix
            (lerOrigSuffixKind r)
            (lerOrigSuffixNum r)
            (lerOrigSuffixSpan r)
            Nothing
            Nothing
     in Just $
          rowToTime
            day
            (lerOrigTimeDayEnd r)
            (lerOrigTimeStart r)
            (lerOrigTimeEnd r)
            (lerOrigTimeKind r)
            suffix
  Nothing -> Nothing

mkDuration :: LogEntryRow -> Maybe Duration
mkDuration r = case (lerDurationHours r, lerDurationMins r) of
  (Just h, Just m) -> Just (Duration (fromIntegral h) (fromIntegral m))
  _ -> Nothing

mkKeyword :: Maybe Text -> Maybe Text -> Maybe Keyword
mkKeyword (Just val) (Just typ) = Just (rowToKeyword typ val)
mkKeyword _ _ = Nothing

-- | Reconstruct a Block from a LogBodyBlockRow.
rowToLogBodyBlock :: LogBodyBlockRow -> Block
rowToLogBodyBlock r =
  let loc = Loc "" (fromMaybe 0 (lbbrByteOffset r))
      content = maybe "" T.unpack (lbbrContent r)
   in case lbbrBlockType r of
        "whitespace" -> Whitespace loc content
        "paragraph" -> Paragraph loc (lines content)
        "drawer" ->
          let dName = maybe "" T.unpack (lbbrDrawerName r)
              dt = case lbbrDrawerType r of
                Just "begin" -> BeginDrawer dName
                _ -> PlainDrawer dName
           in Drawer loc dt (lines content)
        _ -> Paragraph loc []

bodyFromText :: Text -> Body
bodyFromText t
  | T.null t = Body []
  | otherwise = Body [Paragraph noLoc (lines (T.unpack t))]

defaultTime :: Time
defaultTime = Time ActiveTime 0 Nothing Nothing Nothing Nothing

------------------------------------------------------------------------
-- Entry reconstruction (from rows + child data)
------------------------------------------------------------------------

-- | Reconstruct an Entry from its row and all associated child rows.
rowToEntry ::
  EntryRow ->
  [EntryTagRow] ->
  [EntryPropertyRow] ->
  [EntryStampRow] ->
  [LogEntryRow] ->
  M.Map Int64 [LogBodyBlockRow] ->
  [BodyBlockRow] ->
  [Entry] ->
  Entry
rowToEntry er tags props stamps logs logBodyBlockMap bodyBlocks children =
  Entry
    { _entryLoc = Loc "" (erByteOffset er)
    , _entryDepth = erDepth er
    , _entryKeyword = do
        kwType <- erKeywordType er
        kwVal <- erKeywordValue er
        pure (rowToKeyword kwType kwVal)
    , _entryPriority = T.unpack <$> erPriority er
    , _entryHeadline = T.unpack (erHeadline er)
    , _entryVerb = T.unpack <$> erVerb er
    , _entryTitle = T.unpack (erTitle er)
    , _entryContext = T.unpack <$> erContext er
    , _entryLocator = T.unpack <$> erLocator er
    , _entryTags = map rowToTag tags
    , _entryStamps = map rowToStamp stamps
    , _entryProperties = map rowToProperty props
    , _entryLogEntries = buildLogEntries logs logBodyBlockMap
    , _entryBody = Body (map rowToBlock bodyBlocks)
    , _entryItems = children
    }

-- | Build log entries from flat rows, reconstructing LogBook nesting.
buildLogEntries :: [LogEntryRow] -> M.Map Int64 [LogBodyBlockRow] -> [LogEntry]
buildLogEntries rows logBodyBlockMap =
  let
    -- Separate top-level entries from logbook children
    topLevel = filter (isNothing . lerLogbookId) rows
    childMap =
      M.fromListWith
        (++)
        [ (pid, [r])
        | r <- rows
        , Just pid <- [lerLogbookId r]
        ]
   in
    map (\r -> rowToLogEntry r (M.findWithDefault [] (lerId r) childMap) logBodyBlockMap) topLevel

------------------------------------------------------------------------
-- OrgFile reconstruction
------------------------------------------------------------------------

-- | Reconstruct an OrgFile from file row and associated data.
rowToOrgFile ::
  FileRow ->
  [FilePropertyRow] ->
  [Entry] ->
  OrgFile
rowToOrgFile fr fprops entries =
  OrgFile
    { _orgFilePath = T.unpack (frPath fr)
    , _orgFileHeader =
        Header
          { _headerPropertiesDrawer =
              [ Property (Loc (T.unpack (frPath fr)) 0) False (T.unpack (fprName fp)) (T.unpack (fprValue fp))
              | fp <- fprops
              , fprSource fp == "drawer"
              ]
          , _headerFileProperties =
              [ Property (Loc (T.unpack (frPath fr)) 0) False (T.unpack (fprName fp)) (T.unpack (fprValue fp))
              | fp <- fprops
              , fprSource fp == "file"
              ]
          , _headerPreamble = maybe (Body []) bodyFromText (frPreamble fr)
          }
    , _orgFileEntries = entries
    }

------------------------------------------------------------------------
-- Bulk loading from DB
------------------------------------------------------------------------

-- | Load a single entry and all its descendants from the database.
loadEntry :: DBHandle -> Text -> IO (Maybe Entry)
loadEntry db entryId = do
  mRow <- dbQueryOne db "SELECT * FROM entries WHERE id = ?" [SqlText entryId] :: IO (Maybe EntryRow)
  case mRow of
    Nothing -> pure Nothing
    Just er -> Just <$> loadEntryTree db er

-- | Load a full OrgFile from the database by file path.
loadOrgFile :: DBHandle -> Text -> IO (Maybe OrgFile)
loadOrgFile db path = do
  mFile <- dbQueryOne db "SELECT * FROM files WHERE path = ?" [SqlText path] :: IO (Maybe FileRow)
  case mFile of
    Nothing -> pure Nothing
    Just fr -> do
      fprops <- dbQuery db "SELECT * FROM file_properties WHERE file_id = ?" [SqlText (frId fr)]
      entries <- loadFileEntries db (frId fr)
      pure (Just (rowToOrgFile fr fprops entries))

-- | Load all OrgFiles from the database as a Collection.
loadCollection :: DBHandle -> IO Collection
loadCollection db = do
  files <- dbQuery db "SELECT * FROM files ORDER BY path" [] :: IO [FileRow]
  orgFiles <- mapM (loadSingleFile db) files
  pure (Collection (map OrgItem orgFiles))

------------------------------------------------------------------------
-- Internal loading helpers
------------------------------------------------------------------------

loadSingleFile :: DBHandle -> FileRow -> IO OrgFile
loadSingleFile db fr = do
  fprops <- dbQuery db "SELECT * FROM file_properties WHERE file_id = ?" [SqlText (frId fr)]
  entries <- loadFileEntries db (frId fr)
  pure (rowToOrgFile fr fprops entries)

-- | Load all top-level entries for a file and build the tree.
loadFileEntries :: DBHandle -> Text -> IO [Entry]
loadFileEntries db fileId = do
  allEntries <-
    dbQuery
      db
      "SELECT id, file_id, parent_id, depth, position, \
      \byte_offset, keyword_type, keyword_value, priority, \
      \headline, title, verb, context, locator, \
      \hash, mod_time, created_time, path::text \
      \FROM entries WHERE file_id = ? ORDER BY position"
      [SqlText fileId] ::
      IO [EntryRow]
  -- Batch-load all child data for the file's entries
  allTags <- loadBatch db "entry_tags" "entry_id" fileId
  allProps <- loadBatch db "entry_properties" "entry_id" fileId
  allStamps <- loadBatch db "entry_stamps" "entry_id" fileId
  allLogs <- loadBatch db "entry_log_entries" "entry_id" fileId
  allBlocks <- loadBatch db "entry_body_blocks" "entry_id" fileId
  -- Load log body blocks via join through entry_log_entries
  allLogBodyBlocks <-
    dbQuery
      db
      "SELECT lb.* FROM log_entry_body_blocks lb \
      \JOIN entry_log_entries le ON lb.log_entry_id = le.id \
      \JOIN entries e ON le.entry_id = e.id \
      \WHERE e.file_id = ? ORDER BY lb.log_entry_id, lb.position"
      [SqlText fileId] ::
      IO [LogBodyBlockRow]
  -- Group by entry_id / log_entry_id
  let tagMap = groupByKey etrEntryId allTags
      propMap = groupByKey eprEntryId allProps
      stampMap = groupByKey esrEntryId allStamps
      logMap = groupByKey lerEntryId allLogs
      logBodyBlockMap = groupByKey lbbrLogEntryId allLogBodyBlocks
      blockMap = groupByKey bbrEntryId allBlocks
  -- Build the tree from flat entries
  pure (buildEntryTree allEntries tagMap propMap stampMap logMap logBodyBlockMap blockMap)

-- | Batch-load rows for all entries in a file using a subquery.
loadBatch ::
  (FromRow r) =>
  DBHandle ->
  Text ->
  Text ->
  Text ->
  IO [r]
loadBatch db table col fileId =
  dbQuery
    db
    ( "SELECT t.* FROM "
        <> table
        <> " t JOIN entries e ON t."
        <> col
        <> " = e.id WHERE e.file_id = ? ORDER BY t."
        <> col
    )
    [SqlText fileId]

-- | Group rows by a key-extraction function.
groupByKey :: (Ord k) => (a -> k) -> [a] -> M.Map k [a]
groupByKey f = M.fromListWith (++) . map (\x -> (f x, [x]))

-- | Build a nested entry tree from flat rows using parent_id.
buildEntryTree ::
  [EntryRow] ->
  M.Map Text [EntryTagRow] ->
  M.Map Text [EntryPropertyRow] ->
  M.Map Text [EntryStampRow] ->
  M.Map Text [LogEntryRow] ->
  M.Map Int64 [LogBodyBlockRow] ->
  M.Map Text [BodyBlockRow] ->
  [Entry]
buildEntryTree allEntries tagMap propMap stampMap logMap logBodyBlockMap blockMap =
  let
    -- Build a map of entry_id -> child EntryRows
    childMap =
      M.fromListWith
        (++)
        [ (pid, [er])
        | er <- allEntries
        , Just pid <- [erParentId er]
        ]
    -- Recursively build entries
    buildEntry er =
      let eid = erId er
          children =
            map buildEntry $
              M.findWithDefault [] eid childMap
       in rowToEntry
            er
            (M.findWithDefault [] eid tagMap)
            (M.findWithDefault [] eid propMap)
            (M.findWithDefault [] eid stampMap)
            (M.findWithDefault [] eid logMap)
            logBodyBlockMap
            (M.findWithDefault [] eid blockMap)
            children
    -- Top-level entries have no parent
    topLevel = filter (isNothing . erParentId) allEntries
   in
    map buildEntry topLevel

loadEntryTree :: DBHandle -> EntryRow -> IO Entry
loadEntryTree db er = do
  let eid = erId er
  tags <- dbQuery db "SELECT * FROM entry_tags WHERE entry_id = ?" [SqlText eid]
  props <- dbQuery db "SELECT * FROM entry_properties WHERE entry_id = ?" [SqlText eid]
  stamps <- dbQuery db "SELECT * FROM entry_stamps WHERE entry_id = ?" [SqlText eid]
  logs <- dbQuery db "SELECT * FROM entry_log_entries WHERE entry_id = ? ORDER BY position" [SqlText eid]
  bodyBlocks <- dbQuery db "SELECT * FROM entry_body_blocks WHERE entry_id = ? ORDER BY position" [SqlText eid]
  logBodyBlocks <-
    dbQuery
      db
      "SELECT lb.* FROM log_entry_body_blocks lb \
      \JOIN entry_log_entries le ON lb.log_entry_id = le.id \
      \WHERE le.entry_id = ? ORDER BY lb.log_entry_id, lb.position"
      [SqlText eid] ::
      IO [LogBodyBlockRow]
  let logBodyBlockMap = groupByKey lbbrLogEntryId logBodyBlocks
  children <-
    dbQuery
      db
      "SELECT id, file_id, parent_id, depth, position, \
      \byte_offset, keyword_type, keyword_value, priority, \
      \headline, title, verb, context, locator, \
      \hash, mod_time, created_time, path::text \
      \FROM entries WHERE parent_id = ? ORDER BY position"
      [SqlText eid] ::
      IO [EntryRow]
  childEntries <- mapM (loadEntryTree db) children
  pure (rowToEntry er tags props stamps logs logBodyBlockMap bodyBlocks childEntries)
