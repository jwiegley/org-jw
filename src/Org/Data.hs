{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Org.Data where

import Control.Arrow (left)
import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString qualified as B
import Data.List (isInfixOf)
import Data.Map hiding (filter)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lens
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime, parseTimeM)
import Data.Void
import Org.Parser
import Org.Printer
import Org.Types
import System.Directory
import System.FilePath.Posix
import System.IO (IOMode (..), withFile)
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec hiding (match)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text
import Prelude hiding (readFile)

lined :: Traversal' [Text] Text
lined f a = T.lines <$> f (T.unlines a)

-- A property for an entry is either:
--
--   - A property explicit defined by the entry, in its PROPERTIES drawer.
--
--   - A property implicitly inherited from its file or outline context.
property :: Text -> Traversal' Entry Text
property n = entryProperties . lookupProperty n

fileProperty :: Text -> Traversal' OrgFile Text
fileProperty n =
  fileHeader
    . failing
      (headerPropertiesDrawer . lookupProperty n)
      (headerFileProperties . lookupProperty n)

-- "Any property" for an entry includes the above, and also:
--
--   - A virtual property used as an alternate way to access details about the
--     entry.
anyProperty :: Text -> Fold Entry Text
anyProperty n =
  failing
    (entryProperties . lookupProperty n)
    (maybe ignored runFold (Prelude.lookup n specialProperties))

-- jww (2024-05-13): Need to handle inherited tags
specialProperties :: [(Text, ReifiedFold Entry Text)]
specialProperties =
  [ -- All tags, including inherited ones.
    ("ALLTAGS", undefined),
    -- t if task is currently blocked by children or siblings.
    ("BLOCKED", undefined),
    -- The category of an entry. jww (2024-05-13): NYI
    ("CATEGORY", Fold (entryFile . packed)),
    -- The sum of CLOCK intervals in the subtree. org-clock-sum must be run
    -- first to compute the values in the current buffer.
    ("CLOCKSUM", undefined),
    -- The sum of CLOCK intervals in the subtree for today.
    -- org-clock-sum-today must be run first to compute the values in the
    -- current buffer.
    ("CLOCKSUM_T", undefined),
    -- When was this entry closed?
    ("CLOSED", Fold (closedTime . re _Time)),
    -- The deadline timestamp.
    ("DEADLINE", Fold (deadlineTime . re _Time)),
    -- The filename the entry is located in.
    ("FILE", Fold (entryFile . packed)),
    -- The headline of the entry.
    ("ITEM", Fold entryHeadline),
    -- The priority of the entry, a string with a single letter.
    ("PRIORITY", Fold (entryPriority . _Just)),
    -- The scheduling timestamp.
    ("SCHEDULED", Fold (scheduledTime . re _Time)),
    -- The tags defined directly in the headline.
    ("TAGS", undefined),
    -- The first keyword-less timestamp in the entry.
    ("TIMESTAMP", undefined),
    -- The first inactive timestamp in the entry.
    ("TIMESTAMP_IA", undefined),
    -- The TODO keyword of the entry.
    ("TODO", Fold (entryKeyword . _Just . keywordText . filtered isTodo)),
    ------------------------------------------------------------------------
    -- The following are not defined by Org-mode as special
    ------------------------------------------------------------------------
    ("LINE", Fold (entryLine . re _Show . packed)),
    ("COLUMN", Fold (entryColumn . re _Show . packed)),
    ("DEPTH", Fold (entryDepth . re _Show . packed)),
    ("KEYWORD", Fold (entryKeyword . _Just . keywordText)),
    ("TITLE", Fold entryTitle),
    ("CONTEXT", Fold (entryContext . _Just)),
    ("LOCATOR", Fold (entryLocator . _Just)),
    ("BODY", Fold (entryText . to (T.unlines . showBody "")))
  ]

keywordText :: Traversal' Keyword Text
keywordText = failing _OpenKeyword _ClosedKeyword

tagText :: Traversal' Tag Text
tagText = failing _SpecialTag _PlainTag

keyword :: Traversal' Entry Text
keyword = entryKeyword . _Just . keywordText

entryId :: Traversal' Entry Text
entryId = property "ID"

entryCategory :: Traversal' Entry Text
entryCategory = property "CATEGORY"

leadSpace :: Traversal' Body Text
leadSpace = blocks . _head . _Whitespace

endSpace :: Traversal' Body Text
endSpace = blocks . _last . _Whitespace

readOrgFile_ :: Config -> FilePath -> Text -> Either String OrgFile
readOrgFile_ cfg path content =
  left
    errorBundlePretty
    (runReader (runParserT parseOrgFile path content) cfg)

readOrgFile :: (MonadIO m) => Config -> FilePath -> ExceptT String m OrgFile
readOrgFile cfg path = do
  org <- liftIO $ withFile path ReadMode $ \h -> do
    content <- T.hGetContents h
    pure $ readOrgFile_ cfg path content
  liftEither org

_OrgFile :: Config -> FilePath -> Prism' Text OrgFile
_OrgFile cfg path =
  prism
    ( T.intercalate "\n"
        . showOrgFile (cfg ^. propertyColumn) (cfg ^. tagsColumn)
    )
    (left T.pack . readOrgFile_ cfg path)

readStdin :: (MonadIO m) => m Text
readStdin = T.decodeUtf8 <$> liftIO B.getContents

readFile :: (MonadIO m) => FilePath -> m Text
readFile path = T.decodeUtf8 <$> liftIO (B.readFile path)

readLines :: (MonadIO m) => FilePath -> m [Text]
readLines path = T.lines <$> readFile path

readOrgData ::
  (MonadIO m) =>
  Config ->
  [FilePath] ->
  ExceptT String m OrgData
readOrgData cfg paths = OrgData . M.fromList <$> mapM go paths
  where
    go path = (path,) <$> readOrgFile cfg path

_Time :: Prism' Text Time
_Time = prism' showTime (parseMaybe @Void parseTime)

data TimestampFormat
  = HourMinSec
  | HourMin
  | JustDay

tsFormatFmt :: TimestampFormat -> String
tsFormatFmt HourMinSec = "%Y%m%d%H%M%S"
tsFormatFmt HourMin = "%Y%m%d%H%M"
tsFormatFmt JustDay = "%Y%m%d"

tsFormatLen :: TimestampFormat -> Int
tsFormatLen HourMinSec = 14
tsFormatLen HourMin = 12
tsFormatLen JustDay = 8

fileNameRe :: Regex
fileNameRe =
  case compile
    defaultCompOpt
    defaultExecOpt
    "^(:?([0-9]{8,})-)?([^.]+)(:?\\.([^.]+))?$" of
    Left err -> error err
    Right x -> x
{-# NOINLINE fileNameRe #-}

fileNameParts :: Traversal' FilePath (Maybe FilePath, FilePath, Maybe FilePath)
fileNameParts f nm = do
  case match fileNameRe nm of
    [[_, _, stamp, slug, _, ext]] ->
      ( \(stamp', slug', ext') ->
          maybe "" (<> "-") stamp'
            <> slug'
            <> maybe "" ("." <>) ext'
      )
        <$> f
          ( if Prelude.null stamp
              then Nothing
              else Just stamp,
            slug,
            if Prelude.null ext
              then Nothing
              else Just ext
          )
    res -> error $ "impossible, but got this: " ++ show res

dirName :: Lens' FilePath FilePath
dirName f path = (</> takeFileName path) <$> f (takeDirectory path)

fileName :: Lens' FilePath FilePath
fileName f path = (takeDirectory path </>) <$> f (takeFileName path)

fileTitle :: Traversal' OrgFile Text
fileTitle =
  failing
    (fileProperty "TITLE")
    (filePath . fileName . fileNameParts . _2 . packed)

fileSlug :: Traversal' OrgFile Text
fileSlug =
  failing
    (fileProperty "SLUG")
    (filePath . fileName . fileNameParts . _2 . packed)

stringTime :: Traversal' String Time
stringTime f str =
  case ptime HourMinSec <|> ptime HourMin <|> ptime JustDay of
    Nothing -> pure str
    Just (tf, utct) -> do
      tm' <- f $ case tf of
        JustDay -> tm {_timeStart = Nothing}
        _ -> tm
      pure $
        formatTime
          defaultTimeLocale
          (tsFormatFmt tf)
          (timeStartToUTCTime tm')
      where
        tm = utcTimeToTime InactiveTime utct
  where
    ptime tf = (tf,) <$> parseTime' (tsFormatFmt tf)
    parseTime' fmt =
      parseTimeM False defaultTimeLocale fmt str

fileTimestamp :: Traversal' OrgFile Time
fileTimestamp = filePath . fileName . stringTime

fileCreatedTime :: Traversal' OrgFile Time
fileCreatedTime =
  failing
    (fileHeader . headerStamps . traverse . _CreatedStamp)
    fileTimestamp

fileEditedTime :: Traversal' OrgFile Time
fileEditedTime =
  failing
    (fileHeader . headerStamps . traverse . _EditedStamp)
    -- If the file does not have an EDITED stamp, we regard the filesystem
    -- modification time as the defined stamp.
    ( \f org ->
        let modTime = unsafePerformIO $ getModificationTime (org ^. filePath)
         in ( \tm' ->
                let modTime' = timeStartToUTCTime tm'
                 in unsafePerformIO
                      ( setModificationTime
                          (org ^. filePath)
                          modTime'
                      )
                      `seq` org
            )
              <$> f (utcTimeToTime InactiveTime modTime)
    )

fileDateTime :: Traversal' OrgFile Time
fileDateTime = fileHeader . headerStamps . traverse . _DateStamp

createdTime :: Traversal' Entry Time
createdTime = entryStamps . traverse . _CreatedStamp

editedTime :: Traversal' Entry Time
editedTime = entryStamps . traverse . _EditedStamp

dateTime :: Traversal' Entry Time
dateTime = entryStamps . traverse . _DateStamp

scheduledTime :: Traversal' Entry Time
scheduledTime = entryStamps . traverse . _ScheduledStamp

deadlineTime :: Traversal' Entry Time
deadlineTime = entryStamps . traverse . _DeadlineStamp

closedTime :: Traversal' Entry Time
closedTime = entryStamps . traverse . _ClosedStamp

foldEntries :: [Property] -> (Entry -> b -> b) -> b -> [Entry] -> b
foldEntries _ _ z [] = z
foldEntries props f z (e : es) =
  f
    (inheritProperties props e)
    (foldEntries props f z (e ^. entryItems ++ es))

hardCodedInheritedProperties :: [Text]
hardCodedInheritedProperties = ["COLUMNS", "CATEGORY", "ARCHIVE", "LOGGING"]

inheritProperties :: [Property] -> Entry -> Entry
inheritProperties [] e = e
inheritProperties (Property _ n v : ps) e =
  inheritProperties ps $
    if has (property n) e
      then e
      else e & entryProperties <>~ [Property True n v]

traverseEntries ::
  (Applicative f) =>
  [Property] ->
  (Entry -> f a) ->
  [Entry] ->
  f [a]
traverseEntries ps f = foldEntries ps (liftA2 (:) . f) (pure [])

entries :: [Property] -> Traversal' OrgFile Entry
entries ps f = fileEntries %%~ traverseEntries ps f

-- jww (2024-05-14): Inherited properties can be specified by the user
allEntries :: Traversal' OrgFile Entry
allEntries f org =
  org
    & entries
      ( filter
          (\p -> p ^. name `elem` hardCodedInheritedProperties)
          ( org ^. fileHeader . headerPropertiesDrawer
              ++ org ^. fileHeader . headerFileProperties
          )
      )
      f

-- This is the "raw" form of the entries map, with a few invalid yet
-- informational states:
--
--   - If a key has multiple values, there is an ID conflict between two or
--     more entries
--
--   - If a key has no value, there is a link to an unknown ID.
--
--   - If there are values behind the empty key, then there are entries with
--     no ID. This is fine except for certain cases, such as TODOs.
entriesMap :: OrgData -> Map Text [Entry]
entriesMap db =
  Prelude.foldr addEntryToMap M.empty (db ^.. orgFiles . traverse . allEntries)

addEntryToMap :: Entry -> Map Text [Entry] -> Map Text [Entry]
addEntryToMap e =
  at ident
    %~ Just . \case
      Nothing -> [e]
      Just es -> (e : es)
  where
    ident = fromMaybe "" (e ^? entryId)

addRefToMap :: Text -> Map Text [Entry] -> Map Text [Entry]
addRefToMap ident =
  at ident
    %~ Just . \case
      Nothing -> []
      Just es -> es

foldAllEntries :: OrgData -> b -> (Entry -> b -> b) -> b
foldAllEntries org z f =
  Prelude.foldr f z (org ^.. orgFiles . traverse . allEntries)

findDuplicates :: (Ord a) => [a] -> [a]
findDuplicates = M.keys . M.filter (> 1) . Prelude.foldr go M.empty
  where
    go x = at x %~ Just . maybe (1 :: Int) succ

tallyEntry ::
  (IxValue b1 ~ Int, At b1) =>
  (t1 -> t2 -> (b1 -> Index b1 -> b1) -> b2) ->
  t1 ->
  t2 ->
  b2
tallyEntry f e m = f e m $ \m' r -> m' & at r %~ Just . maybe (1 :: Int) succ

countEntries ::
  (IxValue b1 ~ Int, At b1) =>
  OrgData ->
  (Entry -> Map k a -> (b1 -> Index b1 -> b1) -> Map k a) ->
  Map k a
countEntries org = foldAllEntries org M.empty . tallyEntry

-- jww (2024-05-12): This should be driven by a configuration file
isTodo :: Text -> Bool
isTodo kw =
  kw
    `elem` [ "TODO",
             "PROJECT",
             "DOING",
             "WAIT",
             "DEFER",
             "DELEGATED",
             "APPT",
             "DONE",
             "CANCELED"
           ]

isArchive :: OrgFile -> Bool
isArchive org = "archive" `isInfixOf` (org ^. filePath)

entryStateHistory :: Traversal' Entry (Keyword, Maybe Keyword, Time)
entryStateHistory f e =
  e
    & entryLogEntries . traverse . _LogState %%~ \(t, mf, tm, mbody) ->
      (\(t', mf', tm') -> (t', mf', tm', mbody)) <$> f (t, mf, tm)

transitionsOf :: Config -> Text -> [Text]
transitionsOf cfg kw =
  fromMaybe [] (Prelude.lookup kw (cfg ^. keywordTransitions))
