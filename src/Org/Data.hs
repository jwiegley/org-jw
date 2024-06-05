{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Org.Data where

import Control.Arrow (left)
import Control.Lens
import Control.Lens.Unsound
import Control.Monad (unless, void, when)
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString qualified as B
import Data.Char (isAlphaNum, isDigit)
import Data.Data.Lens (biplate)
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
import Text.Megaparsec.Char
import Prelude hiding (readFile)

lined :: Traversal' [Text] Text
lined f a = T.lines <$> f (T.unlines a)

-- Roughly equivalent to: '^[0-9]{8,}[_-].+?( -- .+?|\[.+?\])\.[^.]+$'
fileNameRe ::
  ParsecT
    Void
    String
    m
    ( Maybe FilePath,
      FilePath,
      Maybe [String],
      Maybe FilePath
    )
fileNameRe = do
  date <- optional (pDate <* optional pDateSlugSeparator)
  slug <- pSlug
  tags <- optional pTags
  ext <- optional pExt
  pure (date, slug, words <$> tags, ext)
  where
    pDate =
      try $
        (++)
          <$> count 8 (satisfy isDigit)
          <*> many (satisfy isDigit)
    pDateSlugSeparator = char '-' <|> char '_'
    pSlug = do
      manyTill
        anyChar
        (lookAhead (void pTags <|> void pExt <|> eof))
    pTags =
      try
        ( spaces_
            *> string "--"
            *> spaces_
            *> someTill anyChar (lookAhead (void (char '.') <|> eof))
        )
        <|> try
          ( skipMany singleSpace
              *> between
                (char '[')
                (char ']')
                (someTill anyChar (lookAhead (char ']')))
          )
    pExt = try $ do
      _ <- char '.'
      str <- many anyChar <* eof
      when ("." `isInfixOf` str) $
        fail "Error parsing file extension"
      pure str

fileNameReTest :: IO ()
fileNameReTest = do
  test
    "foo.org"
    (Nothing, "foo", Nothing, Just "org")
  test
    "foo-.org"
    (Nothing, "foo-", Nothing, Just "org")
  test
    "-foo-.org"
    (Nothing, "-foo-", Nothing, Just "org")
  test
    "-foo.org"
    (Nothing, "-foo", Nothing, Just "org")
  test
    "20240601-foo.org"
    (Just "20240601", "foo", Nothing, Just "org")
  test
    "20240601foo.org"
    (Just "20240601", "foo", Nothing, Just "org")
  test
    "20240601foo-.org"
    (Just "20240601", "foo-", Nothing, Just "org")
  test
    "20240601-foo-.org"
    (Just "20240601", "foo-", Nothing, Just "org")
  test
    "20240601-foo[foo bar].org"
    (Just "20240601", "foo", Just ["foo", "bar"], Just "org")
  test
    "20240601-foo [foo bar].org"
    (Just "20240601", "foo", Just ["foo", "bar"], Just "org")
  test
    "20240601[foo bar].org"
    (Just "20240601", "", Just ["foo", "bar"], Just "org")
  test
    "20240601 [foo bar].org"
    (Just "20240601", "", Just ["foo", "bar"], Just "org")
  test
    "20240601-foo --foo bar.org"
    (Just "20240601", "foo --foo bar", Nothing, Just "org")
  test
    "20240601-foo -- foo bar.org"
    (Just "20240601", "foo", Just ["foo", "bar"], Just "org")
  test
    "20240601--foo bar.org"
    (Just "20240601", "-foo bar", Nothing, Just "org")
  test
    "20240601-- foo bar.org"
    (Just "20240601", "- foo bar", Nothing, Just "org")
  test
    "20240601 -- foo bar.org"
    (Just "20240601", "", Just ["foo", "bar"], Just "org")
  test
    "foo"
    (Nothing, "foo", Nothing, Nothing)
  test
    "foo-"
    (Nothing, "foo-", Nothing, Nothing)
  test
    "-foo-"
    (Nothing, "-foo-", Nothing, Nothing)
  test
    "-foo"
    (Nothing, "-foo", Nothing, Nothing)
  test
    "20240601-foo"
    (Just "20240601", "foo", Nothing, Nothing)
  test
    "20240601foo"
    (Just "20240601", "foo", Nothing, Nothing)
  test
    "20240601foo-"
    (Just "20240601", "foo-", Nothing, Nothing)
  test
    "20240601-foo-"
    (Just "20240601", "foo-", Nothing, Nothing)
  test
    "20240601-foo[foo bar]"
    (Just "20240601", "foo", Just ["foo", "bar"], Nothing)
  test
    "20240601-foo [foo bar]"
    (Just "20240601", "foo", Just ["foo", "bar"], Nothing)
  test
    "20240601-foo --foo bar"
    (Just "20240601", "foo --foo bar", Nothing, Nothing)
  test
    "20240601-foo -- foo bar"
    (Just "20240601", "foo", Just ["foo", "bar"], Nothing)
  test
    "foo."
    (Nothing, "foo", Nothing, Just "")
  test
    "foo.."
    (Nothing, "foo.", Nothing, Just "")
  where
    test ::
      FilePath ->
      ( Maybe FilePath,
        FilePath,
        Maybe [String],
        Maybe FilePath
      ) ->
      IO ()
    test path expect = do
      let res = parseMaybe @Void fileNameRe path
      unless (res == Just expect) $
        error $
          "Failed to parse " ++ show path ++ ", got: " ++ show res

fileNameParts ::
  Lens'
    FilePath
    ( Maybe FilePath,
      FilePath,
      Maybe [String],
      Maybe FilePath
    )
fileNameParts f nm = do
  case runIdentity (runParserT @_ @Void fileNameRe "<path>" nm) of
    Right res ->
      ( \(stamp', slug', tags', ext') ->
          maybe "" (<> "-") stamp'
            <> slug'
            <> maybe "" ((\t -> "[" <> t <> "]") . unwords) tags'
            <> maybe "" ("." <>) ext'
      )
        <$> f res
    Left err ->
      error $
        "impossible, failed to parse file name: "
          ++ errorBundlePretty err

dirName :: Lens' FilePath FilePath
dirName f path = (</> takeFileName path) <$> f (takeDirectory path)

fileName :: Lens' FilePath FilePath
fileName f path = (takeDirectory path </>) <$> f (takeFileName path)

fileNameTags :: Lens' FilePath [Tag]
fileNameTags f path =
  case path ^. fileNameParts . _3 of
    Nothing -> path <$ f []
    Just tags ->
      ( \tags' ->
          case tags' of
            [] -> path
            _ ->
              path
                & fileNameParts . _3
                  ?~ tags' ^.. traverse . _PlainTag . unpacked
      )
        <$> f (Prelude.map (PlainTag . T.pack) tags)

fileActualSlug :: Lens' CollectionItem Text
fileActualSlug = filePath . fileName . fileNameParts . _2 . packed

filePath :: Lens' CollectionItem FilePath
filePath f (OrgItem o) = OrgItem <$> (o & orgFilePath %%~ f)
filePath f (DataItem path) = DataItem <$> f path

-- If an Org-mode file has a '#+filetags' property, then the tags are read
-- from there, and written back there. Otherwise, the filename itself is used.
fileTags :: Lens' CollectionItem [Tag]
fileTags f (OrgItem o)
  | Prelude.null (o ^. orgFileHeader . headerTags) =
      OrgItem <$> (o & orgFilePath . fileNameTags %%~ f)
  | otherwise = OrgItem <$> (o & orgFileHeader . headerTags %%~ f)
fileTags f (DataItem path) = DataItem <$> (path & fileNameTags %%~ f)

fileTitle :: Traversal' CollectionItem Text
fileTitle = failing (_OrgItem . orgFileProperty "TITLE") fileActualSlug

sluggify :: Text -> Text
sluggify =
  useDashes
    . dropMultipleUnderscores
    . squashNonAlphanumerics
    . changeCertainCharacters
    . removeCertainCharacters
    . T.toLower
    . T.strip
  where
    dropMultipleUnderscores =
      T.intercalate "_" . filter (not . T.null) . T.splitOn "_"
    squashNonAlphanumerics =
      T.map (\c -> if isAlphaNum c then c else '_')
    changeCertainCharacters =
      T.map
        ( \c ->
            if
              | c == 'á' -> 'a'
              | c == 'í' -> 'i'
              | c == 'ú' -> 'u'
              | otherwise -> c
        )
    removeCertainCharacters =
      T.filter (\c -> c `notElem` ['’', '’'])
    useDashes =
      T.map (\c -> if c == '_' then '-' else c)

fileSlug :: Fold CollectionItem Text
fileSlug =
  failing
    (_OrgItem . orgFileProperty "SLUG")
    (fileTitle . to sluggify)

fileTimestamp :: Traversal' CollectionItem Time
fileTimestamp =
  failing (_OrgItem . orgFilePath) _DataItem . fileName . stringTime

fileCreatedTime :: Traversal' CollectionItem Time
fileCreatedTime =
  failing
    (_OrgItem . orgFileHeader . headerStamps . traverse . _CreatedStamp . _2)
    fileTimestamp

fileEditedTime :: Traversal' CollectionItem Time
fileEditedTime =
  failing
    (_OrgItem . orgFileHeader . headerStamps . traverse . _EditedStamp . _2)
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

fileDateTime :: Traversal' CollectionItem Time
fileDateTime =
  _OrgItem . orgFileHeader . headerStamps . traverse . _DateStamp . _2

-- A property for an entry is either:
--
--   - A property explicit defined by the entry, in its PROPERTIES drawer.
--
--   - A property implicitly inherited from its file or outline context.
property :: Text -> Traversal' Entry Text
property n = entryProperties . lookupProperty n

orgFileProperty :: Text -> Traversal' OrgFile Text
orgFileProperty n =
  orgFileHeader
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
    ("CATEGORY", Fold (entryLoc . file . packed)),
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
    ("FILE", Fold (entryLoc . file . packed)),
    -- The headline of the entry.
    ("ITEM", Fold entryHeadline),
    -- The priority of the entry, a string with a single letter.
    ("PRIORITY", Fold (entryPriority . _Just)),
    -- The scheduling timestamp.
    ("SCHEDULED", Fold (scheduledTime . re _Time)),
    -- The tags defined directly in the headline.
    ("TAGS", Fold entryTagString),
    -- The first keyword-less timestamp in the entry.
    ("TIMESTAMP", undefined),
    -- The first inactive timestamp in the entry.
    ("TIMESTAMP_IA", undefined),
    -- The TODO keyword of the entry.
    ("TODO", Fold (entryKeyword . _Just . keywordText . filtered isTodo)),
    ------------------------------------------------------------------------
    -- The following are not defined by Org-mode as special
    ------------------------------------------------------------------------
    ("LINE", Fold (entryLoc . line . re _Show . packed)),
    ("DEPTH", Fold (entryDepth . re _Show . packed)),
    ("KEYWORD", Fold (entryKeyword . _Just . keywordText)),
    ("TITLE", Fold entryTitle),
    ("CONTEXT", Fold (entryContext . _Just)),
    ("LOCATOR", Fold (entryLocator . _Just)),
    ("BODY", Fold (entryText . to (T.unlines . showBody "")))
  ]

keywordText :: Lens' Keyword Text
keywordText f (OpenKeyword loc kw) = OpenKeyword loc <$> f kw
keywordText f (ClosedKeyword loc kw) = ClosedKeyword loc <$> f kw

tagText :: Lens' Tag Text
tagText f (PlainTag txt) = PlainTag <$> f txt

keyword :: Traversal' Entry Text
keyword = entryKeyword . _Just . keywordText

entryId :: Traversal' Entry Text
entryId = property "ID"

entryCategory :: Traversal' Entry Text
entryCategory = property "CATEGORY"

entryTagString :: Traversal' Entry Text
entryTagString f e = do
  tags' <-
    f
      ( T.intercalate
          ":"
          (":" : e ^.. entryTags . traverse . tagText ++ [":"])
      )
  pure $
    e
      & entryTags
        .~ Prelude.map
          PlainTag
          (filter (not . T.null) (T.splitOn ":" tags'))

leadSpace :: Traversal' Body Text
leadSpace = blocks . _head . _Whitespace . _2

endSpace :: Traversal' Body Text
endSpace = blocks . _last . _Whitespace . _2

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

createdTime :: Traversal' Entry Time
createdTime = entryStamps . traverse . _CreatedStamp . _2

editedTime :: Traversal' Entry Time
editedTime = entryStamps . traverse . _EditedStamp . _2

dateTime :: Traversal' Entry Time
dateTime = entryStamps . traverse . _DateStamp . _2

scheduledTime :: Traversal' Entry Time
scheduledTime = entryStamps . traverse . _ScheduledStamp . _2

deadlineTime :: Traversal' Entry Time
deadlineTime = entryStamps . traverse . _DeadlineStamp . _2

closedTime :: Traversal' Entry Time
closedTime = entryStamps . traverse . _ClosedStamp . _2

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
inheritProperties (Property loc _ n v : ps) e =
  inheritProperties ps $
    if has (property n) e
      then e
      else e & entryProperties <>~ [Property loc True n v]

traverseEntries ::
  (Applicative f) =>
  [Property] ->
  (Entry -> f a) ->
  [Entry] ->
  f [a]
traverseEntries ps f = foldEntries ps (liftA2 (:) . f) (pure [])

entries :: [Property] -> Traversal' OrgFile Entry
entries ps f = orgFileEntries %%~ traverseEntries ps f

-- jww (2024-05-14): Inherited properties can be specified by the user
allEntries :: Traversal' OrgFile Entry
allEntries f org =
  org
    & entries
      ( filter
          (\p -> p ^. name `elem` hardCodedInheritedProperties)
          ( org ^. orgFileHeader . headerPropertiesDrawer
              ++ org ^. orgFileHeader . headerFileProperties
          )
      )
      f

allOrgFiles :: Traversal' Collection OrgFile
allOrgFiles = items . traverse . _OrgItem

allTaggedItems :: Traversal' Collection (FilePath, [Tag])
allTaggedItems = items . traverse . lensProduct filePath fileTags

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
entriesMap :: Collection -> Map Text [Entry]
entriesMap db =
  Prelude.foldr
    addEntryToMap
    M.empty
    (db ^.. items . traverse . _OrgItem . allEntries)

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

foldAllEntries :: Collection -> b -> (Entry -> b -> b) -> b
foldAllEntries cs z f =
  Prelude.foldr f z (cs ^.. items . traverse . _OrgItem . allEntries)

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
  Collection ->
  (Entry -> Map k a -> (b1 -> Index b1 -> b1) -> Map k a) ->
  Map k a
countEntries cs = foldAllEntries cs M.empty . tallyEntry

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
isArchive org = "archive" `isInfixOf` (org ^. orgFilePath)

entryStateHistory :: Traversal' Entry LogEntry
entryStateHistory = entryLogEntries . traverse . biplate

transitionsOf :: Config -> Text -> [Text]
transitionsOf cfg kw =
  fromMaybe [] (Prelude.lookup kw (cfg ^. keywordTransitions))

readOrgFile_ :: Config -> FilePath -> Text -> Either String OrgFile
readOrgFile_ cfg path content =
  left
    errorBundlePretty
    (runReader (runParserT parseOrgFile path content) cfg)

readOrgFile :: (MonadIO m) => Config -> FilePath -> ExceptT String m OrgFile
readOrgFile cfg path = do
  org <-
    liftIO $ withFile path ReadMode $ \h -> do
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

readCollectionItem ::
  (MonadIO m) =>
  Config ->
  FilePath ->
  ExceptT String m CollectionItem
readCollectionItem cfg path =
  if takeExtension path == ".org"
    then OrgItem <$> readOrgFile cfg path
    else pure $ DataItem path

readCollection ::
  (MonadIO m) =>
  Config ->
  [FilePath] ->
  ExceptT String m Collection
readCollection cfg paths = Collection <$> mapM (readCollectionItem cfg) paths
