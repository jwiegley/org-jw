{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Org.Db where

import Control.Applicative
import Control.Lens
import Control.Lens.Unsound
import Control.Monad (unless, void, when)
import Data.Char (isAlphaNum, toLower)
import Data.Data.Lens (biplate)
import Data.List (intercalate, isInfixOf)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time (UTCTime, defaultTimeLocale)
import Data.Time.Format (formatTime, parseTimeM)
import FlatParse.Combinators
import FlatParse.Stateful hiding (optional, (<|>))
import FlatParse.Stateful qualified as FP
import Org.Parse
import Org.Print
import Org.Types
import System.FilePath.Posix
import Prelude hiding (readFile)

lined :: Traversal' [String] String
lined f a = lines <$> f (unlines a)

-- Roughly equivalent to: '^[0-9]{8,}[_-].+?( -- .+?|\[.+?\])\.[^.]+$'
fileNameRe ::
  FP.Parser
    r
    String
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
    pDateSlugSeparator = $(char '-') <|> $(char '_')
    pSlug = do
      manyTill
        anyChar
        (lookahead (void pTags <|> void pExt <|> eof))
    pTags =
      try
        ( spaces_
            *> $(string "--")
            *> spaces_
            *> someTill anyChar (lookahead ($(char '.') <|> eof))
        )
        <|> try
          ( skipMany singleSpace
              *> between
                $(char '[')
                $(char ']')
                (someTill anyChar (lookahead $(char ']')))
          )
    pExt = try $ do
      _ <- $(char '.')
      str <- many anyChar <* eof
      when ("." `isInfixOf` str) $
        err "Error parsing file extension"
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
      let res = parseMaybe fileNameRe (T.encodeUtf8 (T.pack path))
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
  case runParser fileNameRe () 0 (T.encodeUtf8 (T.pack nm)) of
    OK res _ _ ->
      ( \(stamp', slug', tags', ext') ->
          maybe "" (<> "-") stamp'
            <> slug'
            <> maybe "" ((\t -> "[" <> t <> "]") . unwords) tags'
            <> maybe "" ("." <>) ext'
      )
        <$> f res
    Err err ->
      error $ "impossible, failed to parse file name: " ++ show err
    Fail ->
      error $ "impossible, failed to parse file name: " ++ show nm

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
                  ?~ tags' ^.. traverse . _PlainTag
      )
        <$> f (map PlainTag tags)

fileActualSlug :: Lens' CollectionItem String
fileActualSlug = filePath . fileName . fileNameParts . _2

filePath :: Lens' CollectionItem FilePath
filePath f (OrgItem o) = OrgItem <$> (o & orgFilePath %%~ f)
filePath f (DataItem path) = DataItem <$> f path

-- If an Org-mode file has a '#+filetags' property, then the tags are read
-- from there, and written back there. Otherwise, the filename itself is used.
fileTags :: Lens' CollectionItem [Tag]
fileTags f (OrgItem o) = case o ^? orgFileProperty "filetags" . from tagList of
  Nothing -> OrgItem <$> (o & orgFilePath . fileNameTags %%~ f)
  Just filetags ->
    ( \filetags' ->
        OrgItem (o & orgFileProperty "filetags" . from tagList .~ filetags')
    )
      <$> f filetags
fileTags f (DataItem path) = DataItem <$> (path & fileNameTags %%~ f)

fileTitle :: Traversal' CollectionItem String
fileTitle = failing (_OrgItem . orgFileProperty "TITLE") fileActualSlug

sluggify :: String -> String
sluggify =
  useDashes
    . dropMultipleUnderscores
    . squashNonAlphanumerics
    . changeCertainCharacters
    . removeCertainCharacters
    . map toLower
  where
    dropMultipleUnderscores =
      intercalate "_" . filter (not . null) . splitOn "_"
    squashNonAlphanumerics =
      map (\c -> if isAlphaNum c then c else '_')
    changeCertainCharacters =
      map
        ( \c ->
            if
              | c == 'á' -> 'a'
              | c == 'í' -> 'i'
              | c == 'ú' -> 'u'
              | otherwise -> c
        )
    removeCertainCharacters =
      filter (\c -> c `notElem` ['’', '‘', '“', '”', '`', '\''])
    useDashes =
      map (\c -> if c == '_' then '-' else c)

fileSlug :: Fold CollectionItem String
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
    (_OrgItem . orgFileProperty "CREATED" . _Time)
    fileTimestamp

{-
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
-}

-- fileDateTime :: Traversal' CollectionItem Time
-- fileDateTime =
--   _OrgItem . orgFileHeader . headerStamps . traverse . _DateStamp . _2

-- A property for an entry is either:
--
--   - A property explicit defined by the entry, in its PROPERTIES drawer.
--
--   - A property implicitly inherited from its file or outline context.
property :: String -> Traversal' Entry String
property n = entryProperties . lookupProperty n

orgFileProperty :: String -> Traversal' OrgFile String
orgFileProperty n =
  orgFileHeader
    . failing
      (headerPropertiesDrawer . lookupProperty n)
      (headerFileProperties . lookupProperty n)

-- "Any property" for an entry includes the above, and also:
--
--   - A virtual property used as an alternate way to access details about the
--     entry.
anyProperty :: String -> Fold Entry String
anyProperty n =
  failing
    (entryProperties . lookupProperty n)
    (maybe ignored runFold (lookup n specialProperties))

-- jww (2024-05-13): Need to handle inherited tags
specialProperties :: [(String, ReifiedFold Entry String)]
specialProperties =
  [ -- All tags, including inherited ones.
    ("ALLTAGS", undefined),
    -- t if task is currently blocked by children or siblings.
    ("BLOCKED", undefined),
    -- The category of an entry. jww (2024-05-13): NYI
    ("CATEGORY", Fold (entryLoc . file)),
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
    ("FILE", Fold (entryLoc . file)),
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
    ("TODO", Fold (entryKeyword . _Just . keywordString . filtered isTodo)),
    ------------------------------------------------------------------------
    -- The following are not defined by Org-mode as special
    ------------------------------------------------------------------------
    ("OFFSET", Fold (entryLoc . pos . re _Show)),
    ("DEPTH", Fold (entryDepth . re _Show)),
    ("KEYWORD", Fold (entryKeyword . _Just . keywordString)),
    ("TITLE", Fold entryTitle),
    ("CONTEXT", Fold (entryContext . _Just)),
    ("LOCATOR", Fold (entryLocator . _Just))
  ]

keywordString :: Lens' Keyword String
keywordString f (OpenKeyword loc kw) = OpenKeyword loc <$> f kw
keywordString f (ClosedKeyword loc kw) = ClosedKeyword loc <$> f kw

tagString :: Lens' Tag String
tagString f (PlainTag txt) = PlainTag <$> f txt

keyword :: Traversal' Entry String
keyword = entryKeyword . _Just . keywordString

entryId :: Traversal' Entry String
entryId = property "ID"

entryCategory :: Traversal' Entry String
entryCategory = property "CATEGORY"

tagList :: Iso' [Tag] String
tagList =
  iso
    ( \tags ->
        intercalate
          ":"
          (":" : tags ^.. traverse . tagString ++ [":"])
    )
    (map PlainTag . filter (not . null) . splitOn ":")

entryTagString :: Traversal' Entry String
entryTagString f e = e & entryTags . tagList %%~ f

leadSpace :: Traversal' Body String
leadSpace = blocks . _head . _Whitespace . _2

endSpace :: Traversal' Body String
endSpace = blocks . _last . _Whitespace . _2

_Time :: Prism' String Time
_Time = prism' showTime (parseMaybe parseTime . T.encodeUtf8 . T.pack)

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
    ptime :: TimestampFormat -> Maybe (TimestampFormat, UTCTime)
    ptime tf = (tf,) <$> parseTime' (tsFormatFmt tf)
    parseTime' :: String -> Maybe UTCTime
    parseTime' fmt = parseTimeM False defaultTimeLocale fmt str

createdTime :: Traversal' Entry Time
createdTime = property "CREATED" . _Time

editedTime :: Traversal' Entry Time
editedTime = property "EDITED" . _Time

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

hardCodedInheritedProperties :: [String]
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
entriesMap :: Collection -> Map String [Entry]
entriesMap db =
  foldr
    addEntryToMap
    M.empty
    (db ^.. items . traverse . _OrgItem . allEntries)

addEntryToMap :: Entry -> Map String [Entry] -> Map String [Entry]
addEntryToMap e =
  at ident
    %~ Just . \case
      Nothing -> [e]
      Just es -> (e : es)
  where
    ident = fromMaybe "" (e ^? entryId)

addRefToMap :: String -> Map String [Entry] -> Map String [Entry]
addRefToMap ident =
  at ident
    %~ Just . \case
      Nothing -> []
      Just es -> es

foldAllEntries :: Collection -> b -> (Entry -> b -> b) -> b
foldAllEntries cs z f =
  foldr f z (cs ^.. items . traverse . _OrgItem . allEntries)

findDuplicates :: (Ord a) => [a] -> [a]
findDuplicates = M.keys . M.filter (> 1) . foldr go M.empty
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
isTodo :: String -> Bool
isTodo kw =
  kw
    `elem` [ "TODO",
             "PROJECT",
             "DOING",
             "WAIT",
             "DEFER",
             "DELEGATED",
             "DONE",
             "COMPLETE",
             "ABORTED",
             "CANCELED"
           ]

isArchive :: OrgFile -> Bool
isArchive org = "archive" `isInfixOf` (org ^. orgFilePath)

entryStateHistory :: Traversal' Entry LogEntry
entryStateHistory = entryLogEntries . traverse . biplate

transitionsOf :: Config -> String -> [String]
transitionsOf cfg kw =
  fromMaybe [] (lookup kw (cfg ^. keywordTransitions))
