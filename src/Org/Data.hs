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
import Data.Map hiding (filter)
import Data.Map qualified as M hiding (filter)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lens
import Data.Void
import Org.Parser
import Org.Printer
import Org.Types
import System.IO (IOMode (..), withFile)
import Text.Megaparsec
import Prelude hiding (readFile)

lookupProperty :: [Property] -> Text -> Maybe Text
lookupProperty ps n = ps ^? traverse . filtered (\x -> x ^. name == n) . value

lined :: Traversal' [Text] Text
lined f a = T.lines <$> f (T.unlines a)

-- A property for an entry is either:
--
--   - A property explicit defined by the entry, in its PROPERTIES drawer.
--
--   - A property implicitly inherited from its file or outline context.
property :: Text -> Traversal' Entry Text
property n =
  entryProperties . traverse . filtered (\x -> x ^. name == n) . value

-- "Any property" for an entry includes the above, and also:
--
--   - A virtual property used as an alternate way to access details about the
--     entry.
anyProperty :: Text -> Fold Entry Text
anyProperty n =
  failing
    (entryProperties . traverse . filtered (\x -> x ^. name == n) . value)
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

keyword :: Traversal' Entry Text
keyword = entryKeyword . _Just . keywordText

entryId :: Traversal' Entry Text
entryId = property "ID"

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

createdTime :: Traversal' Entry Time
createdTime = property "CREATED" . _Time

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
             "CATEGORY",
             "PROJECT",
             "DOING",
             "WAIT",
             "DEFER",
             "DELEGATED",
             "APPT",
             "DONE",
             "CANCELED"
           ]

entryStateHistory :: Traversal' Entry (Keyword, Maybe Keyword, Time)
entryStateHistory f e =
  e
    & entryLogEntries . traverse . _LogState %%~ \(t, mf, tm, mbody) ->
      (\(t', mf', tm') -> (t', mf', tm', mbody)) <$> f (t, mf, tm)

transitionsOf :: Config -> Text -> [Text]
transitionsOf cfg kw =
  fromMaybe [] (Prelude.lookup kw (cfg ^. keywordTransitions))
