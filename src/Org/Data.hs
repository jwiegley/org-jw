{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Org.Data where

import Control.Arrow (left)
import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Lazy qualified as B
import Data.Map
import Data.Map qualified as M
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as T
import Data.Void
import Org.Parser
import Org.Printer
import Org.Types
import Text.Megaparsec
import Prelude hiding (readFile)

lookupProperty :: [Property] -> Text -> Maybe Text
lookupProperty ps n = ps ^? traverse . filtered (\x -> x ^. name == n) . value

property :: Text -> Traversal' Entry Text
property n =
  entryProperties . traverse . filtered (\x -> x ^. name == n) . value

keyword :: Traversal' Entry Text
keyword f = entryKeyword . _Just . failing _OpenKeyword _ClosedKeyword %%~ f

entryId :: Traversal' Entry Text
entryId = property "ID"

readOrgFile_ :: Config -> FilePath -> Text -> Either String OrgFile
readOrgFile_ cfg path content =
  left
    errorBundlePretty
    (runReader (runParserT parseOrgFile path content) cfg)

readOrgFile :: (MonadIO m) => Config -> FilePath -> ExceptT String m OrgFile
readOrgFile cfg path = do
  content <- lift (readFile path)
  liftEither $ readOrgFile_ cfg path content

_orgFile :: Config -> FilePath -> Prism' Text OrgFile
_orgFile cfg path =
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
  Config ->
  [(FilePath, Text)] ->
  Either String OrgData
readOrgData cfg paths = OrgData . M.fromList <$> mapM go paths
  where
    go (path, content) = do
      org <- readOrgFile_ cfg path content
      pure (path, org)

_orgTime :: Prism' Text Time
_orgTime = prism' showTime (parseMaybe @Void parseTime)

createdTime :: Traversal' Entry Time
createdTime = property "CREATED" . _orgTime

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
  inheritProperties finalProperties finalEntry
  where
    finalEntry
      | has (property n) e = e & property n .~ v
      | otherwise = e & entryProperties <>~ [Property True n v]
    finalProperties =
      concatMap injectedProperty hardCodedInheritedProperties ++ ps
    injectedProperty k =
      [Property False k x | x <- maybeToList (e ^? property k)]

traverseEntries ::
  (Applicative f) =>
  [Property] ->
  (Entry -> f a) ->
  [Entry] ->
  f [a]
traverseEntries ps f = foldEntries ps (liftA2 (:) . f) (pure [])

entries :: [Property] -> Traversal' OrgFile Entry
entries ps f = fileEntries %%~ traverseEntries ps f

allEntries :: [Property] -> Traversal' OrgData Entry
allEntries ps f = orgFiles . traverse . fileEntries %%~ traverseEntries ps f

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
entriesMap :: [Property] -> OrgData -> Map Text [Entry]
entriesMap ps db =
  Prelude.foldr addEntryToMap M.empty (db ^.. allEntries ps)

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
foldAllEntries org z f = Prelude.foldr f z (org ^.. allEntries [])

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
