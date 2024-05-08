{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Org.Data where

import Control.Arrow (left)
import Control.Lens
import Control.Monad.Reader
import Data.Map
import Data.Map qualified as M
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Org.Parser
import Org.Printer
import Org.Types
import Text.Megaparsec

lookupProperty :: [Property] -> Text -> Maybe Text
lookupProperty ps n = ps ^? traverse . filtered (\x -> x ^. name == n) . value

property :: Text -> Traversal' Entry Text
property n =
  entryProperties . traverse . filtered (\x -> x ^. name == n) . value

entryId :: Traversal' Entry Text
entryId = property "ID"

_orgFile :: FilePath -> Config -> Prism' Text OrgFile
_orgFile path cfg =
  prism
    (T.concat . showOrgFile (cfg ^. propertyColumn) (cfg ^. tagsColumn))
    ( \content ->
        left
          (T.pack . errorBundlePretty)
          (runReader (runParserT parseOrgFile path content) cfg)
    )

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

entries :: [Property] -> Traversal' OrgData Entry
entries ps f = orgFiles . traverse . fileEntries %%~ traverseEntries ps f

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
  Prelude.foldr addEntryToMap M.empty (db ^.. entries ps)

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
