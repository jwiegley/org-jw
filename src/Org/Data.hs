{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Org.Data where

import Control.Arrow (left)
import Control.Lens
import Control.Monad.Reader
import Data.Map
import Data.Map qualified as M
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

foldEntries :: (Entry -> b -> b) -> b -> [Entry] -> b
foldEntries _ z [] = z
foldEntries f z (x : xs) = f x (foldEntries f z (x ^. entryItems ++ xs))

traverseEntries :: (Applicative f) => (Entry -> f a) -> [Entry] -> f [a]
traverseEntries f = foldEntries (liftA2 (:) . f) (pure [])

entries :: Traversal' OrgFile Entry
entries f = fileEntries %%~ traverseEntries f

entriesMap :: OrgFile -> ([String], Map Text Entry)
entriesMap OrgFile {..} =
  foldEntries f ([], M.empty) _fileEntries
  where
    f e (errs, m) =
      case e ^? entryId of
        Nothing -> (errs, m)
        Just ident ->
          case m ^. at ident of
            Nothing ->
              (errs, m & at ident ?~ e)
            Just found ->
              ( errs
                  ++ [ T.unpack $
                         "Identifier already exists: entry =\n"
                           <> T.concat (summarizeEntry e)
                           <> "\nfound =\n"
                           <> T.concat (summarizeEntry found)
                     ],
                m
              )
