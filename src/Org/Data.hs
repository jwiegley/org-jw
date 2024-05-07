{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Org.Data where

import Control.Lens
import Data.Map
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Void
import Org.Parser
import Org.Printer
import Org.Types
import Text.Megaparsec

lookupProperty :: [Property] -> Text -> Maybe Text
lookupProperty ps n = ps ^? traverse . filtered (\x -> x ^. name == n) . value

entryId :: Entry -> Maybe Text
entryId e = lookupProperty (e ^. entryProperties) "ID"

entryCreatedTime :: Entry -> Maybe UTCTime
entryCreatedTime e = do
  created <- lookupProperty (e ^. entryProperties) "CREATED"
  tm <- parseMaybe @Void parseOrgTimeSingle created
  pure $ orgTimeStartToUTCTime tm

foldEntries :: (Entry -> a -> a) -> a -> [Entry] -> a
foldEntries _ z [] = z
foldEntries f z (x : xs) = f x (foldEntries f z (_entryItems x ++ xs))

entriesMap :: OrgFile -> ([String], Map Text Entry)
entriesMap OrgFile {..} =
  foldEntries f ([], M.empty) _fileEntries
  where
    f e (errs, m) =
      case entryId e of
        Nothing -> (errs, m)
        Just ident ->
          case m ^. at ident of
            Nothing -> (errs, m & at ident ?~ e)
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
