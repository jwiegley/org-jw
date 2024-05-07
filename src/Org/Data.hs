{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Org.Data where

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
lookupProperty [] _ = Nothing
lookupProperty (Property name value : xs) key
  | name == key = Just value
  | otherwise = lookupProperty xs key

orgEntryId :: OrgEntry -> Maybe Text
orgEntryId entry = lookupProperty (entryProperties entry) "ID"

orgEntryCreatedTime :: OrgEntry -> Maybe UTCTime
orgEntryCreatedTime entry = do
  created <- lookupProperty (entryProperties entry) "CREATED"
  time <- parseMaybe @Void parseOrgTimeSingle created
  pure $ orgTimeStartToUTCTime time

orgFoldEntries :: (OrgEntry -> a -> a) -> a -> [OrgEntry] -> a
orgFoldEntries _ z [] = z
orgFoldEntries f z (x : xs) = f x (orgFoldEntries f z (entryItems x ++ xs))

orgEntriesMap :: OrgFile -> ([String], Map Text OrgEntry)
orgEntriesMap OrgFile {..} =
  orgFoldEntries f ([], M.empty) fileEntries
  where
    f entry (errs, m) =
      case orgEntryId entry of
        Nothing -> (errs, m)
        Just ident ->
          case M.lookup ident m of
            Nothing -> (errs, M.insert ident entry m)
            Just found ->
              ( errs
                  ++ [ T.unpack $
                         "Identifier already exists: entry =\n"
                           <> T.concat (summarizeEntry entry)
                           <> "\nfound =\n"
                           <> T.concat (summarizeEntry found)
                     ],
                m
              )
