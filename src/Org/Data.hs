{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Org.Data where

import Data.Text (Text)
-- import Data.Text qualified as T
import Data.Time
import Data.Void
import Org.Parser
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
