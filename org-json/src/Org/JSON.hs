{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
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
{-# OPTIONS_GHC -Wno-orphans #-}

module Org.JSON where

import Data.Aeson
import Data.Aeson qualified as JSON
import Org.Types
import Prelude hiding (readFile)

instance ToJSON Time where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Time

instance ToJSON TimeKind where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON TimeKind

instance ToJSON TimeSuffix where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON TimeSuffix

instance ToJSON TimeSuffixKind where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON TimeSuffixKind

instance ToJSON TimeSpan where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON TimeSpan

instance ToJSON Stamp where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Stamp

instance ToJSON Duration where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Duration

instance ToJSON Property where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Property

instance ToJSON Tag where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Tag

instance ToJSON Keyword where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Keyword

instance ToJSON Loc where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Loc

instance ToJSON LogEntry where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON LogEntry

instance ToJSON Block where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Block

instance ToJSON Body where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Body

instance ToJSON Entry where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Entry

instance ToJSON Header where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Header

instance ToJSON OrgFile where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON OrgFile

orgFileFromJSON :: FilePath -> IO (Maybe OrgFile)
orgFileFromJSON = decodeFileStrict

orgFileToJSON :: FilePath -> OrgFile -> IO ()
orgFileToJSON = encodeFile
