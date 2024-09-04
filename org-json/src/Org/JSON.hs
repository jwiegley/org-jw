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
import Data.Char (toLower)
import Org.Types
import Prelude hiding (readFile)

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x : xs) = (toLower x : xs)

instance ToJSON Time where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 5 -- _time
        }

instance ToJSON TimeKind where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance ToJSON TimeSuffix where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 7 -- _suffix
        }

instance ToJSON TimeSuffixKind where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance ToJSON TimeSpan where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance ToJSON Stamp where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance ToJSON Duration where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 1 -- _
        }

instance ToJSON Property where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 1 -- _
        }

instance ToJSON Tag where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance ToJSON Keyword where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance ToJSON Loc where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 1 -- _
        }

instance ToJSON LogEntry where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance ToJSON Block where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance ToJSON Body where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 1 -- _
        }

instance ToJSON Entry where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 6 -- _entry
        }

instance ToJSON Header where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 7 -- _header
        }

instance ToJSON OrgFile where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 8 -- _orgFile
        }

orgFileToJSON :: FilePath -> OrgFile -> IO ()
orgFileToJSON = encodeFile
