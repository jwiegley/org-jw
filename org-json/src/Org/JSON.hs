{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Org.JSON (orgFileToJSON, orgFileToJSONFile, orgFileFromJSONFile) where

import Data.Aeson
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Org.Types
import Prelude hiding (readFile)

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x : xs) = (toLower x : xs)

instance FromJSON Config where
  parseJSON = genericParseJSON JSON.defaultOptions

instance ToJSON Config where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Time where
  parseJSON =
    genericParseJSON
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 5 -- _time
        }

instance ToJSON Time where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 5 -- _time
        }

instance FromJSON TimeKind where
  parseJSON = genericParseJSON JSON.defaultOptions

instance ToJSON TimeKind where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance FromJSON TimeSuffix where
  parseJSON =
    genericParseJSON
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 7 -- _suffix
        }

instance ToJSON TimeSuffix where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 7 -- _suffix
        }

instance FromJSON TimeSuffixKind where
  parseJSON = genericParseJSON JSON.defaultOptions

instance ToJSON TimeSuffixKind where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance FromJSON TimeSpan where
  parseJSON = genericParseJSON JSON.defaultOptions

instance ToJSON TimeSpan where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance FromJSON Stamp where
  parseJSON = genericParseJSON JSON.defaultOptions

instance ToJSON Stamp where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance FromJSON Duration where
  parseJSON =
    genericParseJSON
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 1 -- _
        }

instance ToJSON Duration where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 1 -- _
        }

instance FromJSON Property where
  parseJSON =
    genericParseJSON
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 1 -- _
        }

instance ToJSON Property where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 1 -- _
        }

instance FromJSON Tag where
  parseJSON = genericParseJSON JSON.defaultOptions

instance ToJSON Tag where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance FromJSON Keyword where
  parseJSON = genericParseJSON JSON.defaultOptions

instance ToJSON Keyword where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance FromJSON Loc where
  parseJSON =
    genericParseJSON
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 1 -- _
        }

instance ToJSON Loc where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 1 -- _
        }

instance FromJSON LogEntry where
  parseJSON = genericParseJSON JSON.defaultOptions

instance ToJSON LogEntry where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance FromJSON DrawerType where
  parseJSON = genericParseJSON JSON.defaultOptions

instance ToJSON DrawerType where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance FromJSON Block where
  parseJSON = genericParseJSON JSON.defaultOptions

instance ToJSON Block where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions

instance FromJSON Body where
  parseJSON =
    genericParseJSON
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 1 -- _
        }

instance ToJSON Body where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 1 -- _
        }

instance FromJSON Entry where
  parseJSON =
    genericParseJSON
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 6 -- _entry
        }

instance ToJSON Entry where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 6 -- _entry
        }

instance FromJSON Header where
  parseJSON =
    genericParseJSON
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 7 -- _header
        }

instance ToJSON Header where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 7 -- _header
        }

instance FromJSON OrgFile where
  parseJSON =
    genericParseJSON
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 8 -- _orgFile
        }

instance ToJSON OrgFile where
  toEncoding =
    genericToEncoding
      JSON.defaultOptions
        { fieldLabelModifier = lowerFirst . drop 8 -- _orgFile
        }

orgFileToJSON :: OrgFile -> ByteString
orgFileToJSON = encode

orgFileToJSONFile :: FilePath -> OrgFile -> IO ()
orgFileToJSONFile = encodeFile

orgFileFromJSONFile :: FilePath -> IO (Either String OrgFile)
orgFileFromJSONFile = eitherDecodeFileStrict
