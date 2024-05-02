module Org.Parser where

import Text.Megaparsec

parseOrg :: Parser OrgFile
parseOrg = undefined

data Property = Property
  { propertyName :: Text,
    propertyValue :: Text
  }

parseProperties :: Parser [Property]
parseProperties = do
  ":PROPERTIES:"
  ":END:"

data OrgHeader = OrgHeader
  { headerPropertiesDrawer :: [Property],
    headerFileProperties :: [Property],
    headerPreamble :: Maybe Text
  }

parseHeader :: Parser OrgHeader
parseHeader = undefined

data OrgEntry = OrgEntry
  { entryKeyword :: Maybe Text,
    entryPriority :: Maybe Text,
    entryTitle :: Text,
    entryLocation :: Maybe Text,
    entryTags :: [Text],
    entryClosed :: Maybe UTCTime,
    entryScheduled :: Maybe UTCTime,
    entryDeadline :: Maybe UTCTime,
    entryDated :: Maybe (UTCTime, Maybe UTCTime),
    entryProperties :: [Property],
    entryText :: Maybe Text
  }

parseEntry :: Parser OrgEntry
parseEntry = undefined
