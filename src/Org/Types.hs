{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Org.Types where

import Control.Monad.Reader
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time
import Data.Void
import Text.Megaparsec hiding (many, some)

data OrgConfig = OrgConfig
  { openKeywords :: [Text],
    closedKeywords :: [Text],
    priorities :: [Text],
    specialTags :: [Text],
    propertyColumn :: Int,
    tagsColumn :: Int
  }
  deriving (Show)

type BasicParser = ParsecT Void Text Identity

type Parser = ParsecT Void Text (Reader OrgConfig)

data OrgFile = OrgFile
  { fileHeader :: OrgHeader,
    fileEntries :: [OrgEntry]
  }
  deriving (Show)

data Property = Property
  { propertyName :: Text,
    propertyValue :: Text
  }
  deriving (Show)

data OrgHeader = OrgHeader
  { headerPropertiesDrawer :: [Property],
    headerFileProperties :: [Property],
    headerPreamble :: [Text]
  }
  deriving (Show)

data OrgKeyword
  = OpenKeyword Text
  | ClosedKeyword Text
  deriving (Show, Eq, Ord)

data OrgTag
  = OrgSpecialTag Text
  | OrgPlainTag Text
  deriving (Show, Eq, Ord)

data OrgLogEntry
  = OrgLogStateChange OrgKeyword OrgKeyword OrgTime [Text]
  | OrgLogNote OrgTime [Text]
  deriving (Show, Eq, Ord)

data OrgEntry = OrgEntry
  { entryDepth :: Int,
    entryKeyword :: Maybe OrgKeyword,
    entryPriority :: Maybe Text,
    entryTitle :: Text,
    entryContext :: Maybe Text,
    entryLocator :: Maybe Text,
    entryTags :: [OrgTag],
    entryStamps :: [OrgStamp],
    entryProperties :: [Property],
    entryLogEntries :: [OrgLogEntry],
    entryText :: [Text],
    entryItems :: [OrgEntry]
  }
  deriving (Show)

data OrgStampKind
  = ClosedStamp
  | ScheduledStamp
  | DeadlineStamp
  deriving (Show, Eq, Ord, Enum, Bounded)

data OrgStamp = OrgStamp
  { orgStampKind :: OrgStampKind,
    orgStampTime :: OrgTime
  }
  deriving (Show, Eq, Ord)

data OrgTimeKind
  = ActiveTime
  | InactiveTime
  deriving (Show, Eq, Ord, Enum, Bounded)

data OrgTime = OrgTime
  { orgTimeKind :: OrgTimeKind,
    orgTimeDay :: Day,
    orgTimeDayEnd :: Maybe Day,
    orgTimeStart :: Maybe DiffTime,
    orgTimeEnd :: Maybe DiffTime,
    orgTimeSuffix :: Maybe OrgTimeSuffix
  }
  deriving (Show, Eq, Ord)

data OrgTimeSpan
  = OrgDaySpan
  | OrgWeekSpan
  | OrgMonthSpan
  deriving (Show, Eq, Ord, Enum, Bounded)

data OrgTimeSuffixKind
  = OrgTimeRepeat
  | OrgTimeDottedRepeat
  | OrgTimeWithin
  deriving (Show, Eq, Ord, Enum, Bounded)

data OrgTimeSuffix = OrgTimeSuffix
  { orgSuffixKind :: OrgTimeSuffixKind,
    orgSuffixNum :: Int,
    orgSuffixSpan :: OrgTimeSpan
  }
  deriving (Show, Eq, Ord)

orgTimeStartToUTCTime :: OrgTime -> UTCTime
orgTimeStartToUTCTime OrgTime {..} =
  UTCTime orgTimeDay (fromMaybe (secondsToDiffTime 0) orgTimeStart)

orgTimeEndToUTCTime :: OrgTime -> Maybe UTCTime
orgTimeEndToUTCTime OrgTime {..} =
  UTCTime
    <$> orgTimeDayEnd
    <*> Just (fromMaybe (secondsToDiffTime 0) orgTimeEnd)
