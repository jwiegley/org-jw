{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Org.Types where

import Control.Lens
import Control.Monad.Reader
import Data.Data
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time
import Data.Void
import GHC.Generics
import Text.Megaparsec hiding (many, some)

data Config = Config
  { _openKeywords :: [Text],
    _closedKeywords :: [Text],
    _priorities :: [Text],
    _specialTags :: [Text],
    _propertyColumn :: Int,
    _tagsColumn :: Int
  }
  deriving (Show, Eq, Generic, Data, Typeable)

makeClassy ''Config

type BasicParser = ParsecT Void Text Identity

type Parser = ParsecT Void Text (Reader Config)

data Property = Property
  { _name :: Text,
    _value :: Text
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

makeClassy ''Property

data Header = Header
  { _headerPropertiesDrawer :: [Property],
    _headerFileProperties :: [Property],
    _headerPreamble :: [Text]
  }
  deriving (Show, Eq, Generic, Data, Typeable)

makeClassy ''Header

data Keyword
  = OpenKeyword Text
  | ClosedKeyword Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

makePrisms ''Keyword

data Tag
  = SpecialTag Text
  | PlainTag Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

makePrisms ''Tag

data TimeSpan
  = DaySpan
  | WeekSpan
  | MonthSpan
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, Data, Typeable)

makePrisms ''TimeSpan

data TimeKind
  = ActiveTime
  | InactiveTime
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, Data, Typeable)

makePrisms ''TimeKind

data TimeSuffixKind
  = TimeRepeat
  | TimeDottedRepeat
  | TimeWithin
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, Data, Typeable)

makePrisms ''TimeSuffixKind

data TimeSuffix = TimeSuffix
  { _suffixKind :: TimeSuffixKind,
    _suffixNum :: Int,
    _suffixSpan :: TimeSpan
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

makeLenses ''TimeSuffix

data Time = Time
  { _timeKind :: TimeKind,
    _timeDay :: Day,
    _timeDayEnd :: Maybe Day,
    _timeStart :: Maybe DiffTime,
    _timeEnd :: Maybe DiffTime,
    _timeSuffix :: Maybe TimeSuffix
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

makeClassy ''Time

data StampKind
  = ClosedStamp
  | ScheduledStamp
  | DeadlineStamp
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, Data, Typeable)

makePrisms ''StampKind

data Stamp = Stamp
  { orgStampKind :: StampKind,
    orgStampTime :: Time
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

makeClassy ''Stamp

orgTimeStartToUTCTime :: Time -> UTCTime
orgTimeStartToUTCTime Time {..} =
  UTCTime _timeDay (fromMaybe (secondsToDiffTime 0) _timeStart)

orgTimeEndToUTCTime :: Time -> Maybe UTCTime
orgTimeEndToUTCTime Time {..} =
  UTCTime
    <$> _timeDayEnd
    <*> Just (fromMaybe (secondsToDiffTime 0) _timeEnd)

data LogEntry
  = LogStateChange Keyword Keyword Time [Text]
  | LogNote Time [Text]
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

makePrisms ''LogEntry

data Entry = Entry
  { _entryPos :: SourcePos,
    _entryDepth :: Int,
    _entryKeyword :: Maybe Keyword,
    _entryPriority :: Maybe Text,
    _entryTitle :: Text,
    _entryContext :: Maybe Text,
    _entryLocator :: Maybe Text,
    _entryTags :: [Tag],
    _entryStamps :: [Stamp],
    _entryProperties :: [Property],
    _entryLogEntries :: [LogEntry],
    _entryText :: [Text],
    _entryItems :: [Entry]
  }
  deriving (Show, Eq, Generic, Data, Typeable)

makeClassy ''Entry

data OrgFile = OrgFile
  { _fileHeader :: Header,
    _fileEntries :: [Entry]
  }
  deriving (Show, Eq, Generic, Data, Typeable)

makeClassy ''OrgFile
