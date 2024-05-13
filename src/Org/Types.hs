{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Org.Types where

import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader
import Data.Data
import Data.Hashable
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
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
  { _inherited :: Bool,
    _name :: Text,
    _value :: Text
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable)

makeLenses ''Property

data Header = Header
  { _headerPropertiesDrawer :: [Property],
    _headerFileProperties :: [Property],
    _headerPreamble :: [Text]
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable)

makeClassy ''Header

data Keyword
  = OpenKeyword Text
  | ClosedKeyword Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable)

makePrisms ''Keyword

data Tag
  = SpecialTag Text
  | PlainTag Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable)

makePrisms ''Tag

data TimeSpan
  = DaySpan
  | WeekSpan
  | MonthSpan
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, Data, Typeable, Hashable)

makePrisms ''TimeSpan

data TimeKind
  = ActiveTime
  | InactiveTime
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, Data, Typeable, Hashable)

makePrisms ''TimeKind

data TimeSuffixKind
  = TimeRepeat
  | TimeDottedRepeat
  | TimeWithin
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, Data, Typeable, Hashable)

makePrisms ''TimeSuffixKind

data TimeSuffix = TimeSuffix
  { _suffixKind :: TimeSuffixKind,
    _suffixNum :: Integer,
    _suffixSpan :: TimeSpan,
    _suffixLargerSpan :: Maybe (Integer, TimeSpan)
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable)

makeLenses ''TimeSuffix

data Time = Time
  { _timeKind :: TimeKind,
    _timeDay :: Integer,
    _timeDayEnd :: Maybe Integer,
    _timeStart :: Maybe Integer,
    _timeEnd :: Maybe Integer,
    _timeSuffix :: Maybe TimeSuffix
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable)

makeClassy ''Time

data Duration = Duration
  { _hours :: Integer,
    _mins :: Integer
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable)

makeClassy ''Duration

_duration :: Traversal' Time Duration
_duration f tm@Time {..} = do
  case mdur of
    Nothing -> pure tm
    Just dur -> tm <$ f dur
  where
    mdur = do
      let startDay = _timeDay
      startTime <- _timeStart
      endDay <- _timeDayEnd
      endTime <- _timeEnd
      when (startDay > endDay) $
        error $
          "Invalid time (end before start): " ++ show tm
      let days = endDay - startDay
          secs = endTime - startTime
      if days > 0
        then
          if secs < 0
            then undefined
            else undefined
        else pure $ Duration (secs `div` 60) (secs `mod` 60)

data Stamp
  = ClosedStamp Time
  | ScheduledStamp Time
  | DeadlineStamp Time
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable)

makePrisms ''Stamp

timeStartToUTCTime :: Time -> UTCTime
timeStartToUTCTime Time {..} =
  UTCTime
    (ModifiedJulianDay _timeDay)
    (secondsToDiffTime $ fromMaybe 0 _timeStart)

timeEndToUTCTime :: Time -> Maybe UTCTime
timeEndToUTCTime Time {..} = do
  day <- _timeDayEnd
  pure $
    UTCTime
      (ModifiedJulianDay day)
      (secondsToDiffTime $ fromMaybe 0 _timeEnd)

data LogEntry
  = LogState Keyword (Maybe Keyword) Time [Text]
  | LogNote Time [Text]
  | LogBook [(Time, Maybe Duration)]
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable)

makePrisms ''LogEntry

data Entry = Entry
  { _entryFile :: FilePath,
    _entryLine :: Int,
    _entryColumn :: Int,
    _entryDepth :: Int,
    _entryKeyword :: Maybe Keyword,
    _entryPriority :: Maybe Text,
    _entryHeadline :: Text,
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
  deriving (Show, Eq, Generic, Data, Typeable, Hashable)

makeClassy ''Entry

data OrgFile = OrgFile
  { _fileHeader :: Header,
    _fileEntries :: [Entry]
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable)

makeClassy ''OrgFile

newtype OrgData = OrgData
  { _orgFiles :: Map FilePath OrgFile
  }
  deriving (Show, Eq, Generic, Data, Typeable)

makeClassy ''OrgData
