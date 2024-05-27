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
  deriving (Show, Eq, Generic, Data, Typeable, Plated)

makeClassy ''Config

type BasicParser = ParsecT Void Text Identity

type Parser = ParsecT Void Text (Reader Config)

data Property = Property
  { _inherited :: Bool,
    _name :: Text,
    _value :: Text
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

makeLenses ''Property

data Block
  = Whitespace Text
  | Paragraph [Text]
  | Drawer [Text]
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

makePrisms ''Block

newtype Body = Body
  { _blocks :: [Block]
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

instance Semigroup Body where
  Body [] <> ys = ys
  xs <> Body [] = xs
  Body xs <> Body (Whitespace yw : ys)
    | Whitespace xw : xs' <- reverse xs =
        Body (reverse xs' ++ Whitespace (xw <> yw) : ys)
  Body xs <> Body (Paragraph yw : ys)
    | Paragraph xw : xs' <- reverse xs =
        Body (reverse xs' ++ Paragraph (xw <> yw) : ys)
  Body xs <> Body ys = Body (xs ++ ys)

instance Monoid Body where
  mempty = Body []
  mappend = (<>)

makeClassy ''Body

emptyBody :: Body -> Bool
emptyBody = (== mempty)

data Header = Header
  { _headerPropertiesDrawer :: [Property],
    _headerFileProperties :: [Property],
    _headerPreamble :: Body
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, Plated)

makeClassy ''Header

data Keyword
  = OpenKeyword Text
  | ClosedKeyword Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

makePrisms ''Keyword

data Tag
  = SpecialTag Text
  | PlainTag Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

makePrisms ''Tag

data TimeSpan
  = DaySpan
  | WeekSpan
  | MonthSpan
  deriving
    ( Show,
      Eq,
      Ord,
      Enum,
      Bounded,
      Generic,
      Data,
      Typeable,
      Hashable,
      Plated
    )

makePrisms ''TimeSpan

data TimeKind
  = ActiveTime
  | InactiveTime
  deriving
    ( Show,
      Eq,
      Ord,
      Enum,
      Bounded,
      Generic,
      Data,
      Typeable,
      Hashable,
      Plated
    )

makePrisms ''TimeKind

data TimeSuffixKind
  = TimeRepeat
  | TimeDottedRepeat
  | TimeWithin
  deriving
    ( Show,
      Eq,
      Ord,
      Enum,
      Bounded,
      Generic,
      Data,
      Typeable,
      Hashable,
      Plated
    )

makePrisms ''TimeSuffixKind

data TimeSuffix = TimeSuffix
  { _suffixKind :: TimeSuffixKind,
    _suffixNum :: Integer,
    _suffixSpan :: TimeSpan,
    _suffixLargerSpan :: Maybe (Integer, TimeSpan)
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

makeLenses ''TimeSuffix

data Time = Time
  { _timeKind :: TimeKind,
    _timeDay :: Integer,
    _timeDayEnd :: Maybe Integer,
    _timeStart :: Maybe Integer,
    _timeEnd :: Maybe Integer,
    _timeSuffix :: Maybe TimeSuffix
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

makeClassy ''Time

data Duration = Duration
  { _hours :: Integer,
    _mins :: Integer
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

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
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

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
  = LogClosing Time (Maybe Body)
  | LogState Keyword (Maybe Keyword) Time (Maybe Body)
  | LogNote Time (Maybe Body)
  | LogRescheduled Time Time (Maybe Body)
  | LogNotScheduled Time Time (Maybe Body)
  | LogDeadline Time Time (Maybe Body)
  | LogNoDeadline Time Time (Maybe Body)
  | LogRefiling Time (Maybe Body)
  | LogClock Time (Maybe Duration)
  | LogBook [LogEntry]
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

makePrisms ''LogEntry

_LogBody :: Traversal' LogEntry Body
_LogBody f e = case e of
  LogClosing t mbody -> LogClosing t <$> traverse f mbody
  LogState k mk t mbody -> LogState k mk t <$> traverse f mbody
  LogNote t mbody -> LogNote t <$> traverse f mbody
  LogRescheduled t1 t2 mbody -> LogRescheduled t1 t2 <$> traverse f mbody
  LogNotScheduled t1 t2 mbody -> LogNotScheduled t1 t2 <$> traverse f mbody
  LogDeadline t1 t2 mbody -> LogDeadline t1 t2 <$> traverse f mbody
  LogNoDeadline t1 t2 mbody -> LogNoDeadline t1 t2 <$> traverse f mbody
  LogRefiling t mbody -> LogRefiling t <$> traverse f mbody
  LogClock _ _ -> pure e
  LogBook _ -> pure e

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
    _entryText :: Body,
    _entryItems :: [Entry]
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, Plated)

makeClassy ''Entry

data OrgFile = OrgFile
  { _filePath :: FilePath,
    _fileHeader :: Header,
    _fileEntries :: [Entry]
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, Plated)

makeClassy ''OrgFile

newtype OrgData = OrgData
  { _orgFiles :: Map FilePath OrgFile
  }
  deriving (Show, Eq, Generic, Data, Typeable, Plated)

makeClassy ''OrgData
