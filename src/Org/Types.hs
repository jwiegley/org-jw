{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Org.Types where

import Control.Lens
import Control.Monad.Reader
import Data.Data
import Data.Function (on)
import Data.Hashable
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Void
import GHC.Generics
import Text.Megaparsec hiding (many, some)

data Config = Config
  { _openKeywords :: [Text],
    _closedKeywords :: [Text],
    _keywordTransitions :: [(Text, [Text])],
    _priorities :: [Text],
    _propertyColumn :: Int,
    _tagsColumn :: Int
  }
  deriving (Show, Eq, Generic, Data, Typeable, Plated)

makeClassy ''Config

type BasicParser = ParsecT Void Text Identity

type Parser = ParsecT Void Text (Reader Config)

data Loc = Loc
  { _file :: FilePath,
    _line :: Int,
    _column :: Int
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

makeLenses ''Loc

sourcePosToLoc :: SourcePos -> Loc
sourcePosToLoc SourcePos {..} =
  Loc
    { _file = sourceName,
      _line = unPos sourceLine,
      _column = unPos sourceColumn
    }

getLoc :: (TraversableStream s, MonadParsec e s m) => m Loc
getLoc = sourcePosToLoc <$> getSourcePos

data Property = Property
  { _propertyLoc :: Loc,
    _inherited :: Bool,
    _name :: Text,
    _value :: Text
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

makeLenses ''Property

lookupProperty' :: Text -> Traversal' [Property] Property
lookupProperty' n =
  traverse . filtered (\x -> T.toLower (x ^. name) == T.toLower n)

lookupProperty :: Text -> Traversal' [Property] Text
lookupProperty n = lookupProperty' n . value

data Block
  = Whitespace Loc Text
  | Paragraph Loc [Text]
  | Drawer Loc [Text]
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

makePrisms ''Block

newtype Body = Body
  { _blocks :: [Block]
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

instance Semigroup Body where
  Body [] <> ys = ys
  xs <> Body [] = xs
  Body xs <> Body (Whitespace _yloc yw : ys)
    | Whitespace xloc xw : xs' <- reverse xs =
        Body (reverse xs' ++ Whitespace xloc (xw <> yw) : ys)
  Body xs <> Body (Paragraph _yloc yw : ys)
    | Paragraph xloc xw : xs' <- reverse xs =
        Body (reverse xs' ++ Paragraph xloc (xw <> yw) : ys)
  Body xs <> Body ys = Body (xs ++ ys)

instance Monoid Body where
  mempty = Body []
  mappend = (<>)

makeClassy ''Body

emptyBody :: Body -> Bool
emptyBody = (== mempty)

newtype Tag = PlainTag Text
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
    -- | This is a quantity of minutes into the day.
    _timeStart :: Maybe Integer,
    -- | This is a quantity of minutes into the day.
    _timeEnd :: Maybe Integer,
    _timeSuffix :: Maybe TimeSuffix
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, Plated)

makeClassy ''Time

timeStartToUTCTime :: Time -> UTCTime
timeStartToUTCTime Time {..} =
  UTCTime
    (ModifiedJulianDay _timeDay)
    (secondsToDiffTime $ fromMaybe 0 _timeStart * 60)

timeEndToUTCTime :: Time -> Maybe UTCTime
timeEndToUTCTime Time {..} = do
  day <- _timeDayEnd
  pure $
    UTCTime
      (ModifiedJulianDay day)
      (secondsToDiffTime (fromMaybe 0 _timeEnd * 60))

utcTimeToTime :: TimeKind -> UTCTime -> Time
utcTimeToTime kind (UTCTime (ModifiedJulianDay day) diff) =
  Time
    { _timeKind = kind,
      _timeDay = day,
      _timeDayEnd = Nothing,
      _timeStart =
        Just
          ( diffTimeToPicoseconds diff
              `div` ((10 :: Integer) ^ (12 :: Integer))
              `div` 60
          ),
      _timeEnd = Nothing,
      _timeSuffix = Nothing
    }

instance Ord Time where
  compare = compare `on` timeStartToUTCTime

data Duration = Duration
  { _hours :: Integer,
    _mins :: Integer
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

makeClassy ''Duration

{-
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
-}

data Stamp
  = ClosedStamp Loc Time
  | ScheduledStamp Loc Time
  | DeadlineStamp Loc Time
  | ActiveStamp Loc Time
  | CreatedStamp Loc Time
  | EditedStamp Loc Time
  | DateStamp Loc Time
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

makePrisms ''Stamp

isLeadingStamp :: Stamp -> Bool
isLeadingStamp (ClosedStamp _ _) = True
isLeadingStamp (ScheduledStamp _ _) = True
isLeadingStamp (DeadlineStamp _ _) = True
isLeadingStamp _ = False

data Header = Header
  { _headerPropertiesDrawer :: [Property],
    _headerFileProperties :: [Property],
    _headerTags :: [Tag],
    _headerStamps :: [Stamp],
    _headerPreamble :: Body
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, Plated)

makeClassy ''Header

data Keyword
  = OpenKeyword Loc Text
  | ClosedKeyword Loc Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

makePrisms ''Keyword

data LogEntry
  = LogClosing Loc Time (Maybe Body)
  | LogState Loc Keyword (Maybe Keyword) Time (Maybe Body)
  | LogNote Loc Time (Maybe Body)
  | LogRescheduled Loc Time Time (Maybe Body)
  | LogNotScheduled Loc Time Time (Maybe Body)
  | LogDeadline Loc Time Time (Maybe Body)
  | LogNoDeadline Loc Time Time (Maybe Body)
  | LogRefiling Loc Time (Maybe Body)
  | LogClock Loc Time (Maybe Duration)
  | LogBook Loc [LogEntry]
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, Plated)

makePrisms ''LogEntry

_LogLoc :: Lens' LogEntry Loc
_LogLoc f e = case e of
  LogClosing loc t mbody ->
    (\loc' -> LogClosing loc' t mbody) <$> f loc
  LogState loc k mk t mbody ->
    (\loc' -> LogState loc' k mk t mbody) <$> f loc
  LogNote loc t mbody ->
    (\loc' -> LogNote loc' t mbody) <$> f loc
  LogRescheduled loc t1 t2 mbody ->
    (\loc' -> LogRescheduled loc' t1 t2 mbody) <$> f loc
  LogNotScheduled loc t1 t2 mbody ->
    (\loc' -> LogNotScheduled loc' t1 t2 mbody) <$> f loc
  LogDeadline loc t1 t2 mbody ->
    (\loc' -> LogDeadline loc' t1 t2 mbody) <$> f loc
  LogNoDeadline loc t1 t2 mbody ->
    (\loc' -> LogNoDeadline loc' t1 t2 mbody) <$> f loc
  LogRefiling loc t mbody ->
    (\loc' -> LogRefiling loc' t mbody) <$> f loc
  LogClock loc t mbody ->
    (\loc' -> LogClock loc' t mbody) <$> f loc
  LogBook loc es ->
    (`LogBook` es) <$> f loc

_LogBody :: Traversal' LogEntry Body
_LogBody f e = case e of
  LogClosing loc t mbody ->
    LogClosing loc t <$> traverse f mbody
  LogState loc k mk t mbody ->
    LogState loc k mk t <$> traverse f mbody
  LogNote loc t mbody ->
    LogNote loc t <$> traverse f mbody
  LogRescheduled loc t1 t2 mbody ->
    LogRescheduled loc t1 t2 <$> traverse f mbody
  LogNotScheduled loc t1 t2 mbody ->
    LogNotScheduled loc t1 t2 <$> traverse f mbody
  LogDeadline loc t1 t2 mbody ->
    LogDeadline loc t1 t2 <$> traverse f mbody
  LogNoDeadline loc t1 t2 mbody ->
    LogNoDeadline loc t1 t2 <$> traverse f mbody
  LogRefiling loc t mbody ->
    LogRefiling loc t <$> traverse f mbody
  LogClock {} -> pure e
  LogBook {} -> pure e

data Entry = Entry
  { _entryLoc :: Loc,
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
  { _orgFiles :: [OrgFile]
  }
  deriving (Show, Eq, Generic, Data, Typeable, Plated)

makeClassy ''OrgData
