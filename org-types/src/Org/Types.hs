{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Org.Types where

import Control.Applicative (Const (..))
import Control.DeepSeq
import Data.Data (Data)
import Data.Function (on)
import Data.Functor.Identity (Identity (..))
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Monoid (First (..))
import Data.Time
  ( Day (ModifiedJulianDay),
    UTCTime (..),
    diffTimeToPicoseconds,
    secondsToDiffTime,
  )
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

data Config = Config
  { _startKeywords :: [String],
    _openKeywords :: [String],
    _closedKeywords :: [String],
    _keywordTransitions :: [(String, [String])],
    _homeDirectory :: Maybe FilePath,
    _checkFiles :: Bool,
    _priorities :: [String],
    _propertyColumn :: Int,
    _tagsColumn :: Int,
    _attachmentsDir :: FilePath
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, NFData)

defaultConfig :: Config
defaultConfig =
  Config
    { _startKeywords = [],
      _openKeywords = [],
      _closedKeywords = [],
      _keywordTransitions = [],
      _homeDirectory = Nothing,
      _checkFiles = True,
      _priorities = [],
      _propertyColumn = 0,
      _tagsColumn = 0,
      _attachmentsDir = ""
    }

data Loc = Loc
  { _file :: FilePath,
    _pos :: Int
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, NFData)

data Property = Property
  { _propertyLoc :: Loc,
    _inherited :: Bool,
    _name :: String,
    _value :: String
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, NFData)

data DrawerType
  = PlainDrawer String
  | BeginDrawer String
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, NFData)

data Block
  = Whitespace Loc String
  | Paragraph Loc [String]
  | Drawer Loc DrawerType [String]
  | InlineTask Loc Entry
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, NFData)

newtype Body = Body
  { _blocks :: [Block]
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, NFData)

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

emptyBody :: Body -> Bool
emptyBody = (== mempty)
{-# INLINE emptyBody #-}

newtype Tag = PlainTag String
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, NFData)

data TimeSpan
  = DaySpan
  | WeekSpan
  | MonthSpan
  | YearSpan
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
      NFData
    )

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
      NFData
    )

data TimeSuffixKind
  = TimeRepeat
  | TimeRepeatPlus
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
      NFData
    )

data TimeSuffix = TimeSuffix
  { _suffixKind :: TimeSuffixKind,
    _suffixNum :: Integer,
    _suffixSpan :: TimeSpan,
    _suffixLargerSpan :: Maybe (Integer, TimeSpan)
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, NFData)

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
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, NFData)

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
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, NFData)

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
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, NFData)

isLeadingStamp :: Stamp -> Bool
isLeadingStamp (ClosedStamp _ _) = True
isLeadingStamp (ScheduledStamp _ _) = True
isLeadingStamp (DeadlineStamp _ _) = True
isLeadingStamp _ = False

data Header = Header
  { _headerPropertiesDrawer :: [Property],
    _headerFileProperties :: [Property],
    -- _headerTags :: [Tag],
    -- _headerStamps :: [Stamp],
    _headerPreamble :: Body
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, NFData)

data Keyword
  = OpenKeyword Loc String
  | ClosedKeyword Loc String
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable, NFData)

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
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, NFData)

_LogLoc :: (Functor f) => (Loc -> f Loc) -> LogEntry -> f LogEntry
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

_LogTime :: (Applicative f) => (Time -> f Time) -> LogEntry -> f LogEntry
_LogTime f e = case e of
  LogClosing loc t mbody ->
    (\t' -> LogClosing loc t' mbody) <$> f t
  LogState loc k mk t mbody ->
    (\t' -> LogState loc k mk t' mbody) <$> f t
  LogNote loc t mbody ->
    (\t' -> LogNote loc t' mbody) <$> f t
  LogRescheduled loc t1 t2 mbody ->
    (\t2' -> LogRescheduled loc t1 t2' mbody) <$> f t2
  LogNotScheduled loc t1 t2 mbody ->
    (\t2' -> LogNotScheduled loc t1 t2' mbody) <$> f t2
  LogDeadline loc t1 t2 mbody ->
    (\t2' -> LogDeadline loc t1 t2' mbody) <$> f t2
  LogNoDeadline loc t1 t2 mbody ->
    (\t2' -> LogNoDeadline loc t1 t2' mbody) <$> f t2
  LogRefiling loc t mbody ->
    (\t' -> LogRefiling loc t' mbody) <$> f t
  LogClock loc t mbody ->
    (\t' -> LogClock loc t' mbody) <$> f t
  LogBook {} -> pure e

_LogBody :: (Applicative f) => (Body -> f Body) -> LogEntry -> f LogEntry
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
    _entryPriority :: Maybe String,
    _entryHeadline :: String,
    _entryVerb :: Maybe String,
    _entryTitle :: String,
    _entryContext :: Maybe String,
    _entryLocator :: Maybe String,
    _entryTags :: [Tag],
    _entryStamps :: [Stamp],
    _entryProperties :: [Property],
    _entryLogEntries :: [LogEntry],
    _entryBody :: Body,
    _entryItems :: [Entry]
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, NFData)

data OrgFile = OrgFile
  { _orgFilePath :: FilePath,
    _orgFileHeader :: Header,
    _orgFileEntries :: [Entry]
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, NFData)

data CollectionItem
  = OrgItem OrgFile
  | DataItem FilePath
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, NFData)

newtype Collection = Collection
  { _items :: [CollectionItem]
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, NFData)

previewIsh ::
  ((a -> Const (First a) a) -> s -> Const (First a) s) ->
  s ->
  Maybe a
previewIsh l = getFirst . getConst . l (Const . First . Just)

setIsh :: ((a -> Identity a) -> s -> Identity s) -> a -> s -> s
setIsh l x = runIdentity . l (const (Identity x))
