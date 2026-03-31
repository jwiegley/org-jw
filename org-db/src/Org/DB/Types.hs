{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Org.DB.Types (
  -- * Database Configuration
  DBConfig (..),

  -- * Database Handle (record-of-functions)
  DBHandle (..),

  -- * Abstract SQL Values
  SqlValue (..),

  -- * Row Types
  FileRow (..),
  FilePropertyRow (..),
  EntryRow (..),
  EntryTagRow (..),
  EntryPropertyRow (..),
  EntryStampRow (..),
  LogEntryRow (..),
  BodyBlockRow (..),
  RelationshipRow (..),
  CategoryRow (..),
  LinkRow (..),

  -- * FromRow class
  FromRow (..),

  -- * SqlValue extraction helpers
  extractText,
  extractInt,
  extractInt64,
  extractDouble,
  extractBool,
  extractDay,
  extractUTCTime,
  extractByteString,
  extractMaybe,
)
where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Foldable (asum)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Text (Text, unpack)
import Data.Time (Day, UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | PostgreSQL connection configuration.
newtype DBConfig = DBConfig {pgConnString :: ByteString}
  deriving (Show, Eq, Generic, Data, Typeable)

-- | Abstract SQL value type, backend-agnostic.
data SqlValue
  = SqlText Text
  | SqlInt Int64
  | SqlDouble Double
  | SqlBool Bool
  | SqlBlob ByteString
  | SqlUTCTime UTCTime
  | SqlDay Day
  | SqlNull
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

-- | Row parser: convert a list of 'SqlValue' into a typed row.
class FromRow a where
  fromRow :: [SqlValue] -> Either String a

-- | Raw passthrough: a list of SqlValue is already a valid row.
instance FromRow [SqlValue] where
  fromRow = Right

-- | Database handle using the record-of-functions pattern.
data DBHandle = DBHandle
  { dbExecute_ :: Text -> [SqlValue] -> IO ()
  , dbExecute :: Text -> [SqlValue] -> IO Int64
  , dbQuery :: forall r. (FromRow r) => Text -> [SqlValue] -> IO [r]
  , dbQueryOne :: forall r. (FromRow r) => Text -> [SqlValue] -> IO (Maybe r)
  , dbTransaction :: forall a. IO a -> IO a
  , dbClose :: IO ()
  }

------------------------------------------------------------------------
-- Row Types (aligned with PRD 02 PostgreSQL schema)
------------------------------------------------------------------------

data FileRow = FileRow
  { frId :: Text
  , frPath :: Text
  , frTitle :: Maybe Text
  , frPreamble :: Maybe Text
  , frHash :: Maybe Text
  , frModTime :: Maybe UTCTime
  , frCreatedTime :: Maybe UTCTime
  , frCreatedAt :: UTCTime
  , frUpdatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

data FilePropertyRow = FilePropertyRow
  { fprFileId :: Text
  , fprName :: Text
  , fprValue :: Text
  , fprSource :: Text
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData, Hashable)

data EntryRow = EntryRow
  { erId :: Text
  , erFileId :: Text
  , erParentId :: Maybe Text
  , erDepth :: Int
  , erPosition :: Int
  , erByteOffset :: Int
  , erKeywordType :: Maybe Text
  , erKeywordValue :: Maybe Text
  , erPriority :: Maybe Text
  , erHeadline :: Text
  , erTitle :: Text
  , erVerb :: Maybe Text
  , erContext :: Maybe Text
  , erLocator :: Maybe Text
  , erHash :: Maybe Text
  , erModTime :: Maybe UTCTime
  , erCreatedTime :: Maybe UTCTime
  , erPath :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

data EntryTagRow = EntryTagRow
  { etrEntryId :: Text
  , etrTag :: Text
  , etrIsInherited :: Bool
  , etrSourceId :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData, Hashable)

data EntryPropertyRow = EntryPropertyRow
  { eprEntryId :: Text
  , eprName :: Text
  , eprValue :: Text
  , eprIsInherited :: Bool
  , eprByteOffset :: Maybe Int
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData, Hashable)

data EntryStampRow = EntryStampRow
  { esrId :: Int64
  , esrEntryId :: Text
  , esrByteOffset :: Maybe Int
  , esrStampType :: Text
  , esrTimeKind :: Text
  , esrDay :: Int
  , esrDayEnd :: Maybe Int
  , esrTimeStart :: Maybe Int
  , esrTimeEnd :: Maybe Int
  , esrSuffixKind :: Maybe Text
  , esrSuffixNum :: Maybe Int
  , esrSuffixSpan :: Maybe Text
  , esrSuffixLargerNum :: Maybe Int
  , esrSuffixLargerSpan :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

data LogEntryRow = LogEntryRow
  { lerId :: Int64
  , lerEntryId :: Text
  , lerPosition :: Int
  , lerByteOffset :: Maybe Int
  , lerLogType :: Text
  , lerTimeDay :: Maybe Int
  , lerTimeStart :: Maybe Int
  , lerTimeEnd :: Maybe Int
  , lerTimeKind :: Maybe Text
  , lerFromKeyword :: Maybe Text
  , lerFromKeywordType :: Maybe Text
  , lerToKeyword :: Maybe Text
  , lerToKeywordType :: Maybe Text
  , lerOrigTimeDay :: Maybe Int
  , lerOrigTimeDayEnd :: Maybe Int
  , lerOrigTimeStart :: Maybe Int
  , lerOrigTimeEnd :: Maybe Int
  , lerOrigTimeKind :: Maybe Text
  , lerOrigSuffixKind :: Maybe Text
  , lerOrigSuffixNum :: Maybe Int
  , lerOrigSuffixSpan :: Maybe Text
  , lerDurationHours :: Maybe Int
  , lerDurationMins :: Maybe Int
  , lerBodyText :: Maybe Text
  , lerLogbookId :: Maybe Int64
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

data BodyBlockRow = BodyBlockRow
  { bbrId :: Int64
  , bbrEntryId :: Text
  , bbrPosition :: Int
  , bbrByteOffset :: Maybe Int
  , bbrBlockType :: Text
  , bbrContent :: Maybe Text
  , bbrDrawerType :: Maybe Text
  , bbrDrawerName :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData, Hashable)

data RelationshipRow = RelationshipRow
  { rrSourceEntryId :: Text
  , rrTargetEntryId :: Text
  , rrRelationshipType :: Text
  , rrContext :: Maybe Text
  , rrCreatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

data CategoryRow = CategoryRow
  { crEntryId :: Text
  , crCategory :: Text
  , crIsExplicit :: Bool
  , crSourceId :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData, Hashable)

data LinkRow = LinkRow
  { lrId :: Int64
  , lrEntryId :: Text
  , lrLinkType :: Text
  , lrTarget :: Text
  , lrDescription :: Maybe Text
  , lrPosition :: Int
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData, Hashable)

------------------------------------------------------------------------
-- SqlValue extraction helpers
------------------------------------------------------------------------

extractText :: SqlValue -> Either String Text
extractText (SqlText t) = Right t
extractText SqlNull = Left "expected Text, got NULL"
extractText v = Left $ "expected Text, got " ++ show v

extractInt :: SqlValue -> Either String Int
extractInt (SqlInt i) = Right (fromIntegral i)
extractInt SqlNull = Left "expected Int, got NULL"
extractInt v = Left $ "expected Int, got " ++ show v

extractInt64 :: SqlValue -> Either String Int64
extractInt64 (SqlInt i) = Right i
extractInt64 SqlNull = Left "expected Int64, got NULL"
extractInt64 v = Left $ "expected Int64, got " ++ show v

extractDouble :: SqlValue -> Either String Double
extractDouble (SqlDouble d) = Right d
extractDouble SqlNull = Left "expected Double, got NULL"
extractDouble v = Left $ "expected Double, got " ++ show v

extractBool :: SqlValue -> Either String Bool
extractBool (SqlBool b) = Right b
extractBool (SqlInt 0) = Right False
extractBool (SqlInt _) = Right True
extractBool SqlNull = Left "expected Bool, got NULL"
extractBool v = Left $ "expected Bool, got " ++ show v

extractDay :: SqlValue -> Either String Day
extractDay (SqlDay d) = Right d
extractDay (SqlText t) = maybe (Left $ "expected Day, could not parse: " ++ show t) Right (parseDay t)
extractDay SqlNull = Left "expected Day, got NULL"
extractDay v = Left $ "expected Day, got " ++ show v

extractUTCTime :: SqlValue -> Either String UTCTime
extractUTCTime (SqlUTCTime t) = Right t
extractUTCTime (SqlText t) = maybe (Left $ "expected UTCTime, could not parse: " ++ show t) Right (parseUTCTime t)
extractUTCTime SqlNull = Left "expected UTCTime, got NULL"
extractUTCTime v = Left $ "expected UTCTime, got " ++ show v

-- | Parse a 'UTCTime' from text, trying common database formats.
parseUTCTime :: Text -> Maybe UTCTime
parseUTCTime t =
  let s = unpack t
      try fmt = parseTimeM False defaultTimeLocale fmt s
   in asum
        [ try "%Y-%m-%d %H:%M:%S%Q"
        , try "%Y-%m-%d %H:%M:%S%Q UTC"
        , try "%Y-%m-%dT%H:%M:%S%QZ"
        ]

-- | Parse a 'Day' from text in @YYYY-MM-DD@ format.
parseDay :: Text -> Maybe Day
parseDay t = parseTimeM False defaultTimeLocale "%Y-%m-%d" (unpack t)

extractByteString :: SqlValue -> Either String ByteString
extractByteString (SqlBlob b) = Right b
extractByteString SqlNull = Left "expected ByteString, got NULL"
extractByteString v = Left $ "expected ByteString, got " ++ show v

extractMaybe :: (SqlValue -> Either String a) -> SqlValue -> Either String (Maybe a)
extractMaybe _ SqlNull = Right Nothing
extractMaybe f v = Just <$> f v

------------------------------------------------------------------------
-- FromRow instances
------------------------------------------------------------------------

instance FromRow FileRow where
  fromRow [id_, path, title, preamble, hash_, modTime, createdTime, createdAt, updatedAt] = do
    frId <- extractText id_
    frPath <- extractText path
    frTitle <- extractMaybe extractText title
    frPreamble <- extractMaybe extractText preamble
    frHash <- extractMaybe extractText hash_
    frModTime <- extractMaybe extractUTCTime modTime
    frCreatedTime <- extractMaybe extractUTCTime createdTime
    frCreatedAt <- extractUTCTime createdAt
    frUpdatedAt <- extractUTCTime updatedAt
    pure FileRow{..}
  fromRow vs = Left $ "FileRow: expected 9 columns, got " ++ show (length vs)

instance FromRow FilePropertyRow where
  fromRow [fileId, name_, value_, source] = do
    fprFileId <- extractText fileId
    fprName <- extractText name_
    fprValue <- extractText value_
    fprSource <- extractText source
    pure FilePropertyRow{..}
  fromRow vs = Left $ "FilePropertyRow: expected 4 columns, got " ++ show (length vs)

instance FromRow EntryRow where
  fromRow [eid, fid, pid, dep, pos_, bo, kwt, kwv, pri, hl, ttl, vrb, ctx, loc_, hash_, mt, ct, pth] = do
    erId <- extractText eid
    erFileId <- extractText fid
    erParentId <- extractMaybe extractText pid
    erDepth <- extractInt dep
    erPosition <- extractInt pos_
    erByteOffset <- extractInt bo
    erKeywordType <- extractMaybe extractText kwt
    erKeywordValue <- extractMaybe extractText kwv
    erPriority <- extractMaybe extractText pri
    erHeadline <- extractText hl
    erTitle <- extractText ttl
    erVerb <- extractMaybe extractText vrb
    erContext <- extractMaybe extractText ctx
    erLocator <- extractMaybe extractText loc_
    erHash <- extractMaybe extractText hash_
    erModTime <- extractMaybe extractUTCTime mt
    erCreatedTime <- extractMaybe extractUTCTime ct
    erPath <- extractMaybe extractText pth
    pure EntryRow{..}
  fromRow vs = Left $ "EntryRow: expected 18 columns, got " ++ show (length vs)

instance FromRow EntryTagRow where
  fromRow [eid, tag, inh, sid] = do
    etrEntryId <- extractText eid
    etrTag <- extractText tag
    etrIsInherited <- extractBool inh
    etrSourceId <- extractMaybe extractText sid
    pure EntryTagRow{..}
  fromRow vs = Left $ "EntryTagRow: expected 4 columns, got " ++ show (length vs)

instance FromRow EntryPropertyRow where
  fromRow [eid, name_, value_, inh, bo] = do
    eprEntryId <- extractText eid
    eprName <- extractText name_
    eprValue <- extractText value_
    eprIsInherited <- extractBool inh
    eprByteOffset <- extractMaybe extractInt bo
    pure EntryPropertyRow{..}
  fromRow vs = Left $ "EntryPropertyRow: expected 5 columns, got " ++ show (length vs)

instance FromRow EntryStampRow where
  fromRow [id_, eid, bo, stype, tkind, day_, dayEnd, tstart, tend, sk, sn, ss, sln, sls] = do
    esrId <- extractInt64 id_
    esrEntryId <- extractText eid
    esrByteOffset <- extractMaybe extractInt bo
    esrStampType <- extractText stype
    esrTimeKind <- extractText tkind
    esrDay <- extractInt day_
    esrDayEnd <- extractMaybe extractInt dayEnd
    esrTimeStart <- extractMaybe extractInt tstart
    esrTimeEnd <- extractMaybe extractInt tend
    esrSuffixKind <- extractMaybe extractText sk
    esrSuffixNum <- extractMaybe extractInt sn
    esrSuffixSpan <- extractMaybe extractText ss
    esrSuffixLargerNum <- extractMaybe extractInt sln
    esrSuffixLargerSpan <- extractMaybe extractText sls
    pure EntryStampRow{..}
  fromRow vs = Left $ "EntryStampRow: expected 14 columns, got " ++ show (length vs)

instance FromRow LogEntryRow where
  fromRow [id_, eid, pos_, bo, lt, td, ts, te, tk, fk, fkt, tok, tokt, otd, otde, ots, ote, otk, osk, osn, oss, dh, dm, bt, lbi] = do
    lerId <- extractInt64 id_
    lerEntryId <- extractText eid
    lerPosition <- extractInt pos_
    lerByteOffset <- extractMaybe extractInt bo
    lerLogType <- extractText lt
    lerTimeDay <- extractMaybe extractInt td
    lerTimeStart <- extractMaybe extractInt ts
    lerTimeEnd <- extractMaybe extractInt te
    lerTimeKind <- extractMaybe extractText tk
    lerFromKeyword <- extractMaybe extractText fk
    lerFromKeywordType <- extractMaybe extractText fkt
    lerToKeyword <- extractMaybe extractText tok
    lerToKeywordType <- extractMaybe extractText tokt
    lerOrigTimeDay <- extractMaybe extractInt otd
    lerOrigTimeDayEnd <- extractMaybe extractInt otde
    lerOrigTimeStart <- extractMaybe extractInt ots
    lerOrigTimeEnd <- extractMaybe extractInt ote
    lerOrigTimeKind <- extractMaybe extractText otk
    lerOrigSuffixKind <- extractMaybe extractText osk
    lerOrigSuffixNum <- extractMaybe extractInt osn
    lerOrigSuffixSpan <- extractMaybe extractText oss
    lerDurationHours <- extractMaybe extractInt dh
    lerDurationMins <- extractMaybe extractInt dm
    lerBodyText <- extractMaybe extractText bt
    lerLogbookId <- extractMaybe extractInt64 lbi
    pure LogEntryRow{..}
  fromRow vs = Left $ "LogEntryRow: expected 25 columns, got " ++ show (length vs)

instance FromRow BodyBlockRow where
  fromRow [id_, eid, pos_, bo, btype, content, dtype, dname] = do
    bbrId <- extractInt64 id_
    bbrEntryId <- extractText eid
    bbrPosition <- extractInt pos_
    bbrByteOffset <- extractMaybe extractInt bo
    bbrBlockType <- extractText btype
    bbrContent <- extractMaybe extractText content
    bbrDrawerType <- extractMaybe extractText dtype
    bbrDrawerName <- extractMaybe extractText dname
    pure BodyBlockRow{..}
  fromRow vs = Left $ "BodyBlockRow: expected 8 columns, got " ++ show (length vs)

instance FromRow RelationshipRow where
  fromRow [sid, tid, rtype, ctx, cat] = do
    rrSourceEntryId <- extractText sid
    rrTargetEntryId <- extractText tid
    rrRelationshipType <- extractText rtype
    rrContext <- extractMaybe extractText ctx
    rrCreatedAt <- extractUTCTime cat
    pure RelationshipRow{..}
  fromRow vs = Left $ "RelationshipRow: expected 5 columns, got " ++ show (length vs)

instance FromRow CategoryRow where
  fromRow [eid, cat, expl, sid] = do
    crEntryId <- extractText eid
    crCategory <- extractText cat
    crIsExplicit <- extractBool expl
    crSourceId <- extractMaybe extractText sid
    pure CategoryRow{..}
  fromRow vs = Left $ "CategoryRow: expected 4 columns, got " ++ show (length vs)

instance FromRow LinkRow where
  fromRow [id_, eid, ltype, target, desc, pos_] = do
    lrId <- extractInt64 id_
    lrEntryId <- extractText eid
    lrLinkType <- extractText ltype
    lrTarget <- extractText target
    lrDescription <- extractMaybe extractText desc
    lrPosition <- extractInt pos_
    pure LinkRow{..}
  fromRow vs = Left $ "LinkRow: expected 6 columns, got " ++ show (length vs)
