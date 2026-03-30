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
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | Database backend configuration.
data DBConfig
  = PostgresConfig ByteString
  | SQLiteConfig FilePath
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

-- | Backend-agnostic database handle using the record-of-functions pattern.
data DBHandle = DBHandle
  { dbExecute_ :: Text -> [SqlValue] -> IO ()
  , dbExecute :: Text -> [SqlValue] -> IO Int64
  , dbQuery :: forall r. (FromRow r) => Text -> [SqlValue] -> IO [r]
  , dbQueryOne :: forall r. (FromRow r) => Text -> [SqlValue] -> IO (Maybe r)
  , dbTransaction :: forall a. IO a -> IO a
  , dbClose :: IO ()
  , dbBackend :: DBConfig
  }

------------------------------------------------------------------------
-- Row Types
------------------------------------------------------------------------

data FileRow = FileRow
  { frId :: Text
  , frPath :: Text
  , frMtime :: UTCTime
  , frHash :: ByteString
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

data FilePropertyRow = FilePropertyRow
  { fprFileId :: Text
  , fprName :: Text
  , fprValue :: Text
  , fprInherited :: Bool
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData, Hashable)

data EntryRow = EntryRow
  { erEntryId :: Text
  , erFileId :: Text
  , erParentId :: Maybe Text
  , erDepth :: Int
  , erKeyword :: Maybe Text
  , erKeywordClosed :: Bool
  , erPriority :: Maybe Text
  , erHeadline :: Text
  , erVerb :: Maybe Text
  , erTitle :: Text
  , erContext :: Maybe Text
  , erLocator :: Maybe Text
  , erFileLine :: Int
  , erPath :: Text
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData, Hashable)

data EntryTagRow = EntryTagRow
  { etrEntryId :: Text
  , etrTag :: Text
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData, Hashable)

data EntryPropertyRow = EntryPropertyRow
  { eprEntryId :: Text
  , eprName :: Text
  , eprValue :: Text
  , eprInherited :: Bool
  , eprFileLine :: Int
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData, Hashable)

data EntryStampRow = EntryStampRow
  { esrEntryId :: Text
  , esrStampType :: Text
  , esrTimeStart :: UTCTime
  , esrTimeEnd :: Maybe UTCTime
  , esrTimeKind :: Text
  , esrFileLine :: Int
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

data LogEntryRow = LogEntryRow
  { lerEntryId :: Text
  , lerLogType :: Text
  , lerLogTime :: UTCTime
  , lerFromState :: Maybe Text
  , lerToState :: Maybe Text
  , lerOldTime :: Maybe UTCTime
  , lerDurationMins :: Maybe Int
  , lerFileLine :: Int
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

data BodyBlockRow = BodyBlockRow
  { bbrEntryId :: Text
  , bbrBlockType :: Text
  , bbrContent :: Text
  , bbrSeqNum :: Int
  , bbrFileLine :: Int
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData, Hashable)

data RelationshipRow = RelationshipRow
  { rrSourceId :: Text
  , rrTargetId :: Text
  , rrRelType :: Text
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData, Hashable)

data CategoryRow = CategoryRow
  { crFileId :: Text
  , crCategory :: Text
  }
  deriving (Show, Eq, Generic, Data, Typeable, NFData, Hashable)

data LinkRow = LinkRow
  { lrEntryId :: Text
  , lrLinkType :: Text
  , lrTarget :: Text
  , lrDescription :: Maybe Text
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
extractDay SqlNull = Left "expected Day, got NULL"
extractDay v = Left $ "expected Day, got " ++ show v

extractUTCTime :: SqlValue -> Either String UTCTime
extractUTCTime (SqlUTCTime t) = Right t
extractUTCTime SqlNull = Left "expected UTCTime, got NULL"
extractUTCTime v = Left $ "expected UTCTime, got " ++ show v

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
  fromRow [id_, path, mtime, hash_] = do
    frId <- extractText id_
    frPath <- extractText path
    frMtime <- extractUTCTime mtime
    frHash <- extractByteString hash_
    pure FileRow{..}
  fromRow vs = Left $ "FileRow: expected 4 columns, got " ++ show (length vs)

instance FromRow FilePropertyRow where
  fromRow [fileId, name_, value_, inh] = do
    fprFileId <- extractText fileId
    fprName <- extractText name_
    fprValue <- extractText value_
    fprInherited <- extractBool inh
    pure FilePropertyRow{..}
  fromRow vs = Left $ "FilePropertyRow: expected 4 columns, got " ++ show (length vs)

instance FromRow EntryRow where
  fromRow [eid, fid, pid, dep, kw, kwc, pri, hl, vrb, ttl, ctx, loc_, fl, pth] = do
    erEntryId <- extractText eid
    erFileId <- extractText fid
    erParentId <- extractMaybe extractText pid
    erDepth <- extractInt dep
    erKeyword <- extractMaybe extractText kw
    erKeywordClosed <- extractBool kwc
    erPriority <- extractMaybe extractText pri
    erHeadline <- extractText hl
    erVerb <- extractMaybe extractText vrb
    erTitle <- extractText ttl
    erContext <- extractMaybe extractText ctx
    erLocator <- extractMaybe extractText loc_
    erFileLine <- extractInt fl
    erPath <- extractText pth
    pure EntryRow{..}
  fromRow vs = Left $ "EntryRow: expected 14 columns, got " ++ show (length vs)

instance FromRow EntryTagRow where
  fromRow [eid, tag] = do
    etrEntryId <- extractText eid
    etrTag <- extractText tag
    pure EntryTagRow{..}
  fromRow vs = Left $ "EntryTagRow: expected 2 columns, got " ++ show (length vs)

instance FromRow EntryPropertyRow where
  fromRow [eid, name_, value_, inh, fl] = do
    eprEntryId <- extractText eid
    eprName <- extractText name_
    eprValue <- extractText value_
    eprInherited <- extractBool inh
    eprFileLine <- extractInt fl
    pure EntryPropertyRow{..}
  fromRow vs = Left $ "EntryPropertyRow: expected 5 columns, got " ++ show (length vs)

instance FromRow EntryStampRow where
  fromRow [eid, stype, tstart, tend, tkind, fl] = do
    esrEntryId <- extractText eid
    esrStampType <- extractText stype
    esrTimeStart <- extractUTCTime tstart
    esrTimeEnd <- extractMaybe extractUTCTime tend
    esrTimeKind <- extractText tkind
    esrFileLine <- extractInt fl
    pure EntryStampRow{..}
  fromRow vs = Left $ "EntryStampRow: expected 6 columns, got " ++ show (length vs)

instance FromRow LogEntryRow where
  fromRow [eid, ltype, ltime, fstate, tstate, otime, dur, fl] = do
    lerEntryId <- extractText eid
    lerLogType <- extractText ltype
    lerLogTime <- extractUTCTime ltime
    lerFromState <- extractMaybe extractText fstate
    lerToState <- extractMaybe extractText tstate
    lerOldTime <- extractMaybe extractUTCTime otime
    lerDurationMins <- extractMaybe extractInt dur
    lerFileLine <- extractInt fl
    pure LogEntryRow{..}
  fromRow vs = Left $ "LogEntryRow: expected 8 columns, got " ++ show (length vs)

instance FromRow BodyBlockRow where
  fromRow [eid, btype, content, seqn, fl] = do
    bbrEntryId <- extractText eid
    bbrBlockType <- extractText btype
    bbrContent <- extractText content
    bbrSeqNum <- extractInt seqn
    bbrFileLine <- extractInt fl
    pure BodyBlockRow{..}
  fromRow vs = Left $ "BodyBlockRow: expected 5 columns, got " ++ show (length vs)

instance FromRow RelationshipRow where
  fromRow [sid, tid, rtype] = do
    rrSourceId <- extractText sid
    rrTargetId <- extractText tid
    rrRelType <- extractText rtype
    pure RelationshipRow{..}
  fromRow vs = Left $ "RelationshipRow: expected 3 columns, got " ++ show (length vs)

instance FromRow CategoryRow where
  fromRow [fid, cat] = do
    crFileId <- extractText fid
    crCategory <- extractText cat
    pure CategoryRow{..}
  fromRow vs = Left $ "CategoryRow: expected 2 columns, got " ++ show (length vs)

instance FromRow LinkRow where
  fromRow [eid, ltype, target, desc] = do
    lrEntryId <- extractText eid
    lrLinkType <- extractText ltype
    lrTarget <- extractText target
    lrDescription <- extractMaybe extractText desc
    pure LinkRow{..}
  fromRow vs = Left $ "LinkRow: expected 4 columns, got " ++ show (length vs)
