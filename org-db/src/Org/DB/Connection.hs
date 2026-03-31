{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Org.DB.Connection (
  connectDB,
  withDB,
  createDBPool,
  withPooledDB,
  destroyPool,
  PoolConfig (..),
  defaultPoolConfig,
) where

import Control.Exception (bracket)
import Control.Monad (replicateM, void)
import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text (Text, pack)
import Data.Text.Encoding qualified as TE
import Data.Time ()
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.FromField (typeOid)
import Database.PostgreSQL.Simple.FromField qualified as PGF
import Database.PostgreSQL.Simple.FromRow qualified as PGR
import Database.PostgreSQL.Simple.ToField qualified as PGT
import Database.PostgreSQL.Simple.TypeInfo.Static qualified as PGTI
import Database.PostgreSQL.Simple.Types (Binary (..), Null (..), Query (..))
import Database.SQLite.Simple qualified as SQLite
import Database.SQLite.Simple.Internal (Connection (connectionHandle))
import Database.SQLite3 qualified as Direct
import Org.DB.Types

------------------------------------------------------------------------
-- SqlValue → SQL parameters (Haskell → database)
------------------------------------------------------------------------

toPGParams :: [SqlValue] -> [PGT.Action]
toPGParams = map toPGAction

toPGAction :: SqlValue -> PGT.Action
toPGAction (SqlText t) = PGT.toField t
toPGAction (SqlInt i) = PGT.toField i
toPGAction (SqlDouble d) = PGT.toField d
toPGAction (SqlBool b) = PGT.toField b
toPGAction (SqlBlob bs) = PGT.toField (Binary bs)
toPGAction (SqlUTCTime t) = PGT.toField t
toPGAction (SqlDay d) = PGT.toField d
toPGAction SqlNull = PGT.toField Null

toDirectParams :: [SqlValue] -> [Direct.SQLData]
toDirectParams = map toDirectData

toDirectData :: SqlValue -> Direct.SQLData
toDirectData (SqlText t) = Direct.SQLText t
toDirectData (SqlInt i) = Direct.SQLInteger (fromIntegral i)
toDirectData (SqlDouble d) = Direct.SQLFloat d
toDirectData (SqlBool b) = Direct.SQLInteger (if b then 1 else 0)
toDirectData (SqlBlob bs) = Direct.SQLBlob bs
toDirectData (SqlUTCTime t) = Direct.SQLText (showText t)
toDirectData (SqlDay d) = Direct.SQLText (showText d)
toDirectData SqlNull = Direct.SQLNull

showText :: (Show a) => a -> Text
showText = pack . show

------------------------------------------------------------------------
-- SQL results → [SqlValue] (database → Haskell)
------------------------------------------------------------------------

-- PostgreSQL: use RowParser with OID-based type dispatch
pgRawRowParser :: PGR.RowParser [SqlValue]
pgRawRowParser = do
  n <- PGR.numFieldsRemaining
  replicateM n readPGField

readPGField :: PGR.RowParser SqlValue
readPGField = PGR.fieldWith $ \field mbs -> case mbs of
  Nothing -> pure SqlNull
  Just _ ->
    let oid = typeOid field
     in if
          | oid == PGTI.typoid PGTI.int2
              || oid == PGTI.typoid PGTI.int4
              || oid == PGTI.typoid PGTI.int8 ->
              SqlInt <$> PGF.fromField field mbs
          | oid == PGTI.typoid PGTI.float4
              || oid == PGTI.typoid PGTI.float8
              || oid == PGTI.typoid PGTI.numeric ->
              SqlDouble <$> PGF.fromField field mbs
          | oid == PGTI.typoid PGTI.bool ->
              SqlBool <$> PGF.fromField field mbs
          | oid == PGTI.typoid PGTI.timestamptz
              || oid == PGTI.typoid PGTI.timestamp ->
              SqlUTCTime <$> PGF.fromField field mbs
          | oid == PGTI.typoid PGTI.date ->
              SqlDay <$> PGF.fromField field mbs
          | oid == PGTI.typoid PGTI.bytea ->
              SqlBlob . fromBinary <$> PGF.fromField field mbs
          | otherwise ->
              SqlText <$> PGF.fromField field mbs

-- SQLite: use direct-sqlite for raw column access
directToSqlValue :: Direct.SQLData -> SqlValue
directToSqlValue Direct.SQLNull = SqlNull
directToSqlValue (Direct.SQLInteger i) = SqlInt (fromIntegral i)
directToSqlValue (Direct.SQLFloat d) = SqlDouble d
directToSqlValue (Direct.SQLText t) = SqlText t
directToSqlValue (Direct.SQLBlob bs) = SqlBlob bs

------------------------------------------------------------------------
-- Query execution with FromRow bridging
------------------------------------------------------------------------

pgQueryRows ::
  forall r. (FromRow r) => PG.Connection -> Text -> [SqlValue] -> IO [r]
pgQueryRows conn sql params = do
  rawRows <- PG.queryWith pgRawRowParser conn (pgQuery sql) (toPGParams params)
  mapM (either fail pure . fromRow) rawRows

sqliteQueryRows ::
  forall r. (FromRow r) => SQLite.Connection -> Text -> [SqlValue] -> IO [r]
sqliteQueryRows conn sql params = do
  let db = connectionHandle conn
  rawRows <- sqliteRawQuery db sql (toDirectParams params)
  mapM (either fail pure . fromRow) rawRows

sqliteRawQuery :: Direct.Database -> Text -> [Direct.SQLData] -> IO [[SqlValue]]
sqliteRawQuery db sql params =
  bracket (Direct.prepare db sql) Direct.finalize $ \stmt -> do
    Direct.bind stmt params
    collectRows stmt
 where
  collectRows stmt = do
    result <- Direct.step stmt
    case result of
      Direct.Done -> pure []
      Direct.Row -> do
        cols <- Direct.columns stmt
        let vals = map directToSqlValue cols
        rest <- collectRows stmt
        pure (vals : rest)

------------------------------------------------------------------------
-- Backend implementations
------------------------------------------------------------------------

connectPostgres :: ByteString -> IO DBHandle
connectPostgres connStr = do
  conn <- PG.connectPostgreSQL connStr
  pure
    DBHandle
      { dbExecute_ = \sql params ->
          void $ PG.execute conn (pgQuery sql) (toPGParams params)
      , dbExecute = \sql params ->
          fromIntegral <$> PG.execute conn (pgQuery sql) (toPGParams params)
      , dbQuery = pgQueryRows conn
      , dbQueryOne = \sql params -> do
          rows <- pgQueryRows conn sql params
          pure $ case rows of
            (r : _) -> Just r
            [] -> Nothing
      , dbTransaction = PG.withTransaction conn
      , dbClose = PG.close conn
      , dbBackend = PostgresConfig connStr
      }

connectSQLite :: FilePath -> IO DBHandle
connectSQLite path = do
  conn <- SQLite.open path
  SQLite.execute_ conn "PRAGMA journal_mode=WAL;"
  SQLite.execute_ conn "PRAGMA foreign_keys=ON;"
  SQLite.execute_ conn "PRAGMA busy_timeout=5000;"
  pure
    DBHandle
      { dbExecute_ = \sql params -> do
          let db = connectionHandle conn
          bracket (Direct.prepare db sql) Direct.finalize $ \stmt -> do
            Direct.bind stmt (toDirectParams params)
            _ <- Direct.step stmt
            pure ()
      , dbExecute = \sql params -> do
          let db = connectionHandle conn
          bracket (Direct.prepare db sql) Direct.finalize $ \stmt -> do
            Direct.bind stmt (toDirectParams params)
            _ <- Direct.step stmt
            fromIntegral <$> Direct.changes db
      , dbQuery = sqliteQueryRows conn
      , dbQueryOne = \sql params -> do
          rows <- sqliteQueryRows conn sql params
          pure $ case rows of
            (r : _) -> Just r
            [] -> Nothing
      , dbTransaction = SQLite.withTransaction conn
      , dbClose = SQLite.close conn
      , dbBackend = SQLiteConfig path
      }

pgQuery :: Text -> PG.Query
pgQuery = Query . TE.encodeUtf8

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

connectDB :: DBConfig -> IO DBHandle
connectDB (PostgresConfig connStr) = connectPostgres connStr
connectDB (SQLiteConfig path) = connectSQLite path

withDB :: DBConfig -> (DBHandle -> IO a) -> IO a
withDB config = bracket (connectDB config) dbClose

------------------------------------------------------------------------
-- Connection Pooling
------------------------------------------------------------------------

data PoolConfig = PoolConfig
  { poolIdleTime :: Double
  , poolMaxResources :: Int
  }
  deriving (Show, Eq)

defaultPoolConfig :: PoolConfig
defaultPoolConfig = PoolConfig 60.0 10

createDBPool :: PoolConfig -> DBConfig -> IO (Pool DBHandle)
createDBPool pc dbConfig =
  Pool.newPool $
    Pool.defaultPoolConfig
      (connectDB dbConfig)
      dbClose
      (poolIdleTime pc)
      (poolMaxResources pc)

withPooledDB :: Pool DBHandle -> (DBHandle -> IO a) -> IO a
withPooledDB = Pool.withResource

destroyPool :: Pool DBHandle -> IO ()
destroyPool = Pool.destroyAllResources
