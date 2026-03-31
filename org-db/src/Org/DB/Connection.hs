{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
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
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.FromField (typeOid)
import Database.PostgreSQL.Simple.FromField qualified as PGF
import Database.PostgreSQL.Simple.FromRow qualified as PGR
import Database.PostgreSQL.Simple.ToField qualified as PGT
import Database.PostgreSQL.Simple.TypeInfo.Static qualified as PGTI
import Database.PostgreSQL.Simple.Types (Binary (..), Null (..), Query (..))
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

------------------------------------------------------------------------
-- SQL results → [SqlValue] (database → Haskell)
------------------------------------------------------------------------

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

------------------------------------------------------------------------
-- Query execution with FromRow bridging
------------------------------------------------------------------------

pgQueryRows ::
  forall r. (FromRow r) => PG.Connection -> Text -> [SqlValue] -> IO [r]
pgQueryRows conn sql params = do
  rawRows <- PG.queryWith pgRawRowParser conn (pgQuery sql) (toPGParams params)
  mapM (either fail pure . fromRow) rawRows

pgQuery :: Text -> PG.Query
pgQuery = Query . TE.encodeUtf8

------------------------------------------------------------------------
-- PostgreSQL connection
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

connectDB :: DBConfig -> IO DBHandle
connectDB cfg = do
  conn <- PG.connectPostgreSQL (pgConnString cfg)
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
      }

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
