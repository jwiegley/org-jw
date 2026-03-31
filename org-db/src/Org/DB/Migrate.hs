{-# LANGUAGE OverloadedStrings #-}

module Org.DB.Migrate (
  runMigrations,
  MigrationResult (..),
  Migration (..),
  migrations,
) where

import Data.Int (Int64)
import Data.Text (Text)
import Org.DB.Types

-- | Result of running migrations.
data MigrationResult
  = MigrationsApplied Int
  | AlreadyCurrent
  | MigrationFailed Int Text
  deriving (Show, Eq)

-- | A single migration step.
data Migration = Migration
  { migVersion :: Int
  , migDescription :: Text
  , migUp :: [Text]
  }
  deriving (Show, Eq)

{- | All known migrations, in version order.
Version 1 was the original SQLite-era schema.
Version 2 is the PRD 02 PostgreSQL-only schema (created by initDB).
Future schema changes add new migrations here.
-}
migrations :: [Migration]
migrations =
  [ Migration
      { migVersion = 3
      , migDescription = "Add tsvector full-text search column to entries"
      , migUp =
          [ "ALTER TABLE entries ADD COLUMN IF NOT EXISTS \
            \tsv tsvector GENERATED ALWAYS AS (\
            \  setweight(to_tsvector('english', coalesce(headline, '')), 'A') ||\
            \  setweight(to_tsvector('english', coalesce(title, '')), 'B')\
            \) STORED"
          , "CREATE INDEX IF NOT EXISTS idx_entries_tsv ON entries USING GIN (tsv)"
          ]
      }
  , Migration
      { migVersion = 4
      , migDescription = "Add vector embedding column to entries"
      , migUp =
          [ "ALTER TABLE entries ADD COLUMN IF NOT EXISTS \
            \embedding vector(1536)"
          , "CREATE INDEX IF NOT EXISTS idx_entries_embedding \
            \ON entries USING ivfflat (embedding vector_cosine_ops) \
            \WITH (lists = 100) \
            \WHERE embedding IS NOT NULL"
          ]
      }
  ]

{- | Run all pending migrations.
Returns the number of migrations applied, or an error.
-}
runMigrations :: DBHandle -> IO MigrationResult
runMigrations db = do
  currentVersion <- getCurrentVersion db
  let pending = filter (\m -> migVersion m > currentVersion) migrations
  if null pending
    then pure AlreadyCurrent
    else do
      results <- mapM (applyMigration db) pending
      case filter isFailed results of
        (MigrationFailed v msg : _) -> pure (MigrationFailed v msg)
        _ -> pure (MigrationsApplied (length pending))

getCurrentVersion :: DBHandle -> IO Int
getCurrentVersion db = do
  rows <-
    dbQuery
      db
      "SELECT COALESCE(MAX(version), 0) FROM schema_version"
      [] ::
      IO [[SqlValue]]
  case rows of
    ([SqlInt v] : _) -> pure (fromIntegral v)
    _ -> pure 0

applyMigration :: DBHandle -> Migration -> IO MigrationResult
applyMigration db mig = do
  dbTransaction db $ do
    mapM_ (\sql -> dbExecute_ db sql []) (migUp mig)
    dbExecute_
      db
      "INSERT INTO schema_version (version, description) VALUES (?, ?)"
      [SqlInt (fromIntegral (migVersion mig) :: Int64), SqlText (migDescription mig)]
  pure (MigrationsApplied 1)

isFailed :: MigrationResult -> Bool
isFailed (MigrationFailed _ _) = True
isFailed _ = False
