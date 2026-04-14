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
  , Migration
      { migVersion = 5
      , migDescription = "Add position column to file_properties for repeated keys"
      , migUp =
          [ "ALTER TABLE file_properties DROP CONSTRAINT IF EXISTS file_properties_pkey"
          , "ALTER TABLE file_properties ADD COLUMN IF NOT EXISTS position INTEGER NOT NULL DEFAULT 0"
          , "ALTER TABLE file_properties ADD PRIMARY KEY (file_id, name, source, position)"
          ]
      }
  , Migration
      { migVersion = 6
      , migDescription = "Add embedding_hash for change detection"
      , migUp =
          ["ALTER TABLE entries ADD COLUMN IF NOT EXISTS embedding_hash TEXT"]
      }
  , Migration
      { migVersion = 7
      , migDescription = "Normalize log entry bodies into log_entry_body_blocks table"
      , migUp =
          [ "CREATE TABLE IF NOT EXISTS log_entry_body_blocks (\
            \  id BIGSERIAL PRIMARY KEY,\
            \  log_entry_id BIGINT NOT NULL REFERENCES entry_log_entries(id) ON DELETE CASCADE,\
            \  position INTEGER NOT NULL,\
            \  byte_offset INTEGER,\
            \  block_type TEXT NOT NULL CHECK (block_type IN\
            \    ('whitespace', 'paragraph', 'drawer', 'inline_task')),\
            \  content TEXT,\
            \  drawer_type TEXT CHECK (drawer_type IN ('plain', 'begin')),\
            \  drawer_name TEXT,\
            \  UNIQUE (log_entry_id, position)\
            \)"
          , "CREATE INDEX IF NOT EXISTS idx_log_body_blocks_log_entry \
            \ON log_entry_body_blocks(log_entry_id)"
          , "ALTER TABLE entry_log_entries DROP COLUMN IF EXISTS body_text"
          , "UPDATE files SET hash = NULL, mod_time = NULL"
          ]
      }
  , Migration
      { migVersion = 8
      , migDescription = "Remove fixed dimension from embedding column to support any model"
      , migUp =
          [ "DROP INDEX IF EXISTS idx_entries_embedding"
          , "ALTER TABLE entries ALTER COLUMN embedding TYPE vector USING embedding::vector"
          ]
      }
  , Migration
      { migVersion = 9
      , migDescription = "Move embeddings to entry_embeddings table with chunked text"
      , migUp =
          [ "CREATE TABLE IF NOT EXISTS entry_embeddings (\
            \  id BIGSERIAL PRIMARY KEY,\
            \  entry_id TEXT NOT NULL REFERENCES entries(id) ON DELETE CASCADE,\
            \  chunk_position INTEGER NOT NULL,\
            \  chunk_text TEXT NOT NULL,\
            \  embedding vector,\
            \  UNIQUE (entry_id, chunk_position)\
            \)"
          , "CREATE INDEX IF NOT EXISTS idx_entry_embeddings_entry \
            \ON entry_embeddings(entry_id)"
          , "ALTER TABLE entries DROP COLUMN IF EXISTS embedding"
          , "UPDATE entries SET embedding_hash = NULL"
          ]
      }
  , Migration
      { migVersion = 10
      , migDescription = "Add chunk_source column to entry_embeddings for source tracking"
      , migUp =
          [ "ALTER TABLE entry_embeddings ADD COLUMN IF NOT EXISTS \
            \chunk_source TEXT NOT NULL DEFAULT ''"
          , "UPDATE entries SET embedding_hash = NULL"
          ]
      }
  , Migration
      { migVersion = 11
      , migDescription = "Add title_embedding column for title-only similarity"
      , migUp =
          [ "ALTER TABLE entries ADD COLUMN IF NOT EXISTS \
            \title_embedding vector"
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
