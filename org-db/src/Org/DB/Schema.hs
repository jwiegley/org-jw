{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Org.DB.Schema (
  initDB,
  schemaVersion,
  getEmbeddingDimensions,
) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import Org.DB.Types
import Text.Read (readMaybe)

-- | Current schema version. Bump when changing table definitions.
schemaVersion :: Int
schemaVersion = 2

{- | Initialize the database schema, creating all tables if they don't exist.
The embedding dimension is required and determines the vector column types.
It is stored in db_settings so that subsequent commands can read it.

Extension creation is best-effort since it requires superuser.
If extensions are missing, create them manually:
  psql -U postgres -d <dbname> -c 'CREATE EXTENSION IF NOT EXISTS ltree; CREATE EXTENSION IF NOT EXISTS vector'
-}
initDB :: DBHandle -> Int -> IO ()
initDB db dims = do
  -- Extensions run outside the transaction since failure (permission denied)
  -- would abort the entire transaction in PostgreSQL.
  mapM_ tryExt extensionsDDL
  dbTransaction db $ do
    mapM_ (\ddl -> dbExecute_ db ddl []) (tableDDL dims)
    -- Store the embedding dimensions for use by other commands
    dbExecute_
      db
      "INSERT INTO db_settings (key, value) VALUES ('embedding_dimensions', ?) \
      \ON CONFLICT (key) DO UPDATE SET value = EXCLUDED.value"
      [SqlText (T.pack (show dims))]
    mapM_ (\ddl -> dbExecute_ db ddl []) indexDDL
    mapM_ (\ddl -> dbExecute_ db ddl []) triggerDDL
    existing <- dbQuery db "SELECT version FROM schema_version WHERE version = ?" [SqlInt (fromIntegral schemaVersion)] :: IO [[SqlValue]]
    case existing of
      [] ->
        dbExecute_
          db
          "INSERT INTO schema_version (version, description) VALUES (?, ?)"
          [SqlInt (fromIntegral schemaVersion), SqlText "PRD 02 PostgreSQL-only schema"]
      _ -> pure ()
 where
  tryExt ddl = do
    result <- try (dbExecute_ db ddl []) :: IO (Either SomeException ())
    case result of
      Right () -> pure ()
      Left _ -> pure () -- Skip — requires superuser

{- | Read the embedding dimensions from db_settings.
Returns Nothing if the database has not been initialized or if the
db_settings table does not exist.
-}
getEmbeddingDimensions :: DBHandle -> IO (Maybe Int)
getEmbeddingDimensions db = do
  result <-
    try
      ( dbQuery
          db
          "SELECT value FROM db_settings WHERE key = 'embedding_dimensions'"
          []
      ) ::
      IO (Either SomeException [[SqlValue]])
  case result of
    Right ([SqlText t] : _) -> pure (readMaybe (T.unpack t))
    _ -> pure Nothing

------------------------------------------------------------------------
-- Extensions
------------------------------------------------------------------------

extensionsDDL :: [Text]
extensionsDDL =
  [ "CREATE EXTENSION IF NOT EXISTS ltree"
  , "CREATE EXTENSION IF NOT EXISTS vector"
  ]

------------------------------------------------------------------------
-- Table DDL
------------------------------------------------------------------------

tableDDL :: Int -> [Text]
tableDDL dims =
  [ createSchemaVersion
  , createDBSettings
  , createFiles
  , createFileProperties
  , createEntries dims
  , createEntryTags
  , createEntryProperties
  , createEntryStamps
  , createEntryLogEntries
  , createLogEntryBodyBlocks
  , createEntryBodyBlocks
  , createEntryRelationships
  , createEntryCategories
  , createEntryLinks
  , createEntryEmbeddings dims
  ]

createSchemaVersion :: Text
createSchemaVersion =
  "CREATE TABLE IF NOT EXISTS schema_version (\
  \  version INTEGER PRIMARY KEY,\
  \  applied_at TIMESTAMPTZ NOT NULL DEFAULT now(),\
  \  description TEXT\
  \)"

createDBSettings :: Text
createDBSettings =
  "CREATE TABLE IF NOT EXISTS db_settings (\
  \  key TEXT PRIMARY KEY,\
  \  value TEXT NOT NULL\
  \)"

createFiles :: Text
createFiles =
  "CREATE TABLE IF NOT EXISTS files (\
  \  id TEXT PRIMARY KEY,\
  \  path TEXT NOT NULL UNIQUE,\
  \  title TEXT,\
  \  preamble TEXT,\
  \  hash TEXT,\
  \  mod_time TIMESTAMPTZ,\
  \  created_time TIMESTAMPTZ,\
  \  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),\
  \  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()\
  \)"

createFileProperties :: Text
createFileProperties =
  "CREATE TABLE IF NOT EXISTS file_properties (\
  \  file_id TEXT NOT NULL REFERENCES files(id) ON DELETE CASCADE,\
  \  position INTEGER NOT NULL,\
  \  name TEXT NOT NULL,\
  \  value TEXT NOT NULL,\
  \  source TEXT NOT NULL CHECK (source IN ('drawer', 'file')),\
  \  PRIMARY KEY (file_id, name, source, position)\
  \)"

createEntries :: Int -> Text
createEntries dims =
  "CREATE TABLE IF NOT EXISTS entries (\
  \  id TEXT PRIMARY KEY,\
  \  file_id TEXT NOT NULL REFERENCES files(id) ON DELETE CASCADE,\
  \  parent_id TEXT REFERENCES entries(id) ON DELETE CASCADE,\
  \  depth SMALLINT NOT NULL,\
  \  position INTEGER NOT NULL,\
  \  byte_offset INTEGER NOT NULL,\
  \  keyword_type TEXT CHECK (keyword_type IN ('open', 'closed')),\
  \  keyword_value TEXT,\
  \  priority TEXT,\
  \  headline TEXT NOT NULL,\
  \  title TEXT NOT NULL,\
  \  verb TEXT,\
  \  context TEXT,\
  \  locator TEXT,\
  \  hash TEXT,\
  \  mod_time TIMESTAMPTZ,\
  \  created_time TIMESTAMPTZ,\
  \  path ltree,\
  \  embedding_hash TEXT,\
  \  title_embedding vector("
    <> T.pack (show dims)
    <> "),\
       \  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),\
       \  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()\
       \)"

createEntryTags :: Text
createEntryTags =
  "CREATE TABLE IF NOT EXISTS entry_tags (\
  \  entry_id TEXT NOT NULL REFERENCES entries(id) ON DELETE CASCADE,\
  \  tag TEXT NOT NULL,\
  \  is_inherited BOOLEAN NOT NULL DEFAULT false,\
  \  source_id TEXT REFERENCES entries(id),\
  \  PRIMARY KEY (entry_id, tag)\
  \)"

createEntryProperties :: Text
createEntryProperties =
  "CREATE TABLE IF NOT EXISTS entry_properties (\
  \  entry_id TEXT NOT NULL REFERENCES entries(id) ON DELETE CASCADE,\
  \  name TEXT NOT NULL,\
  \  value TEXT NOT NULL,\
  \  is_inherited BOOLEAN NOT NULL DEFAULT false,\
  \  byte_offset INTEGER,\
  \  PRIMARY KEY (entry_id, name)\
  \)"

createEntryStamps :: Text
createEntryStamps =
  "CREATE TABLE IF NOT EXISTS entry_stamps (\
  \  id BIGSERIAL PRIMARY KEY,\
  \  entry_id TEXT NOT NULL REFERENCES entries(id) ON DELETE CASCADE,\
  \  byte_offset INTEGER,\
  \  stamp_type TEXT NOT NULL CHECK (stamp_type IN\
  \    ('closed', 'scheduled', 'deadline', 'active')),\
  \  time_kind TEXT NOT NULL CHECK (time_kind IN ('active', 'inactive')),\
  \  day INTEGER NOT NULL,\
  \  day_end INTEGER,\
  \  time_start INTEGER,\
  \  time_end INTEGER,\
  \  suffix_kind TEXT CHECK (suffix_kind IN\
  \    ('repeat', 'repeat_plus', 'dotted_repeat', 'within')),\
  \  suffix_num INTEGER,\
  \  suffix_span TEXT CHECK (suffix_span IN ('day', 'week', 'month', 'year')),\
  \  suffix_larger_num INTEGER,\
  \  suffix_larger_span TEXT CHECK (suffix_larger_span IN\
  \    ('day', 'week', 'month', 'year'))\
  \)"

createEntryLogEntries :: Text
createEntryLogEntries =
  "CREATE TABLE IF NOT EXISTS entry_log_entries (\
  \  id BIGSERIAL PRIMARY KEY,\
  \  entry_id TEXT NOT NULL REFERENCES entries(id) ON DELETE CASCADE,\
  \  position INTEGER NOT NULL,\
  \  byte_offset INTEGER,\
  \  log_type TEXT NOT NULL CHECK (log_type IN (\
  \    'closing', 'state', 'note', 'rescheduled', 'not_scheduled',\
  \    'deadline', 'no_deadline', 'refiling', 'clock', 'logbook')),\
  \  time_day INTEGER,\
  \  time_start INTEGER,\
  \  time_end INTEGER,\
  \  time_kind TEXT,\
  \  from_keyword TEXT,\
  \  from_keyword_type TEXT CHECK (from_keyword_type IN ('open', 'closed')),\
  \  to_keyword TEXT,\
  \  to_keyword_type TEXT CHECK (to_keyword_type IN ('open', 'closed')),\
  \  orig_time_day INTEGER,\
  \  orig_time_day_end INTEGER,\
  \  orig_time_start INTEGER,\
  \  orig_time_end INTEGER,\
  \  orig_time_kind TEXT,\
  \  orig_suffix_kind TEXT CHECK (orig_suffix_kind IN\
  \    ('repeat', 'repeat_plus', 'dotted_repeat', 'within')),\
  \  orig_suffix_num INTEGER,\
  \  orig_suffix_span TEXT CHECK (orig_suffix_span IN\
  \    ('day', 'week', 'month', 'year')),\
  \  duration_hours INTEGER,\
  \  duration_mins INTEGER,\
  \  logbook_id BIGINT REFERENCES entry_log_entries(id)\
  \)"

createLogEntryBodyBlocks :: Text
createLogEntryBodyBlocks =
  "CREATE TABLE IF NOT EXISTS log_entry_body_blocks (\
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

createEntryBodyBlocks :: Text
createEntryBodyBlocks =
  "CREATE TABLE IF NOT EXISTS entry_body_blocks (\
  \  id BIGSERIAL PRIMARY KEY,\
  \  entry_id TEXT NOT NULL REFERENCES entries(id) ON DELETE CASCADE,\
  \  position INTEGER NOT NULL,\
  \  byte_offset INTEGER,\
  \  block_type TEXT NOT NULL CHECK (block_type IN\
  \    ('whitespace', 'paragraph', 'drawer', 'inline_task')),\
  \  content TEXT,\
  \  drawer_type TEXT CHECK (drawer_type IN ('plain', 'begin')),\
  \  drawer_name TEXT,\
  \  UNIQUE (entry_id, position)\
  \)"

createEntryRelationships :: Text
createEntryRelationships =
  "CREATE TABLE IF NOT EXISTS entry_relationships (\
  \  source_entry_id TEXT NOT NULL REFERENCES entries(id) ON DELETE CASCADE,\
  \  target_entry_id TEXT NOT NULL REFERENCES entries(id) ON DELETE CASCADE,\
  \  relationship_type TEXT NOT NULL CHECK (relationship_type IN (\
  \    'link', 'blocks', 'blocked_by', 'related', 'parent_child')),\
  \  context TEXT,\
  \  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),\
  \  PRIMARY KEY (source_entry_id, target_entry_id, relationship_type),\
  \  CHECK (source_entry_id != target_entry_id)\
  \)"

createEntryCategories :: Text
createEntryCategories =
  "CREATE TABLE IF NOT EXISTS entry_categories (\
  \  entry_id TEXT NOT NULL REFERENCES entries(id) ON DELETE CASCADE,\
  \  category TEXT NOT NULL,\
  \  is_explicit BOOLEAN NOT NULL DEFAULT false,\
  \  source_id TEXT,\
  \  PRIMARY KEY (entry_id, category)\
  \)"

createEntryLinks :: Text
createEntryLinks =
  "CREATE TABLE IF NOT EXISTS entry_links (\
  \  id BIGSERIAL PRIMARY KEY,\
  \  entry_id TEXT NOT NULL REFERENCES entries(id) ON DELETE CASCADE,\
  \  link_type TEXT NOT NULL,\
  \  target TEXT NOT NULL,\
  \  description TEXT,\
  \  position INTEGER NOT NULL\
  \)"

createEntryEmbeddings :: Int -> Text
createEntryEmbeddings dims =
  "CREATE TABLE IF NOT EXISTS entry_embeddings (\
  \  id BIGSERIAL PRIMARY KEY,\
  \  entry_id TEXT NOT NULL REFERENCES entries(id) ON DELETE CASCADE,\
  \  chunk_position INTEGER NOT NULL,\
  \  chunk_source TEXT NOT NULL,\
  \  chunk_text TEXT NOT NULL,\
  \  embedding vector("
    <> T.pack (show dims)
    <> "),\
       \  UNIQUE (entry_id, chunk_position)\
       \)"

------------------------------------------------------------------------
-- Index DDL
------------------------------------------------------------------------

indexDDL :: [Text]
indexDDL =
  [ -- entries
    "CREATE INDEX IF NOT EXISTS idx_entries_file ON entries(file_id)"
  , "CREATE INDEX IF NOT EXISTS idx_entries_parent ON entries(parent_id)"
  , "CREATE INDEX IF NOT EXISTS idx_entries_keyword ON entries(keyword_value) WHERE keyword_value IS NOT NULL"
  , "CREATE INDEX IF NOT EXISTS idx_entries_path ON entries USING GIST (path)"
  , "CREATE INDEX IF NOT EXISTS idx_entries_embedding_hash ON entries(embedding_hash)"
  , -- entry_tags
    "CREATE INDEX IF NOT EXISTS idx_entry_tags_tag ON entry_tags(tag)"
  , -- entry_properties
    "CREATE INDEX IF NOT EXISTS idx_entry_properties_name ON entry_properties(name)"
  , -- entry_stamps
    "CREATE INDEX IF NOT EXISTS idx_entry_stamps_entry ON entry_stamps(entry_id)"
  , "CREATE INDEX IF NOT EXISTS idx_entry_stamps_type ON entry_stamps(stamp_type)"
  , "CREATE INDEX IF NOT EXISTS idx_entry_stamps_day ON entry_stamps(day)"
  , -- entry_log_entries
    "CREATE INDEX IF NOT EXISTS idx_log_entries_entry ON entry_log_entries(entry_id)"
  , "CREATE INDEX IF NOT EXISTS idx_log_entries_type ON entry_log_entries(log_type)"
  , "CREATE INDEX IF NOT EXISTS idx_log_entries_logbook ON entry_log_entries(logbook_id) WHERE logbook_id IS NOT NULL"
  , "CREATE INDEX IF NOT EXISTS idx_log_entries_time_day ON entry_log_entries(time_day) WHERE time_day IS NOT NULL"
  , -- log_entry_body_blocks
    "CREATE INDEX IF NOT EXISTS idx_log_body_blocks_log_entry ON log_entry_body_blocks(log_entry_id)"
  , -- entry_body_blocks
    "CREATE INDEX IF NOT EXISTS idx_body_blocks_entry ON entry_body_blocks(entry_id)"
  , -- entry_relationships
    "CREATE INDEX IF NOT EXISTS idx_rel_target ON entry_relationships(target_entry_id)"
  , "CREATE INDEX IF NOT EXISTS idx_rel_type ON entry_relationships(relationship_type)"
  , -- entry_categories
    "CREATE INDEX IF NOT EXISTS idx_categories_cat ON entry_categories(category)"
  , -- entry_links
    "CREATE INDEX IF NOT EXISTS idx_links_entry ON entry_links(entry_id)"
  , "CREATE INDEX IF NOT EXISTS idx_links_target ON entry_links(target)"
  , -- entry_embeddings
    "CREATE INDEX IF NOT EXISTS idx_entry_embeddings_entry ON entry_embeddings(entry_id)"
  ]

------------------------------------------------------------------------
-- Trigger DDL (ltree path maintenance)
------------------------------------------------------------------------

triggerDDL :: [Text]
triggerDDL =
  [ computeEntryPathFn
  , updateEntryPathFn
  , triggerEntryPathInsert
  , cascadeEntryPathFn
  , triggerEntryPathUpdate
  ]

computeEntryPathFn :: Text
computeEntryPathFn =
  "CREATE OR REPLACE FUNCTION compute_entry_path(\
  \  entry_id TEXT, entry_parent_id TEXT\
  \) RETURNS ltree AS $$\
  \DECLARE parent_path ltree; safe_id TEXT;\
  \BEGIN\
  \  safe_id := regexp_replace(replace(entry_id, '-', '_'), '[^A-Za-z0-9_]', '', 'g');\
  \  IF safe_id = '' THEN safe_id := 'x'; END IF;\
  \  IF entry_parent_id IS NULL THEN\
  \    RETURN safe_id::ltree;\
  \  ELSE\
  \    SELECT path INTO parent_path FROM entries WHERE id = entry_parent_id;\
  \    RETURN parent_path || safe_id;\
  \  END IF;\
  \END;\
  \$$ LANGUAGE plpgsql"

updateEntryPathFn :: Text
updateEntryPathFn =
  "CREATE OR REPLACE FUNCTION update_entry_path() RETURNS trigger AS $$\
  \BEGIN\
  \  NEW.path = compute_entry_path(NEW.id, NEW.parent_id);\
  \  RETURN NEW;\
  \END;\
  \$$ LANGUAGE plpgsql"

triggerEntryPathInsert :: Text
triggerEntryPathInsert =
  "DROP TRIGGER IF EXISTS trg_entry_path_insert ON entries;\
  \CREATE TRIGGER trg_entry_path_insert\
  \  BEFORE INSERT ON entries\
  \  FOR EACH ROW EXECUTE FUNCTION update_entry_path()"

cascadeEntryPathFn :: Text
cascadeEntryPathFn =
  "CREATE OR REPLACE FUNCTION cascade_entry_path() RETURNS trigger AS $$\
  \BEGIN\
  \  IF OLD.parent_id IS DISTINCT FROM NEW.parent_id THEN\
  \    NEW.path = compute_entry_path(NEW.id, NEW.parent_id);\
  \    WITH RECURSIVE descendants AS (\
  \      SELECT id, parent_id FROM entries WHERE parent_id = NEW.id\
  \      UNION ALL\
  \      SELECT e.id, e.parent_id FROM entries e\
  \        JOIN descendants d ON e.parent_id = d.id\
  \    )\
  \    UPDATE entries e\
  \      SET path = compute_entry_path(e.id, e.parent_id)\
  \      FROM descendants d WHERE e.id = d.id;\
  \  END IF;\
  \  RETURN NEW;\
  \END;\
  \$$ LANGUAGE plpgsql"

triggerEntryPathUpdate :: Text
triggerEntryPathUpdate =
  "DROP TRIGGER IF EXISTS trg_entry_path_update ON entries;\
  \CREATE TRIGGER trg_entry_path_update\
  \  BEFORE UPDATE OF parent_id ON entries\
  \  FOR EACH ROW EXECUTE FUNCTION cascade_entry_path()"
