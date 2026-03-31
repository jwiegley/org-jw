{-# LANGUAGE OverloadedStrings #-}

module Org.DB.Schema (
  initDB,
  schemaVersion,
) where

import Data.Text (Text)
import Org.DB.Types

-- | Current schema version. Bump when changing table definitions.
schemaVersion :: Int
schemaVersion = 1

-- | Initialize the database schema, creating all tables if they don't exist.
initDB :: DBHandle -> IO ()
initDB db = dbTransaction db $ do
  mapM_ (\ddl -> dbExecute_ db ddl []) schemaDDL
  -- Ensure schema_version has a row
  existing <- dbQuery db "SELECT version FROM schema_version" [] :: IO [[SqlValue]]
  case existing of
    [] -> dbExecute_ db "INSERT INTO schema_version (version) VALUES (?)" [SqlInt (fromIntegral schemaVersion)]
    _ -> dbExecute_ db "UPDATE schema_version SET version = ?" [SqlInt (fromIntegral schemaVersion)]

------------------------------------------------------------------------
-- Schema DDL
------------------------------------------------------------------------

schemaDDL :: [Text]
schemaDDL =
  [ createSchemaVersion
  , createFiles
  , createFileProperties
  , createEntries
  , createEntryTags
  , createEntryProperties
  , createEntryStamps
  , createLogEntries
  , createBodyBlocks
  , createRelationships
  , createCategories
  , createLinks
  , -- Indexes
    createIdxEntriesFileId
  , createIdxEntriesParentId
  , createIdxEntriesKeyword
  , createIdxEntryTagsEntryId
  , createIdxEntryTagsTag
  , createIdxEntryPropertiesEntryId
  , createIdxEntryStampsEntryId
  , createIdxLogEntriesEntryId
  , createIdxBodyBlocksEntryId
  , createIdxRelationshipsSourceId
  , createIdxRelationshipsTargetId
  , createIdxCategoriesFileId
  , createIdxLinksEntryId
  , createIdxFilePropertiesFileId
  ]

------------------------------------------------------------------------
-- Table definitions
------------------------------------------------------------------------

createSchemaVersion :: Text
createSchemaVersion =
  "CREATE TABLE IF NOT EXISTS schema_version (\
  \  version INTEGER NOT NULL\
  \)"

createFiles :: Text
createFiles =
  "CREATE TABLE IF NOT EXISTS files (\
  \  id TEXT PRIMARY KEY,\
  \  path TEXT NOT NULL UNIQUE,\
  \  mtime TEXT NOT NULL,\
  \  hash BLOB NOT NULL\
  \)"

createFileProperties :: Text
createFileProperties =
  "CREATE TABLE IF NOT EXISTS file_properties (\
  \  file_id TEXT NOT NULL REFERENCES files(id) ON DELETE CASCADE,\
  \  name TEXT NOT NULL,\
  \  value TEXT NOT NULL,\
  \  inherited INTEGER NOT NULL DEFAULT 0,\
  \  PRIMARY KEY (file_id, name)\
  \)"

createEntries :: Text
createEntries =
  "CREATE TABLE IF NOT EXISTS entries (\
  \  entry_id TEXT PRIMARY KEY,\
  \  file_id TEXT NOT NULL REFERENCES files(id) ON DELETE CASCADE,\
  \  parent_id TEXT REFERENCES entries(entry_id) ON DELETE CASCADE,\
  \  depth INTEGER NOT NULL,\
  \  keyword TEXT,\
  \  keyword_closed INTEGER NOT NULL DEFAULT 0,\
  \  priority TEXT,\
  \  headline TEXT NOT NULL,\
  \  verb TEXT,\
  \  title TEXT NOT NULL,\
  \  context TEXT,\
  \  locator TEXT,\
  \  file_line INTEGER NOT NULL,\
  \  path TEXT NOT NULL\
  \)"

createEntryTags :: Text
createEntryTags =
  "CREATE TABLE IF NOT EXISTS entry_tags (\
  \  entry_id TEXT NOT NULL REFERENCES entries(entry_id) ON DELETE CASCADE,\
  \  tag TEXT NOT NULL,\
  \  PRIMARY KEY (entry_id, tag)\
  \)"

createEntryProperties :: Text
createEntryProperties =
  "CREATE TABLE IF NOT EXISTS entry_properties (\
  \  entry_id TEXT NOT NULL REFERENCES entries(entry_id) ON DELETE CASCADE,\
  \  name TEXT NOT NULL,\
  \  value TEXT NOT NULL,\
  \  inherited INTEGER NOT NULL DEFAULT 0,\
  \  file_line INTEGER NOT NULL,\
  \  PRIMARY KEY (entry_id, name)\
  \)"

createEntryStamps :: Text
createEntryStamps =
  "CREATE TABLE IF NOT EXISTS entry_stamps (\
  \  entry_id TEXT NOT NULL REFERENCES entries(entry_id) ON DELETE CASCADE,\
  \  stamp_type TEXT NOT NULL,\
  \  time_start TEXT NOT NULL,\
  \  time_end TEXT,\
  \  time_kind TEXT NOT NULL,\
  \  file_line INTEGER NOT NULL\
  \)"

createLogEntries :: Text
createLogEntries =
  "CREATE TABLE IF NOT EXISTS log_entries (\
  \  entry_id TEXT NOT NULL REFERENCES entries(entry_id) ON DELETE CASCADE,\
  \  log_type TEXT NOT NULL,\
  \  log_time TEXT NOT NULL,\
  \  from_state TEXT,\
  \  to_state TEXT,\
  \  old_time TEXT,\
  \  duration_mins INTEGER,\
  \  file_line INTEGER NOT NULL\
  \)"

createBodyBlocks :: Text
createBodyBlocks =
  "CREATE TABLE IF NOT EXISTS body_blocks (\
  \  entry_id TEXT NOT NULL REFERENCES entries(entry_id) ON DELETE CASCADE,\
  \  block_type TEXT NOT NULL,\
  \  content TEXT NOT NULL,\
  \  seq_num INTEGER NOT NULL,\
  \  file_line INTEGER NOT NULL\
  \)"

createRelationships :: Text
createRelationships =
  "CREATE TABLE IF NOT EXISTS relationships (\
  \  source_id TEXT NOT NULL REFERENCES entries(entry_id) ON DELETE CASCADE,\
  \  target_id TEXT NOT NULL,\
  \  rel_type TEXT NOT NULL,\
  \  PRIMARY KEY (source_id, target_id, rel_type)\
  \)"

createCategories :: Text
createCategories =
  "CREATE TABLE IF NOT EXISTS categories (\
  \  file_id TEXT NOT NULL REFERENCES files(id) ON DELETE CASCADE,\
  \  category TEXT NOT NULL,\
  \  PRIMARY KEY (file_id)\
  \)"

createLinks :: Text
createLinks =
  "CREATE TABLE IF NOT EXISTS links (\
  \  entry_id TEXT NOT NULL REFERENCES entries(entry_id) ON DELETE CASCADE,\
  \  link_type TEXT NOT NULL,\
  \  target TEXT NOT NULL,\
  \  description TEXT\
  \)"

------------------------------------------------------------------------
-- Index definitions
------------------------------------------------------------------------

createIdxEntriesFileId :: Text
createIdxEntriesFileId =
  "CREATE INDEX IF NOT EXISTS idx_entries_file_id ON entries(file_id)"

createIdxEntriesParentId :: Text
createIdxEntriesParentId =
  "CREATE INDEX IF NOT EXISTS idx_entries_parent_id ON entries(parent_id)"

createIdxEntriesKeyword :: Text
createIdxEntriesKeyword =
  "CREATE INDEX IF NOT EXISTS idx_entries_keyword ON entries(keyword)"

createIdxEntryTagsEntryId :: Text
createIdxEntryTagsEntryId =
  "CREATE INDEX IF NOT EXISTS idx_entry_tags_entry_id ON entry_tags(entry_id)"

createIdxEntryTagsTag :: Text
createIdxEntryTagsTag =
  "CREATE INDEX IF NOT EXISTS idx_entry_tags_tag ON entry_tags(tag)"

createIdxEntryPropertiesEntryId :: Text
createIdxEntryPropertiesEntryId =
  "CREATE INDEX IF NOT EXISTS idx_entry_properties_entry_id ON entry_properties(entry_id)"

createIdxEntryStampsEntryId :: Text
createIdxEntryStampsEntryId =
  "CREATE INDEX IF NOT EXISTS idx_entry_stamps_entry_id ON entry_stamps(entry_id)"

createIdxLogEntriesEntryId :: Text
createIdxLogEntriesEntryId =
  "CREATE INDEX IF NOT EXISTS idx_log_entries_entry_id ON log_entries(entry_id)"

createIdxBodyBlocksEntryId :: Text
createIdxBodyBlocksEntryId =
  "CREATE INDEX IF NOT EXISTS idx_body_blocks_entry_id ON body_blocks(entry_id)"

createIdxRelationshipsSourceId :: Text
createIdxRelationshipsSourceId =
  "CREATE INDEX IF NOT EXISTS idx_relationships_source_id ON relationships(source_id)"

createIdxRelationshipsTargetId :: Text
createIdxRelationshipsTargetId =
  "CREATE INDEX IF NOT EXISTS idx_relationships_target_id ON relationships(target_id)"

createIdxCategoriesFileId :: Text
createIdxCategoriesFileId =
  "CREATE INDEX IF NOT EXISTS idx_categories_file_id ON categories(file_id)"

createIdxLinksEntryId :: Text
createIdxLinksEntryId =
  "CREATE INDEX IF NOT EXISTS idx_links_entry_id ON links(entry_id)"

createIdxFilePropertiesFileId :: Text
createIdxFilePropertiesFileId =
  "CREATE INDEX IF NOT EXISTS idx_file_properties_file_id ON file_properties(file_id)"
