# org-db: PostgreSQL Migration & Full PRD Implementation Plan

## Overview

Migrate org-db from dual SQLite/PostgreSQL to PostgreSQL-only, align schema with
PRD 02, and implement all remaining PRD features. This document tracks every work
item from immediate migration through deferred features.

---

## Phase 1: Remove SQLite (Commit 1) — IN PROGRESS

Strip all SQLite code. Project must build with no schema changes yet.

### Status: Partially done

- [x] Types.hs: `DBConfig` → `newtype DBConfig = DBConfig { pgConnString :: ByteString }`
- [x] Types.hs: Remove `dbBackend` field from `DBHandle`
- [x] Options.hs: `_dbPath` → `_dbConnStr`, `--db-url` flag, default `"dbname=org_jw"`
- [x] Exec.hs: Use `DBConfig (BS8.pack ...)` instead of `SQLiteConfig`
- [x] Connection.hs: Delete all SQLite code (toDirectParams, toDirectData, directToSqlValue, sqliteQueryRows, sqliteRawQuery, connectSQLite, SQLite imports)
- [x] Connection.hs: Remove `BangPatterns` extension, `Data.Time.Format` import
- [x] Connection.hs: Simplify `connectDB` to only PostgreSQL path
- [x] Connection.hs: Remove `dbBackend = PostgresConfig connStr` from connectPostgres
- [x] package.yaml: Remove `direct-sqlite`, `sqlite-simple` deps
- [x] Run `hpack -f`
- [x] Build verification: `cabal build all`
- [ ] Commit

---

## Phase 2: PRD Schema (Commit 2, part 1)

Complete rewrite of Schema.hs to match PRD section 3.

- [ ] Add `CREATE EXTENSION IF NOT EXISTS ltree`
- [ ] Add `CREATE EXTENSION IF NOT EXISTS vector`
- [ ] Rewrite `schema_version`: version PK, applied_at TIMESTAMPTZ, description TEXT
- [ ] Rewrite `files`: add title, preamble, mod_time TIMESTAMPTZ, created_time, created_at/updated_at DEFAULT now(); hash as TEXT not BLOB
- [ ] Rewrite `file_properties`: add source CHECK ('drawer','file'), PK (file_id, name, source), remove inherited
- [ ] Rewrite `entries`: keyword_type/keyword_value (not keyword/keyword_closed), position, byte_offset, hash, mod_time, created_time, path ltree, created_at/updated_at
- [ ] Rewrite `entry_tags`: add is_inherited BOOLEAN, source_id TEXT REFERENCES entries(id)
- [ ] Rewrite `entry_properties`: is_inherited BOOLEAN, byte_offset (not file_line)
- [ ] Rewrite `entry_stamps`: BIGSERIAL id, MJD integers (day, day_end, time_start, time_end as INTEGER), byte_offset, suffix columns (suffix_kind, suffix_num, suffix_span, suffix_larger_num, suffix_larger_span), CHECK constraints on stamp_type/time_kind
- [ ] Rewrite `entry_log_entries` (rename from log_entries): BIGSERIAL id, position, byte_offset, MJD time fields, from_keyword/from_keyword_type/to_keyword/to_keyword_type, orig time MJD fields, body_text, logbook_id self-ref, CHECK on log_type
- [ ] Rewrite `entry_body_blocks` (rename from body_blocks): BIGSERIAL id, position (not seq_num), byte_offset, drawer_type CHECK, drawer_name, UNIQUE (entry_id, position)
- [ ] Rewrite `entry_relationships` (rename from relationships): source_entry_id/target_entry_id/relationship_type, context, created_at, CHECK constraints
- [ ] Rewrite `entry_categories` (rename from categories): per-entry (entry_id not file_id), is_explicit BOOLEAN, source_id, PK (entry_id, category)
- [ ] Rewrite `entry_links` (rename from links): BIGSERIAL id, position
- [ ] Add PRD indexes: GiST on entries(path), partial on keyword_value, all others
- [ ] Add trigger functions: compute_entry_path, update_entry_path (BEFORE INSERT), cascade_entry_path (BEFORE UPDATE OF parent_id)
- [ ] Update `initDB` for new schema_version structure

---

## Phase 3: Row Types (Commit 2, part 2)

Update all row types in Types.hs to match new schema columns.

- [ ] FileRow: add frTitle, frPreamble, frCreatedTime, frCreatedAt, frUpdatedAt; frMtime → frModTime (Maybe UTCTime); frHash → Maybe Text
- [ ] FilePropertyRow: fprInherited → fprSource :: Text
- [ ] EntryRow: erEntryId → erId; keyword/keywordClosed → keywordType/keywordValue; add position, byteOffset, hash, modTime, createdTime, createdAt, updatedAt; erPath → Maybe Text (ltree as text)
- [ ] EntryTagRow: add etrIsInherited, etrSourceId
- [ ] EntryPropertyRow: inherited → isInherited; fileLine → byteOffset
- [ ] EntryStampRow: MAJOR — add esrId (Int64); UTCTime → MJD integers (esrDay, esrDayEnd, esrTimeStart, esrTimeEnd as Int/Maybe Int); add byte_offset; add suffix fields
- [ ] LogEntryRow: MAJOR — add lerId, lerPosition, lerByteOffset; UTCTime → MJD; keyword type fields; orig time MJD fields; lerBodyText; lerLogbookId; split duration
- [ ] BodyBlockRow: add bbrId, bbrByteOffset, bbrDrawerType, bbrDrawerName; seqNum → position
- [ ] RelationshipRow: rename fields to sourceEntryId/targetEntryId/relationshipType; add context, createdAt
- [ ] CategoryRow: crFileId → crEntryId; add crIsExplicit, crSourceId
- [ ] LinkRow: add lrId, lrPosition
- [ ] Update ALL FromRow instances (column counts change)

---

## Phase 4: Store.hs Rewrite (Commit 2, part 3)

Rewrite all INSERT SQL and serialization logic.

- [ ] Add `timeToParams :: Time -> [SqlValue]` helper — MJD day, day_end, time_start, time_end, suffix fields (12 values)
- [ ] Add `timeSuffixToParams :: Maybe TimeSuffix -> [SqlValue]` helper
- [ ] Rewrite insertFileSQL for new files columns (title, preamble, mod_time, created_time, hash as TEXT)
- [ ] Rewrite insertFilePropertySQL: source instead of inherited
- [ ] Rewrite insertEntrySQL: keyword_type/keyword_value, position, byte_offset, hash, mod_time, created_time (omit path — trigger fills it)
- [ ] Rewrite insertTagSQL: add is_inherited, source_id
- [ ] Rewrite insertPropertySQL: is_inherited, byte_offset
- [ ] Rewrite insertStampSQL: MJD integers + suffix columns
- [ ] Rewrite insertLogEntrySQL → entry_log_entries: MJD, keyword types, orig time, body_text, logbook_id
- [ ] Rewrite insertBodyBlockSQL → entry_body_blocks: position, byte_offset, drawer_type, drawer_name
- [ ] Rewrite insertLinkSQL → entry_links: add position
- [ ] New insertRelationshipSQL → entry_relationships: new column names, context
- [ ] New insertCategorySQL → entry_categories: entry_id, is_explicit, source_id
- [ ] Rewrite `keywordInfo` → return (Maybe Text, Maybe Text) for (keyword_type, keyword_value)
- [ ] Rewrite `insertEntry`: add position param (use zipWithM_ for sibling order), extract hash/mod_time from properties
- [ ] Rewrite `insertStamp`: direct MJD field access from Time, store TimeSuffix fields
- [ ] Rewrite `insertLogEntry`: LogBook gets own row, children use logbook_id via INSERT...RETURNING id
- [ ] Rewrite `insertBody`/`blockInfo`: separate drawer_type and drawer_name
- [ ] Rewrite `replaceOrgFile`: extract title from #+TITLE:, preamble from header, source='drawer'/'file' for properties
- [ ] Move category insertion from replaceOrgFile to insertEntry (per-entry categories)
- [ ] Rewrite `storeOrgFile`: hash is Text not ByteString, mod_time column name
- [ ] Update all SELECT column lists in query functions (queryFiles, queryEntries, queryEntriesByKeyword, queryEntriesByTag, queryEntryProperties, queryEntryTags, queryEntryStamps)

---

## Phase 5: Query.hs Updates (Commit 2, part 4)

Adapt the 877-line org-ql compiler for PostgreSQL schema.

- [ ] Update `entrySelectSQL` column list for new entries schema
- [ ] Column renames throughout: `entry_id` → `id` (entries table), `keyword` → `keyword_value`, `keyword_closed = 1` → `keyword_type = 'closed'`
- [ ] Table renames: `log_entries` → `entry_log_entries`, `body_blocks` → `entry_body_blocks`, `links` → `entry_links`, `categories` → `entry_categories`
- [ ] Replace `COLLATE NOCASE` with `LOWER(col) = LOWER(?)` (4 occurrences in QProperty/QHabit)
- [ ] Replace `LIKE` with `ILIKE` for heading/path/regexp (case-insensitive matching)
- [ ] Rewrite `compileDateConds` for MJD integer comparisons (not substr text comparison)
- [ ] Update stamp/log subqueries to use MJD day column instead of time_start text column
- [ ] Category subquery: `c__.file_id = a.file_id` → `c__.entry_id = a.id`
- [ ] Verify recursive CTEs still work (they do — PostgreSQL supports WITH RECURSIVE)
- [ ] Build verification: `cabal build all`

---

## Phase 6: Connection Cleanup (Commit 3)

- [ ] Remove dead SQLite imports (already partially done in Phase 1)
- [ ] Inline connectPostgres into connectDB (only one backend)
- [ ] Clean up module exports
- [ ] Build verification

---

## Phase 7: Final Verification (Commit 4)

- [ ] `hpack -f` for org-db
- [ ] `cabal build all`
- [ ] `fourmolu -m check` + `hlint` on all changed files
- [ ] `make all` (round-trip tests — unaffected by DB changes)
- [ ] Manual smoke test with local PostgreSQL

---

## Deferred Phase A: Deserialize.hs (PRD 4.2)

New module: DB rows → Haskell types reconstruction.

- [ ] `rowsToEntry :: EntryRow -> [EntryTagRow] -> [EntryPropertyRow] -> ... -> Entry`
- [ ] `rowsToOrgFile :: FileRow -> [FilePropertyRow] -> [Entry] -> OrgFile`
- [ ] `dbToCollection :: DBHandle -> IO Collection` (bulk-load with batch queries to avoid N+1)
- [ ] MJD integers → Time type reconstruction (reverse of timeToParams)
- [ ] TimeSuffix reconstruction from suffix columns
- [ ] LogEntry reconstruction from log entry rows (including LogBook containment via logbook_id)
- [ ] Body/Block reconstruction from body block rows (drawer_type/drawer_name → DrawerType)
- [ ] Keyword reconstruction from keyword_type/keyword_value
- [ ] Tree assembly: flat entries → nested Entry._entryItems using parent_id
- [ ] Category and tag inheritance reconstruction

---

## Deferred Phase B: Sync.hs (PRD 5.2)

Full bidirectional sync algorithm.

- [ ] `syncToDb :: DBHandle -> Collection -> IO SyncResult` — org files are source of truth
- [ ] `syncFromDb :: DBHandle -> IO Collection` — DB is source of truth
- [ ] `syncBidirectional :: DBHandle -> Collection -> IO SyncResult` — MOD_TIME comparison
- [ ] Per-entry hash comparison (not just per-file)
- [ ] Conflict detection and reporting (DB MOD_TIME > file MOD_TIME)
- [ ] Entry deletion detection (entry in DB but not in parsed file)
- [ ] Incremental sync: only process changed files (mtime + hash check)
- [ ] `ensureEntryId :: DBHandle -> Entry -> IO (Entry, Bool)` — generate UUID for entries without ID property
- [ ] ID writeback to org files (modify file, add ID property)

---

## Deferred Phase C: CLI Commands (PRD 5.1-5.4)

- [ ] `org db init --db-url <url>` — already partially implemented
- [ ] `org db sync --db-url <url> --direction to-db|from-db|bidirectional` — new command
- [ ] `org db sync --entries <ids> --files <paths>` — selective sync
- [ ] `org db dump --db-url <url> --format json|csv|text --table <name>` — new command
- [ ] `org db query --db-url <url> --ql <expr> --sort --limit --format` — enhance existing
- [ ] `org db dot --db-url <url> --output <file.dot> --relationship-types --filter-* --max-depth` — new command

---

## Deferred Phase D: org-dot Package (PRD 5.4)

New package for Graphviz generation from relationships table.

- [ ] Create `org-dot/` package directory
- [ ] `org-dot/package.yaml` with deps: org-types, org-db, graphviz, text
- [ ] `org-dot/src/Org/Dot.hs` — dot file generation
- [ ] Color-code edges by relationship type
- [ ] Filter by file, tag, keyword, subtree
- [ ] Cluster nodes by file
- [ ] Control depth of parent-child edges
- [ ] Add to cabal.project
- [ ] Wire into CLI via org-jw

---

## Deferred Phase E: Performance Optimizations

- [ ] Batch inserts: multi-row INSERT or COPY for bulk loading
- [ ] Statement caching / prepared statements
- [ ] Streaming file hash (incremental MD5 instead of readFile)
- [ ] Connection pool tuning and usage in CLI
- [ ] ltree-based hierarchy queries (replace recursive CTEs where beneficial)
- [ ] FTS5/tsvector for full-text search

---

## Deferred Phase F: Schema Migration

- [ ] Migration framework: numbered migrations checked against schema_version
- [ ] Migration runner in initDB: apply pending migrations in order
- [ ] Rollback support (down migrations)

---

## Deferred Phase G: Isomorphism Tests (PRD 8)

- [ ] Parse → serialize → deserialize → compare (requires Deserialize.hs)
- [ ] Parse → serialize → deserialize → print → compare with original file
- [ ] Both automated via `make db-test` or similar
- [ ] Property-based tests with QuickCheck Arbitrary instances for Entry

---

## Deferred Phase H: Vector Embeddings (PRD 04)

- [ ] pgvector extension already created in schema (Phase 2)
- [ ] Add embedding column to entries table
- [ ] Integration with embedding API
- [ ] Similarity search queries

---

## Notes

- `postgresql-simple` uses `?` placeholders natively (translates to `$N` on wire) — no placeholder migration needed
- org-types `Time._timeDay` is already MJD; `Time._timeStart` is already minutes-into-day — store directly, don't convert to UTCTime
- ltree path is maintained by PostgreSQL triggers — Haskell code omits path on INSERT
- Round-trip tests (`make all`) test org parsing, not DB — unaffected by DB changes
- Pre-commit hooks enforce: fourmolu formatting, hlint, full `cabal build all`
