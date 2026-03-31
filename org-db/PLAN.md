# org-db: PostgreSQL Migration & Full PRD Implementation Plan

## Overview

Migrate org-db from dual SQLite/PostgreSQL to PostgreSQL-only, align schema with
PRD 02, and implement all remaining PRD features. This document tracks every work
item from immediate migration through deferred features.

---

## Phase 1: Remove SQLite — COMPLETE

Strip all SQLite code. Project must build with no schema changes yet.

- [x] Types.hs: `DBConfig` → `newtype DBConfig = DBConfig { pgConnString :: ByteString }`
- [x] Types.hs: Remove `dbBackend` field from `DBHandle`
- [x] Options.hs: `_dbPath` → `_dbConnStr`, `--db-url` flag, default `"dbname=org_jw"`
- [x] Exec.hs: Use `DBConfig (BS8.pack ...)` instead of `SQLiteConfig`
- [x] Connection.hs: Delete all SQLite code
- [x] Connection.hs: Simplify `connectDB` to only PostgreSQL path
- [x] package.yaml: Remove `direct-sqlite`, `sqlite-simple` deps
- [x] Build verification: `cabal build all`
- [x] Committed

---

## Phase 2: PRD Schema — COMPLETE

Complete rewrite of Schema.hs to match PRD section 3: ltree + vector extensions,
12 tables with TIMESTAMPTZ/BOOLEAN/BIGSERIAL, CHECK constraints, GiST index on
entries(path), trigger functions for ltree path maintenance.

---

## Phase 3: Row Types — COMPLETE

All 11 row types and FromRow instances updated to match new schema columns,
including MJD integer time storage, suffix fields, per-entry categories, and
byte_offset tracking.

---

## Phase 4: Store.hs Rewrite — COMPLETE

All INSERT SQL rewritten with MJD time helpers, LogBook containment via
INSERT...RETURNING id, per-entry category insertion, and position tracking.

---

## Phase 5: Query.hs Updates — COMPLETE

SQL compiler adapted for PostgreSQL: table/column renames, ILIKE, MJD integer
date comparisons, path::text cast for ltree, entry_categories per-entry join.

---

## Phase 6: Connection Cleanup — COMPLETE

Inlined connectPostgres into connectDB, removed unused ByteString import and
OverloadedStrings pragma.

---

## Phase 7: Final Verification — COMPLETE

- [x] `hpack -f` for org-db
- [x] `cabal build all`
- [x] `fourmolu -m check` + `hlint` on all changed files
- [x] `make all` (round-trip tests pass — failures are pre-existing slug mismatches)
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
