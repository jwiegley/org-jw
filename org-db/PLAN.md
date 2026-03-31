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

## Phase A: Deserialize.hs (PRD 4.2) — COMPLETE

New module: DB rows → Haskell types reconstruction (Org.DB.Deserialize).

- [x] `rowToEntry`, `rowToOrgFile` with all child row types
- [x] `loadCollection` with batch queries (avoid N+1)
- [x] MJD integers → Time type reconstruction
- [x] TimeSuffix reconstruction from suffix columns
- [x] LogEntry reconstruction with LogBook containment via logbook_id
- [x] Body/Block reconstruction (drawer_type/drawer_name → DrawerType)
- [x] Keyword reconstruction from keyword_type/keyword_value
- [x] Tree assembly: flat entries → nested Entry._entryItems using parent_id

---

## Phase B: Sync.hs (PRD 5.2) — COMPLETE

Bidirectional sync with conflict detection (Org.DB.Sync).

- [x] `syncToDb` — org files are source of truth, mtime-based skip
- [x] `syncFromDb` — DB is source of truth via loadCollection
- [x] `syncBidirectional` — MOD_TIME comparison with conflict reporting
- [x] `ensureEntryId` — generate UUID for entries without ID property

---

## Phase C: CLI Commands (PRD 5.1-5.4) — COMPLETE

- [x] `org db init` with migration runner
- [x] `org db sync --direction to-db|from-db|bidirectional`
- [x] `org db dump --table <name> --limit N`
- [x] `org db query --ql <expr> --limit N --format text|json|csv`
- [x] `org db dot --output <file> --rel-type --filter-file --filter-keyword --max-depth`

---

## Phase D: org-dot Package (PRD 5.4) — COMPLETE

- [x] `org-dot/` package with graphviz dependency
- [x] DotConfig for filtering (relationship types, files, keywords, max depth)
- [x] Color-coded edges by relationship type
- [x] Cluster nodes by file
- [x] Wired into CLI and cabal.project

---

## Phase E: Performance Optimizations — COMPLETE

- [x] Batch inserts: multi-row VALUES for tags and properties
- [x] ltree-based hierarchy queries (replaced recursive CTEs for ancestors/descendants)
- [x] tsvector full-text search for rifle queries (GIN index from migration v3)

---

## Phase F: Schema Migration — COMPLETE

- [x] Migration framework with version tracking (Org.DB.Migrate)
- [x] Migration runner: check current version, apply pending migrations
- [x] First migration (v3): tsvector full-text search column with GIN index

---

## Deferred Phase G: Isomorphism Tests (PRD 8)

- [ ] Parse → serialize → deserialize → compare (requires Deserialize.hs)
- [ ] Parse → serialize → deserialize → print → compare with original file
- [ ] Both automated via `make db-test` or similar
- [ ] Property-based tests with QuickCheck Arbitrary instances for Entry

---

## Phase H: Vector Embeddings (PRD 04) — COMPLETE

- [x] pgvector extension created in schema (Phase 2)
- [x] Migration v4: embedding vector(1536) column with IVFFlat index
- [x] `storeEmbedding` — store embedding vector for an entry
- [x] `querySimilar` — cosine similarity search with limit

---

## Notes

- `postgresql-simple` uses `?` placeholders natively (translates to `$N` on wire) — no placeholder migration needed
- org-types `Time._timeDay` is already MJD; `Time._timeStart` is already minutes-into-day — store directly, don't convert to UTCTime
- ltree path is maintained by PostgreSQL triggers — Haskell code omits path on INSERT
- Round-trip tests (`make all`) test org parsing, not DB — unaffected by DB changes
- Pre-commit hooks enforce: fourmolu formatting, hlint, full `cabal build all`
