# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**Tech Stack:** Haskell (GHC 9.10) • Multi-package Cabal project • hpack • Nix + haskell.nix • FlatParse • lens • Hakyll

---

## Quick Reference

```bash
# Build everything
cabal build all

# Run tests (round-trip validation)
make all

# Test specific file
echo "path/to/file.org" | cabal run org-jw:exe:org -- \
  --config ~/org/org.yaml --keywords ~/org/org.dot \
  lint --round-trip -F -

# Regenerate .cabal from package.yaml
cd <package> && hpack -f

# Enter Nix development environment
nix develop
```

---

## ⚠️ Critical Gotchas

**1. NEVER edit .cabal files directly**
- Always edit `package.yaml`, then run `hpack -f`
- Both files are committed; they must stay in sync
- Build will break if .cabal doesn't match package.yaml

**2. Round-trip must pass before committing**
- Run `make all` - it must succeed
- This project has NO traditional tests
- Validation is 100% via round-trip: parse → print → parse must be identical
- Debug failures: `make trip` shows diffs

**3. Changes to org-types trigger full rebuild**
- 10 packages depend on org-types (directly or transitively)
- Expect 5-10 minute rebuild when modifying org-types
- Batch changes to org-types; test incrementally

**4. All lenses go in org-data, NOT org-types**
- org-types: pure data types only
- org-data: ALL lens derivations (makeClassy, makeLenses, makePrisms)
- This separation is architectural; don't add Template Haskell to org-types

**5. Profiling is intentional**
- All packages compile with `-fprof-auto`
- Runtime uses `+RTS -N` for parallelism
- Do NOT remove profiling flags - author relies on profiling data

**6. FlatParse ≠ megaparsec/attoparsec**
- No automatic backtracking
- Explicit error handling required
- Check `flatparse-util/` for existing combinators before writing new ones

**7. Parser and printer must stay in sync**
- Changing org-parse requires updating org-print
- Otherwise round-trip fails
- Test after every parser/printer change

---

## Package Architecture

### Dependency Graph (Topological Order)

```
Layer 0 (base - no local dependencies):
  ├─ org-types        Core AST types (Entry, Header, Body, Property, etc.)
  └─ flatparse-util   Parser combinators for FlatParse

Layer 1 (parsing & printing):
  ├─ org-parse  → org-types, flatparse-util
  └─ org-print  → org-types

Layer 2 (lens API hub):
  └─ org-data   → org-types, org-parse, org-print
                  Re-exports everything + lens API

Layer 3 (feature packages - all depend on org-data):
  ├─ org-lint       40+ validation rules, round-trip checking
  ├─ org-json       Export to JSON
  ├─ org-cbor       Export to CBOR
  ├─ org-filetags   Tag-based filtering
  └─ org-site       Hakyll static site generator

Layer 4 (executable):
  └─ org-jw → ALL packages
              CLI that orchestrates all functionality
```

### Impact Radius

| Change to...      | Rebuilds...                                    | Package Count |
|-------------------|------------------------------------------------|---------------|
| org-types         | Everything (entire dependency tree)            | 10 packages   |
| flatparse-util    | org-parse, org-data, and downstream            | 8 packages    |
| org-parse         | org-data and all feature packages              | 7 packages    |
| org-print         | org-data and all feature packages              | 7 packages    |
| org-data          | All feature packages + org-jw                  | 6 packages    |
| org-lint          | Only org-jw                                    | 1 package     |
| org-jw            | Just the executable                            | 1 package     |

### Package Responsibilities

- **org-types**: Pure AST data types. No parsing, no lenses, minimal dependencies.
- **org-parse**: FlatParse-based parser. Input: ByteString → Output: AST. No validation.
- **org-print**: Pretty printer. Input: AST → Output: Text. Preserves exact formatting.
- **org-data**: Lens API layer. Re-exports types/parse/print + all lens definitions. This is the "public API."
- **org-lint**: Validation rules only. Uses org-data's lens API extensively.
- **org-jw**: CLI orchestration. Uses optparse-applicative. Delegates to feature packages.

---

## Testing Strategy: The Round-Trip Property

**Property:** ∀ file. `parse(print(parse(file))) == parse(file)`

**Why this works:**
- Tests parser correctness
- Tests printer correctness
- Tests AST completeness (no information loss)
- Tests on real-world data (actual .org files)

**There are NO traditional unit tests.** The entire test suite is round-trip validation.

### Running Tests

```bash
# Primary test - runs on entire ~/org directory
make all

# Test specific file with diff output
cabal run org-jw:exe:org -- \
  --config ~/org/org.yaml \
  trip -f path/to/file.org

# Update files in place (after intentional format changes)
make trip-update
```

### Debugging Round-Trip Failures

**Symptom:** `make all` reports failures

**Common causes:**
- Parser skipped whitespace/formatting details
- Printer uses different formatting than input
- AST doesn't represent an edge case
- Property drawer ordering changed

**Debug process:**
1. Run `trip` command on failing file to see diff
2. Check if parser missed something (inspect AST)
3. Check if printer output differs from original
4. Determine if intentional change or bug

**Fix:**
- If intentional format change: `make trip-update`
- If bug: fix parser or printer to preserve format
- If AST limitation: extend types in org-types

---

## Development Workflows

### Adding a New Lint Rule

**File:** `org-lint/src/Org/Lint.hs`

1. Add error code to `LintMessageCode` sum type
2. Add severity in `lintMessageKind` function
3. Add formatter in `formatLintMessage` function
4. Write rule function: `checkMyRule :: Entry -> [LintMessage]`
5. Add to `lintEntry` or `lintFile` function
6. Rebuild: `cd org-lint && hpack -f && cabal build org-lint`
7. Test: `make lint`

**Example:**
```haskell
checkMissingProperty :: String -> Entry -> [LintMessage]
checkMissingProperty propName entry =
  case entry ^? entryProperties . folded . filtered (\p -> p ^. propertyName == propName) of
    Nothing -> [LintMessage LintError (TodoMissingProperty propName)]
    Just _  -> []
```

### Modifying the Parser

**File:** `org-parse/src/Org/Parse.hs`

⚠️ **High impact** - affects all downstream packages

1. If AST changes needed: edit `org-types/src/Org/Types.hs` first
2. Update parser in `org-parse/src/Org/Parse.hs`
3. Update printer in `org-print/src/Org/Print.hs` (critical for round-trip!)
4. Update lenses if needed in `org-data/src/Org/Data.hs`
5. Rebuild dependency chain: `cabal build org-types org-parse org-print org-data`
6. **Test round-trip:** `make trip`
7. Debug failures with diff output

**FlatParse gotchas:**
- No automatic backtracking - order alternatives carefully
- Explicit whitespace handling: `many space`, `skipWhile isSpace`
- Use `<?>` operator for debugging context
- Check `flatparse-util/` for existing combinators

### Changing org-types

**File:** `org-types/src/Org/Types.hs`

⚠️ **Maximum impact** - rebuilds 10 packages (~5-10 minutes)

1. Modify data type (use `Maybe` for new optional fields)
2. Update `org-data/src/Org/Data.hs` - `makeLenses` auto-generates new lenses
3. Update parser in `org-parse` to handle new field
4. Update printer in `org-print` to output new field
5. Find and update all usage sites: `grep -r "entryHeader\|entryBody" org-lint/ org-json/`
6. Full rebuild: `cabal build all`
7. **Critical:** Test round-trip with `make all`

---

## Build System

### Multi-Package Setup

This is a Cabal multi-package project (workspace). All packages are listed in `cabal.project`.

**Build commands:**
```bash
cabal build all              # Build all packages
cabal build org-lint         # Build single package
cabal test all               # No tests defined (uses round-trip instead)
```

### hpack Workflow

**All packages use hpack** - `.cabal` files are generated from `package.yaml`:

```bash
# Edit dependencies or options
vim org-lint/package.yaml

# Regenerate .cabal
cd org-lint && hpack -f

# Or use Makefile target
make org-lint/org-lint.cabal
```

**Both `package.yaml` and `.cabal` are committed to git.** They must stay in sync.

### Nix + direnv

**Development environment:**
```bash
# Enter Nix shell manually
nix develop

# Or use direnv (automatic)
direnv allow    # First time setup
cd .            # Triggers environment reload
```

**GHC version:** Locked to 9.10 via `flake.nix`. Do not add dependencies incompatible with GHC 9.10.

**Custom Hakyll:** Uses a forked version specified in `cabal.project` as `source-repository-package`.

---

## Lens API Patterns

**org-data provides the lens-based query API** over org-types AST.

### Three Lens Patterns

**1. makeClassy - For config/environment types**
```haskell
makeClassy ''Config
-- Generates: class HasConfig a where config :: Lens' a Config
-- Use: Functions can require (MonadReader r m, HasConfig r)
```

**2. makeLenses - For record types**
```haskell
data Entry = Entry { _entryHeader :: Header, ... }
makeLenses ''Entry
-- Generates: entryHeader :: Lens' Entry Header
-- Note: Field has underscore prefix, lens does not
```

**3. makePrisms - For sum types**
```haskell
data Block = Para Text | Drawer DrawerData | ...
makePrisms ''Block
-- Generates: _Para :: Prism' Block Text, _Drawer :: Prism' Block DrawerData
```

### Common Traversal Patterns

```haskell
-- All TODO entries
coll ^.. entries . filtered (\e -> e ^. entryKeyword == Just "TODO")

-- Deep traversal (Plated instance)
coll ^.. entries . cosmos

-- All property values named "ID"
coll ^.. entries . cosmos . entryProperties . folded
     . filtered (\p -> p ^. propertyName == "ID") . propertyValue
```

**Where to add lenses:** ALL lens definitions go in `org-data/src/Org/Data.hs`, never in org-types.

---

## CLI Commands

The main executable is `org`. All commands require `--config` (YAML file defining keywords, priorities, etc.).

### Command Structure

```bash
cabal run org-jw:exe:org -- \
  --config ~/org/org.yaml \
  [--keywords ~/org/org.dot] \
  COMMAND [OPTIONS] \
  -F - or -f FILE
```

**Input modes:**
- `-f -`: Single file from stdin
- `-F -`: File list from stdin (one path per line)
- `-F <file>`: File list from file
- `<path>...`: Direct file paths

### Available Commands

- `parse`: Parse files and report count
- `lint`: Run validation rules (`--round-trip` for round-trip check)
- `trip`: Round-trip test with diff output (`--change-in-place` to update files)
- `json`: Export to JSON (`--output <dir>`)
- `stats`: Generate statistics
- `filetags`: Tag-based filtering
- `site`: Build Hakyll static site (`rebuild <config>`)

### Makefile Targets

```bash
make all           # Build + lint with round-trip check
make lint          # Lint only
make json          # Export to JSON
make trip          # Round-trip test
make trip-update   # Update files in place
make stats         # Statistics
make newartisans   # Build static site
```

---

## Configuration

The `--config` YAML file defines:
- `startKeywords`, `openKeywords`, `closedKeywords`: TODO state keywords
- `keywordTransitions`: Valid state transitions
- `priorities`: Priority markers (A, B, C, etc.)
- `propertyColumn`, `tagsColumn`: Formatting column positions
- `attachmentsDir`: Attachment directory path

The optional `--keywords` DOT file contains keyword relationships in Graphviz format.
