# org-jw

I've been using Emacs Org-mode for years to manage everything -- tasks, notes,
journal entries, even my blog. At some point I realized that certain operations
I wanted to perform on my Org files were either too slow or too awkward to do
from inside Emacs. Bulk validation across hundreds of files, exporting to JSON,
building a static site -- these are better handled by a dedicated tool.

So I wrote one, in Haskell.

This repository contains a FlatParse-based parser for Org-mode files, a
formatting-preserving printer, and several tools built on top of them. The
parser is fast, the printer reproduces the original formatting exactly, and the
whole thing is validated by a simple but powerful property: for every Org file,
parsing it, printing the result, and parsing again must yield an identical AST.
There are no unit tests -- just this one round-trip check, run against my entire
Org directory.

Fair warning: most of this code is tailored to managing *my* Org files. It
handles the keywords, properties, and conventions I use every day. If your Org
setup differs significantly from mine, you'll likely need to adapt things.

## Packages

The project is a multi-package Cabal workspace:

| Package | What it does |
|---|---|
| **org-types** | Core AST data types |
| **flatparse-util** | Parser combinators for FlatParse |
| **org-parse** | FlatParse-based Org-mode parser |
| **org-print** | Pretty printer that preserves original formatting |
| **org-data** | Lens API layer; re-exports types, parser, and printer |
| **org-lint** | 40+ validation rules |
| **org-json** | JSON export |
| **org-cbor** | CBOR export |
| **org-filetags** | Tag-based file filtering |
| **org-site** | Hakyll-based static site generator |
| **org-jw** | CLI that ties everything together |

## Building

You'll need GHC 9.10 and Cabal. The easiest way to get a working environment
is through Nix:

```bash
nix develop
```

Then build everything:

```bash
cabal build all
```

## Testing

The entire test strategy is a round-trip property:

```
for all file: parse(print(parse(file))) == parse(file)
```

Run it against your Org directory with:

```bash
make all
```

If the round-trip breaks, something is wrong with the parser, the printer, or
both. Debug with `make trip` to see the diffs.

## Development

Pre-commit hooks handle formatting, linting, and build verification. Set them
up with:

```bash
lefthook install
```

Here's what runs on every commit:

- **fourmolu** -- code formatting check
- **hlint** -- Haskell linting
- **cabal build** -- full build with warnings as errors

These also run in CI via GitHub Actions. Heavier checks run on pre-push:

- **round-trip** -- full validation against `~/org`
- **coverage** -- HPC coverage regression check
- **performance** -- timing regression check (5% threshold)

### Makefile targets

```bash
make format         # Format all .hs files with fourmolu
make format-check   # Check formatting without modifying files
make hlint          # Run hlint
make build-werror   # Build with -Werror
make coverage       # Generate HPC coverage report
make profile        # Generate profiling report
make fuzz           # Run fuzz testing
make haddock        # Build Haddock documentation
make all            # Full round-trip validation
```

## License

BSD-3-Clause. See [LICENSE.md](LICENSE.md).
