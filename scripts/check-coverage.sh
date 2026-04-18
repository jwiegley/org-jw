#!/usr/bin/env bash
set -euo pipefail

# Check that HPC code coverage does not regress.
#
# This script builds with -fhpc, runs the round-trip test, generates a
# coverage report, and compares against a stored baseline. If coverage
# drops, the check fails.
#
# The baseline file (.coverage-baseline) is local and gitignored since
# it depends on the set of org files available on the machine.

BASELINE_FILE=".coverage-baseline"
COVERAGE_BUILDDIR="dist-coverage"
ORG_YAML="${ORG_YAML:-$HOME/org/org.yaml}"
ORG_DOT="${ORG_DOT:-$HOME/org/org.dot}"
FIND_FILES="find -L $HOME/org \( \( -name .git -o -name template -o -name data \) -type d -prune -o -name '*.org' \) -type f"

# Known location for the .tix output so we can find it reliably.
TIX_FILE="$COVERAGE_BUILDDIR/org.tix"
export HPCTIXFILE="$TIX_FILE"

if [ ! -f "$ORG_YAML" ]; then
    echo "SKIP: $ORG_YAML not found (coverage check requires ~/org)"
    exit 0
fi

echo "Building with HPC coverage instrumentation..."
cabal build all --enable-coverage --builddir="$COVERAGE_BUILDDIR" 2>&1 | tail -1

echo "Running unit test suites with coverage..."
# Test suites emit their own .tix files which we merge with the
# round-trip .tix below (via hpc sum --union). Failures here are
# non-fatal because the round-trip data is still useful on its own.
cabal test org-data-test org-lint-test org-types-test org-print-test \
    --enable-coverage \
    --builddir="$COVERAGE_BUILDDIR" \
    --test-show-details=failures 2>&1 | tail -3 || true

# Locate the built executable directly.  cabal run wraps the process
# in a way that can interfere with HPC's atexit .tix dump.
ORG_EXE=$(cabal list-bin org-jw:exe:org --builddir="$COVERAGE_BUILDDIR" 2>/dev/null)
if [ ! -x "$ORG_EXE" ]; then
    echo "WARN: Could not locate coverage-instrumented executable."
    exit 0
fi

# Remove stale .tix before running so we get a clean measurement.
rm -f "$TIX_FILE"

CHECK=false
if [[ $(hostname) =~ [Hh]era ]]; then
    CHECK=true
fi

echo "Running round-trip with coverage..."
# The executable has -with-rtsopts=-N (threaded, all capabilities) but
# GHC's HPC runtime fails to write .tix with multiple capabilities.
# Override with -N1 so the atexit handler runs on the main OS thread.
eval "$FIND_FILES" \
    | "$ORG_EXE" \
	--config <(cat "$ORG_YAML" |					\
		   sed -e "s/checkFiles: true/checkFiles: $CHECK/")	\
        --keywords "$ORG_DOT" \
        lint \
        --round-trip \
        -F - \
        +RTS -N1 2>/dev/null

# Locate the .tix file: prefer the explicit HPCTIXFILE path, fall back
# to CWD (where GHC writes it when HPCTIXFILE is unset).
if [ ! -f "$TIX_FILE" ]; then
    # Fallback: GHC default writes <progname>.tix in the CWD.
    for candidate in org.tix *.tix; do
        if [ -f "$candidate" ]; then
            TIX_FILE="$candidate"
            break
        fi
    done
fi

if [ ! -f "$TIX_FILE" ]; then
    echo "WARN: No .tix file found. Coverage data unavailable."
    exit 0
fi

echo "Using .tix file: $TIX_FILE"

# Collect HPC mix directories.  Cabal with --enable-coverage places .mix
# files under:
#   <builddir>/build/<arch>/<ghc-ver>/<pkg>/build/extra-compilation-artifacts/
#     hpc/vanilla/mix/<unit-id>/<Module>.mix
# hpc expects --hpcdir to point at the "mix/" parent so it can resolve
# <unit-id>/<Module> relative paths from the .tix file.
HPCDIRS=""
while IFS= read -r mixdir; do
    HPCDIRS="$HPCDIRS --hpcdir=$mixdir"
done < <(find "$COVERAGE_BUILDDIR" -path "*/hpc/vanilla/mix/*" -name "*.mix" -type f 2>/dev/null \
    | while IFS= read -r f; do dirname "$(dirname "$f")"; done \
    | sort -u)

if [ -z "$HPCDIRS" ]; then
    echo "WARN: No .mix directories found under $COVERAGE_BUILDDIR."
    exit 0
fi

# Packages to exclude from coverage measurement:
#  - org-db, org-dot: database/IO code not exercised by round-trip
#  - org-site, org-json, org-cbor, org-filetags: feature packages not
#    exercised by the lint --round-trip command
#  - org-jw executable modules (Main, Options, etc.): CLI glue
#
# hpc --exclude requires exact module names (prefix matching does NOT
# work), so every submodule must be listed explicitly.
EXCLUDES=""
# org-db modules
EXCLUDES="$EXCLUDES --exclude=Org.DB.Connection"
EXCLUDES="$EXCLUDES --exclude=Org.DB.Deserialize"
EXCLUDES="$EXCLUDES --exclude=Org.DB.Embed"
EXCLUDES="$EXCLUDES --exclude=Org.DB.Migrate"
EXCLUDES="$EXCLUDES --exclude=Org.DB.Query"
EXCLUDES="$EXCLUDES --exclude=Org.DB.Schema"
EXCLUDES="$EXCLUDES --exclude=Org.DB.Store"
EXCLUDES="$EXCLUDES --exclude=Org.DB.Sync"
EXCLUDES="$EXCLUDES --exclude=Org.DB.Types"
# single-module feature packages
EXCLUDES="$EXCLUDES --exclude=Org.Dot"
EXCLUDES="$EXCLUDES --exclude=Org.Site"
EXCLUDES="$EXCLUDES --exclude=Org.JSON"
EXCLUDES="$EXCLUDES --exclude=Org.CBOR"
# org-filetags modules
EXCLUDES="$EXCLUDES --exclude=Org.FileTags.Filter"
EXCLUDES="$EXCLUDES --exclude=Org.FileTags.TagTrees"
# org-jw executable modules (no Org. prefix)
EXCLUDES="$EXCLUDES --exclude=Main"
EXCLUDES="$EXCLUDES --exclude=Options"
EXCLUDES="$EXCLUDES --exclude=Read"
EXCLUDES="$EXCLUDES --exclude=DB.Exec"
EXCLUDES="$EXCLUDES --exclude=DB.Options"
EXCLUDES="$EXCLUDES --exclude=Lint.Exec"
EXCLUDES="$EXCLUDES --exclude=Lint.Options"
EXCLUDES="$EXCLUDES --exclude=Trip.Exec"
EXCLUDES="$EXCLUDES --exclude=Trip.Options"
EXCLUDES="$EXCLUDES --exclude=JSON.Exec"
EXCLUDES="$EXCLUDES --exclude=JSON.Options"
EXCLUDES="$EXCLUDES --exclude=Stats.Exec"
EXCLUDES="$EXCLUDES --exclude=Stats.Options"
EXCLUDES="$EXCLUDES --exclude=Site.Exec"
EXCLUDES="$EXCLUDES --exclude=Site.Options"
EXCLUDES="$EXCLUDES --exclude=FileTags.Exec"
EXCLUDES="$EXCLUDES --exclude=FileTags.Options"
# Auto-generated Paths_ modules
EXCLUDES="$EXCLUDES --exclude=Paths_org_db"
EXCLUDES="$EXCLUDES --exclude=Paths_org_dot"
EXCLUDES="$EXCLUDES --exclude=Paths_org_site"
EXCLUDES="$EXCLUDES --exclude=Paths_org_json"
EXCLUDES="$EXCLUDES --exclude=Paths_org_cbor"
EXCLUDES="$EXCLUDES --exclude=Paths_org_filetags"
EXCLUDES="$EXCLUDES --exclude=Paths_org_jw"
EXCLUDES="$EXCLUDES --exclude=Paths_org_types"
EXCLUDES="$EXCLUDES --exclude=Paths_org_parse"
EXCLUDES="$EXCLUDES --exclude=Paths_org_print"
EXCLUDES="$EXCLUDES --exclude=Paths_org_data"
EXCLUDES="$EXCLUDES --exclude=Paths_org_lint"
EXCLUDES="$EXCLUDES --exclude=Paths_flatparse_util"

# Merge any additional .tix files produced by the test suites into the
# round-trip .tix so the report reflects all covered code paths.
MERGED_TIX="$COVERAGE_BUILDDIR/merged.tix"
EXTRA_TIX=$(find "$COVERAGE_BUILDDIR" -path "*/hpc/vanilla/tix/*" -name "*.tix" -type f 2>/dev/null | grep -v "/org.tix$" || true)
if [ -n "$EXTRA_TIX" ]; then
    # shellcheck disable=SC2086
    if hpc sum --union --output="$MERGED_TIX" "$TIX_FILE" $EXTRA_TIX 2>/dev/null; then
        TIX_FILE="$MERGED_TIX"
        echo "Merged test-suite .tix files into $MERGED_TIX"
    fi
fi

# Generate report and extract expression coverage percentage
# shellcheck disable=SC2086
REPORT=$(hpc report "$TIX_FILE" $HPCDIRS $EXCLUDES 2>&1 || true)
CURRENT=$(echo "$REPORT" | grep "expressions used" | grep -oE '[0-9]+' | head -1)

if [ -z "$CURRENT" ]; then
    echo "WARN: Could not extract coverage percentage."
    echo "$REPORT"
    exit 0
fi

echo "Current expression coverage: ${CURRENT}%"

# Generate HTML report. Source paths in .mix files are relative to each
# package's root, so list all covered package directories via --srcdir.
SRCDIRS=""
for pkg in flatparse-util org-types org-parse org-print org-data org-lint; do
    SRCDIRS="$SRCDIRS --srcdir=$pkg"
done
# shellcheck disable=SC2086
hpc markup "$TIX_FILE" $HPCDIRS $EXCLUDES $SRCDIRS --destdir=coverage-report 2>/dev/null || true
echo "HTML report: coverage-report/hpc_index.html"

# Compare against baseline
if [ -f "$BASELINE_FILE" ]; then
    BASELINE=$(cat "$BASELINE_FILE")
    if [ "$CURRENT" -lt "$BASELINE" ]; then
        echo "FAIL: Coverage dropped from ${BASELINE}% to ${CURRENT}%"
        exit 1
    else
        echo "PASS: Coverage ${CURRENT}% >= baseline ${BASELINE}%"
    fi
else
    echo "No baseline found. Saving current coverage as baseline."
    echo "$CURRENT" > "$BASELINE_FILE"
fi
