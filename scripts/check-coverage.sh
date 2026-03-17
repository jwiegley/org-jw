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

if [ ! -f "$ORG_YAML" ]; then
    echo "SKIP: $ORG_YAML not found (coverage check requires ~/org)"
    exit 0
fi

echo "Building with HPC coverage instrumentation..."
cabal build all --enable-coverage --builddir="$COVERAGE_BUILDDIR" 2>&1 | tail -1

echo "Running round-trip with coverage..."
eval "$FIND_FILES" \
    | cabal run org-jw:exe:org --builddir="$COVERAGE_BUILDDIR" -- \
        --config "$ORG_YAML" \
        --keywords "$ORG_DOT" \
        lint \
        --round-trip \
        -F - \
        +RTS -N 2>/dev/null

# Find and merge .tix files
TIX_FILES=$(find "$COVERAGE_BUILDDIR" -name "*.tix" -type f 2>/dev/null || true)
if [ -z "$TIX_FILES" ]; then
    echo "WARN: No .tix files found. Coverage data unavailable."
    exit 0
fi

# Collect HPC mix directories
HPCDIRS=""
for mixdir in $(find "$COVERAGE_BUILDDIR" -path "*/hpc/vanilla/mix/*" -type d 2>/dev/null); do
    HPCDIRS="$HPCDIRS --hpcdir=$mixdir"
done

# Merge .tix files if more than one
TIX_COUNT=$(echo "$TIX_FILES" | wc -l | tr -d ' ')
if [ "$TIX_COUNT" -gt 1 ]; then
    # shellcheck disable=SC2086
    hpc sum --union $TIX_FILES --output=combined.tix
    TIX_FILE="combined.tix"
else
    TIX_FILE=$(echo "$TIX_FILES" | head -1)
fi

# Generate report and extract expression coverage percentage
# shellcheck disable=SC2086
REPORT=$(hpc report "$TIX_FILE" $HPCDIRS 2>&1 || true)
CURRENT=$(echo "$REPORT" | grep "expressions used" | grep -oE '[0-9]+' | head -1)

if [ -z "$CURRENT" ]; then
    echo "WARN: Could not extract coverage percentage."
    echo "$REPORT"
    exit 0
fi

echo "Current expression coverage: ${CURRENT}%"

# Generate HTML report
# shellcheck disable=SC2086
hpc markup "$TIX_FILE" $HPCDIRS --destdir=coverage-report 2>/dev/null || true
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
