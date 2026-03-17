#!/usr/bin/env bash
set -euo pipefail

# Check that performance has not regressed by more than 5%.
#
# Measures wall-clock time for a full round-trip pass and compares
# against a stored baseline. The baseline is local and gitignored
# since it depends on the machine and the set of org files.

BASELINE_FILE=".perf-baseline"
THRESHOLD=5
ORG_YAML="${ORG_YAML:-$HOME/org/org.yaml}"
ORG_DOT="${ORG_DOT:-$HOME/org/org.dot}"
FIND_FILES="find -L $HOME/org \( \( -name .git -o -name template -o -name data \) -type d -prune -o -name '*.org' \) -type f"

if [ ! -f "$ORG_YAML" ]; then
    echo "SKIP: $ORG_YAML not found (perf check requires ~/org)"
    exit 0
fi

echo "Building..."
cabal build all 2>&1 | tail -1

echo "Measuring round-trip performance..."

# Use perl for sub-second timing (macOS date lacks %N)
elapsed_ms() {
    perl -MTime::HiRes=time -e 'print int(time * 1000)'
}

START=$(elapsed_ms)

eval "$FIND_FILES" \
    | cabal run org-jw:exe:org -- \
        --config "$ORG_YAML" \
        --keywords "$ORG_DOT" \
        lint \
        --round-trip \
        -F - \
        +RTS -N 2>/dev/null

END=$(elapsed_ms)
ELAPSED_MS=$(( END - START ))

echo "Elapsed: ${ELAPSED_MS}ms"

# Compare against baseline
if [ -f "$BASELINE_FILE" ]; then
    BASELINE=$(cat "$BASELINE_FILE")
    MAX_ALLOWED=$(( BASELINE * (100 + THRESHOLD) / 100 ))

    if [ "$ELAPSED_MS" -gt "$MAX_ALLOWED" ]; then
        REGRESSION=$(( (ELAPSED_MS - BASELINE) * 100 / BASELINE ))
        echo "FAIL: Performance regressed by ${REGRESSION}% (${ELAPSED_MS}ms vs baseline ${BASELINE}ms, threshold ${THRESHOLD}%)"
        exit 1
    else
        echo "PASS: ${ELAPSED_MS}ms within ${THRESHOLD}% of baseline ${BASELINE}ms"
    fi
else
    echo "No baseline found. Saving current time as baseline."
fi

# Update baseline (use the better of current and previous)
if [ -f "$BASELINE_FILE" ]; then
    PREV=$(cat "$BASELINE_FILE")
    if [ "$ELAPSED_MS" -lt "$PREV" ]; then
        echo "$ELAPSED_MS" > "$BASELINE_FILE"
        echo "Updated baseline: ${ELAPSED_MS}ms (improved from ${PREV}ms)"
    fi
else
    echo "$ELAPSED_MS" > "$BASELINE_FILE"
fi
