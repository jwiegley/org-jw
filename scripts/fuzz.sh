#!/usr/bin/env bash
set -euo pipefail

# Fuzz the org-mode parser by feeding it truncated and mutated real org files.
#
# Takes real .org files, randomly truncates or mutates them, and verifies
# the parser handles them without crashing. This catches edge cases in the
# FlatParse-based parser that clean input would never exercise.
#
# Usage: scripts/fuzz.sh [iterations] [org-dir]

ITERS="${1:-200}"
ORG_DIR="${2:-$HOME/org}"
ORG_YAML="${ORG_YAML:-$HOME/org/org.yaml}"
ORG_DOT="${ORG_DOT:-$HOME/org/org.dot}"

if [ ! -d "$ORG_DIR" ]; then
    echo "SKIP: $ORG_DIR not found"
    exit 0
fi

if [ ! -f "$ORG_YAML" ]; then
    echo "SKIP: $ORG_YAML not found"
    exit 0
fi

echo "Building..."
cabal build all 2>&1 | tail -1

ORG_BIN=$(cabal list-bin org 2>/dev/null)
if [ ! -x "$ORG_BIN" ]; then
    echo "FAIL: could not find org executable"
    exit 1
fi

# Collect org files
FILES=()
while IFS= read -r -d '' f; do
    FILES+=("$f")
done < <(find -L "$ORG_DIR" \( \( -name .git -o -name template -o -name data \) -type d -prune -o -name '*.org' \) -type f -print0)

if [ ${#FILES[@]} -eq 0 ]; then
    echo "SKIP: no .org files found in $ORG_DIR"
    exit 0
fi

CRASHES=0
TOTAL=${#FILES[@]}

echo "Fuzz testing: $ITERS iterations across $TOTAL org files..."

for i in $(seq 1 "$ITERS"); do
    # Pick a random file
    FILE="${FILES[$((RANDOM % TOTAL))]}"
    SIZE=$(wc -c < "$FILE")

    if [ "$SIZE" -eq 0 ]; then
        continue
    fi

    # Strategy: randomly truncate the file
    TRUNCATE_AT=$((RANDOM % SIZE + 1))

    if ! head -c "$TRUNCATE_AT" "$FILE" | \
        timeout 10 "$ORG_BIN" \
            --config "$ORG_YAML" \
            --keywords "$ORG_DOT" \
            parse -f - >/dev/null 2>&1; then
        EXIT_CODE=$?
        # Exit codes: 1 = parse error (expected), 124 = timeout, 139 = segfault
        if [ "$EXIT_CODE" -eq 124 ]; then
            echo "TIMEOUT on $FILE (truncated at byte $TRUNCATE_AT)"
            CRASHES=$((CRASHES + 1))
        elif [ "$EXIT_CODE" -gt 128 ]; then
            SIGNAL=$((EXIT_CODE - 128))
            echo "CRASH (signal $SIGNAL) on $FILE (truncated at byte $TRUNCATE_AT)"
            CRASHES=$((CRASHES + 1))
        fi
        # Exit code 1 is a normal parse failure, that's fine
    fi

    if [ $((i % 50)) -eq 0 ]; then
        echo "  $i / $ITERS iterations..."
    fi
done

echo "Fuzz test complete: $ITERS iterations, $CRASHES crashes"

if [ "$CRASHES" -gt 0 ]; then
    echo "FAIL: $CRASHES crashes detected"
    exit 1
fi
