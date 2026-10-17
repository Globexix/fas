#!/bin/sh
set -eu
export DEBUGINFOD_URLS=

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
GRAMMAR_DIR="$ROOT/editors/tree-sitter-fas"
TREE_SITTER=${TREE_SITTER:-tree-sitter}
GATE_TMP=$(mktemp -d)
trap 'rm -rf "$GATE_TMP"' EXIT HUP INT TERM

case "$TREE_SITTER" in
  /*) ;;
  */*) TREE_SITTER=$(pwd)/$TREE_SITTER ;;
  *) TREE_SITTER=$(command -v "$TREE_SITTER" 2>/dev/null || true) ;;
esac
if [ -z "$TREE_SITTER" ] || [ ! -x "$TREE_SITTER" ]; then
  echo "validation: tree-sitter CLI is missing; set TREE_SITTER" >&2
  exit 2
fi

cd "$ROOT"
git ls-files -- '*.fas' | grep -vE '^test/(mistakes|unsafe/invalid)/' | sed "s|^|$ROOT/|" > "$GATE_TMP/fixtures" || true
if [ ! -s "$GATE_TMP/fixtures" ]; then
  echo "tree-sitter corpus: no tracked Fas fixtures found" >&2
  exit 1
fi

cd "$GRAMMAR_DIR"
"$TREE_SITTER" generate
"$TREE_SITTER" test

sample=$(head -n 1 "$GATE_TMP/fixtures")
for query in highlights brackets indents outline; do
  "$TREE_SITTER" query "queries/$query.scm" "$sample" > /dev/null
done

xargs "$TREE_SITTER" parse --quiet < "$GATE_TMP/fixtures"
echo "tree-sitter corpus: $(wc -l < "$GATE_TMP/fixtures") tracked fixtures parse without errors"
