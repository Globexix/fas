#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BIN=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT HUP INT TERM
ulimit -v 4194304

count=0
check_source() {
  source=$1
  relative=${source#"$ROOT"/}
  case "$relative" in
    test/mistakes/*)
      if grep -Fq "$relative" "$ROOT/test/mistakes/COMPILES.md"; then
        return
      fi
      ;;
  esac
  set +e
  "$BIN" --emit-llvm "$source" >"$WORK/first.out" 2>"$WORK/first.err"
  first_status=$?
  "$BIN" --emit-llvm "$source" >"$WORK/second.out" 2>"$WORK/second.err"
  second_status=$?
  set -e
  [ "$first_status" -ne 0 ] || {
    echo "diagnostic determinism: accepted rejection fixture $source" >&2
    exit 1
  }
  [ "$first_status" -eq "$second_status" ] || {
    echo "diagnostic determinism: status changed for $source" >&2
    exit 1
  }
  cmp -s "$WORK/first.err" "$WORK/second.err" || {
    echo "diagnostic determinism: stderr changed for $source" >&2
    diff -u "$WORK/first.err" "$WORK/second.err" >&2 || true
    exit 1
  }
  count=$((count + 1))
}

for source in "$ROOT"/test/diagnostics/*.fas "$ROOT"/test/mistakes/*/*.fas; do
  [ -f "$source" ] || continue
  check_source "$source"
done

echo "diagnostic determinism: $count rejected inputs matched"
