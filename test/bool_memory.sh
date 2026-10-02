#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
fail() { echo "bool_memory: $*" >&2; exit 1; }

"$OCAML_FAS" --emit-llvm "$ROOT/test/bool_memory.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" \
    -o "$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$TMP/program.O$level" \
    "$ROOT/test/bool_memory.fas" "$ROOT/test/bool_memory_oracle.c"
  "$TMP/program.O$level" >"$TMP/observed"
  printf 'expected=8 observed=8\n' >"$TMP/expected"
  cmp -s "$TMP/expected" "$TMP/observed" || fail "bool memory paths differed at O$level"
done
echo "bool_memory: local, global, field, element, raw, view and imported field reads at O0/O2: ok"
