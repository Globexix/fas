#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
PHASE27_TMP=$(mktemp -d)
trap 'rm -rf "$PHASE27_TMP"' EXIT HUP INT TERM
ulimit -c 0 || true

"$CC" -std=c11 -Wall -Wextra -Werror -pedantic \
  "$ROOT/test/phase27_main_oracle.c" -o "$PHASE27_TMP/oracle"
"$PHASE27_TMP/oracle"

for level in 0 2; do
  for fixture in phase27_main_empty phase27_main_args; do
    source="$ROOT/test/$fixture.fas"
    "$OCAML_FAS" --emit-llvm -O"$level" "$source" >"$PHASE27_TMP/$fixture-O$level.ll"
    "$LLVM_OPT" -passes=verify "$PHASE27_TMP/$fixture-O$level.ll" -disable-output
    "$LLVM_OPT" -S "-passes=default<O$level>" "$PHASE27_TMP/$fixture-O$level.ll" \
      -o "$PHASE27_TMP/$fixture-O$level.opt.ll"
    "$LLVM_OPT" -passes=verify "$PHASE27_TMP/$fixture-O$level.opt.ll" -disable-output
    "$OCAML_FAS" -O"$level" "$source" -o "$PHASE27_TMP/$fixture-O$level"
    "$PHASE27_TMP/$fixture-O$level"
  done
done

"$OCAML_FAS" -c "$ROOT/test/phase27_main_no_entry.fas" \
  -o "$PHASE27_TMP/no-entry.o"
[ -s "$PHASE27_TMP/no-entry.o" ]

echo 'phase27_main: both documented entry signatures at O0/O2; no-main object: ok'
