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
  "$ROOT/test/phase27_integer_separators_oracle.c" -o "$PHASE27_TMP/oracle"
"$PHASE27_TMP/oracle"

for level in 0 2; do
  "$OCAML_FAS" --emit-llvm -O"$level" "$ROOT/test/phase27_integer_separators.fas" \
    >"$PHASE27_TMP/program-O$level.ll"
  "$LLVM_OPT" -passes=verify "$PHASE27_TMP/program-O$level.ll" -disable-output
  "$LLVM_OPT" -S "-passes=default<O$level>" "$PHASE27_TMP/program-O$level.ll" \
    -o "$PHASE27_TMP/program-O$level.opt.ll"
  "$LLVM_OPT" -passes=verify "$PHASE27_TMP/program-O$level.opt.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$ROOT/test/phase27_integer_separators.fas" \
    -o "$PHASE27_TMP/program-O$level"
  "$PHASE27_TMP/program-O$level"
done

echo 'phase27_integer_separators: exact errors and decimal/hex/binary values O0/O2: ok'
