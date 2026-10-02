#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
PHASE27_TMP=$(mktemp -d)
trap 'rm -rf "$PHASE27_TMP"' EXIT HUP INT TERM
ulimit -c 0 || true

for level in 0 2; do
  "$OCAML_FAS" --emit-llvm -O"$level" "$ROOT/test/phase27_c_import.fas" \
    >"$PHASE27_TMP/program-O$level.ll"
  "$LLVM_OPT" -passes=verify "$PHASE27_TMP/program-O$level.ll" -disable-output
  "$LLVM_OPT" -S "-passes=default<O$level>" "$PHASE27_TMP/program-O$level.ll" \
    -o "$PHASE27_TMP/program-O$level.opt.ll"
  "$LLVM_OPT" -passes=verify "$PHASE27_TMP/program-O$level.opt.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$ROOT/test/phase27_c_import.fas" \
    "$ROOT/test/phase27_c_import_runtime.c" -o "$PHASE27_TMP/fas-O$level"
  "$CC" -std=gnu17 -Werror -O"$level" \
    "$ROOT/test/phase27_c_import_oracle.c" \
    "$ROOT/test/phase27_c_import_runtime.c" -o "$PHASE27_TMP/oracle-O$level"
  timeout 30 "$PHASE27_TMP/fas-O$level"
  timeout 30 "$PHASE27_TMP/oracle-O$level"
done

echo "phase27_c_import: O0/O2 C oracle and LLVM verification passed"
