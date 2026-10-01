#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
LLVM_OPT=${LLVM_OPT:-opt-22}
PHASE25_TMP=$(mktemp -d)
trap 'rm -rf "$PHASE25_TMP"' EXIT HUP INT TERM
"$OCAML_FAS" --emit-llvm "$ROOT/test/phase25_types.fas" > "$PHASE25_TMP/program.ll"
"$LLVM_OPT" -passes=verify "$PHASE25_TMP/program.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$PHASE25_TMP/program.ll" -o "$PHASE25_TMP/optimized.ll"
  "$LLVM_OPT" -passes=verify "$PHASE25_TMP/optimized.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$ROOT/test/phase25_types.fas" -o "$PHASE25_TMP/program"
  "$PHASE25_TMP/program"
done
echo 'phase25_types: generic type slots and nested indexing against C oracle at O0/O2: ok'
