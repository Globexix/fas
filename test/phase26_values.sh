#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
fail() { echo "phase26_values: $*" >&2; exit 1; }

CC="$CC" "$OCAML_FAS" --emit-llvm "$ROOT/test/phase26_values.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" -o "$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  CC="$CC" "$OCAML_FAS" -O"$level" "$ROOT/test/phase26_values.fas" -o "$TMP/program.O$level"
  "$TMP/program.O$level" || fail "constant/runtime value builtin sweep failed at O$level"
done
echo "phase26_values: SIMD value builtins at 1 and 256 lanes, const and runtime, O0/O2: ok"
