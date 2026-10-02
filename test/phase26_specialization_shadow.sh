#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
SOURCE="$ROOT/test/phase26_specialization_shadow.fas"
ORACLE="$ROOT/test/phase26_specialization_shadow_oracle.c"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
fail() { echo "phase26_specialization_shadow: $*" >&2; exit 1; }

for level in 0 2; do
  "$CC" -std=c11 -Wall -Wextra -Werror -O"$level" "$ORACLE" -o "$TMP/oracle-$level"
  "$TMP/oracle-$level" || fail "C oracle failed at O$level"
  "$OCAML_FAS" --emit-llvm "$SOURCE" >"$TMP/program.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" -o "$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$SOURCE" -o "$TMP/fas-$level"
  "$TMP/fas-$level" || fail "Fas shadow cases failed at O$level"
done

echo "phase26_specialization_shadow: nested, for, switch, view and index shadows at O0/O2: ok"
