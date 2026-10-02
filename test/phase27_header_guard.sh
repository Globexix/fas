#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
PHASE27_TMP=$(mktemp -d)
trap 'rm -rf "$PHASE27_TMP"' EXIT HUP INT TERM
ulimit -c 0 || true

mkdir "$PHASE27_TMP/include"
"$OCAML_FAS" --emit-header -o "$PHASE27_TMP/include/add.h" \
  "$ROOT/test/phase27_header_guard.fas"
if grep -Fx '#ifndef FAS_ADD_H' "$PHASE27_TMP/include/add.h"; then
  echo 'phase27_header_guard: include guard shadows exported identifier' >&2
  exit 1
fi
"$CC" -std=c11 -Wall -Wextra -Werror -pedantic \
  "$ROOT/test/phase27_header_guard_oracle.c" -o "$PHASE27_TMP/oracle"
"$PHASE27_TMP/oracle" >"$PHASE27_TMP/expected"

for level in 0 2; do
  "$OCAML_FAS" --emit-llvm -O"$level" "$ROOT/test/phase27_header_guard.fas" \
    >"$PHASE27_TMP/program-O$level.ll"
  "$LLVM_OPT" -passes=verify "$PHASE27_TMP/program-O$level.ll" -disable-output
  "$LLVM_OPT" -S "-passes=default<O$level>" "$PHASE27_TMP/program-O$level.ll" \
    -o "$PHASE27_TMP/program-O$level.opt.ll"
  "$LLVM_OPT" -passes=verify "$PHASE27_TMP/program-O$level.opt.ll" -disable-output
  "$OCAML_FAS" -O"$level" -c "$ROOT/test/phase27_header_guard.fas" \
    -o "$PHASE27_TMP/program-O$level.o"
  "$CC" -std=c11 -Wall -Wextra -Werror -pedantic -O"$level" \
    -I"$PHASE27_TMP/include" "$ROOT/test/phase27_header_guard_consumer.c" \
    "$PHASE27_TMP/program-O$level.o" -o "$PHASE27_TMP/consumer-O$level"
  "$PHASE27_TMP/consumer-O$level" >"$PHASE27_TMP/actual"
  diff -u "$PHASE27_TMP/expected" "$PHASE27_TMP/actual"
done

echo 'phase27_header_guard: add.h export collision, O0/O2 C oracle and LLVM verification passed'
