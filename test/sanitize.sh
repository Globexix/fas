#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
REAL_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
SANITIZE_TMP=$(mktemp -d)
trap 'rm -rf "$SANITIZE_TMP"' EXIT HUP INT TERM
ulimit -Sv 4194304

fail() {
  printf 'sanitize: %s\n' "$*" >&2
  exit 1
}

verify_address_ir() {
  source=$1
  level=$2
  label=$3
  "$REAL_FAS" --sanitize=address --emit-llvm "$source" \
    >"$SANITIZE_TMP/$label.input.ll"
  "$LLVM_OPT" -passes=verify "$SANITIZE_TMP/$label.input.ll" -disable-output
  "$LLVM_OPT" -asan-use-after-scope -S "-passes=asan,default<O$level>" \
    "$SANITIZE_TMP/$label.input.ll" -o "$SANITIZE_TMP/$label.optimized.ll"
  "$LLVM_OPT" -passes=verify "$SANITIZE_TMP/$label.optimized.ll" -disable-output
}

run_asan_failure() {
  binary=$1
  expected=$2
  status=0
  (ulimit -Sv unlimited; ASAN_OPTIONS=detect_stack_use_after_return=1 "$binary") \
    >"$SANITIZE_TMP/run.out" 2>"$SANITIZE_TMP/run.err" || status=$?
  [ "$status" -ne 0 ] || fail "$expected program unexpectedly succeeded"
  grep -F "$expected" "$SANITIZE_TMP/run.err" >/dev/null \
    || fail "expected $expected; got: $(head -n 5 "$SANITIZE_TMP/run.err")"
}

for level in 0 2; do
  for case_name in oob scope return; do
    verify_address_ir "$ROOT/test/sanitize_$case_name.fas" "$level" "$case_name.O$level"
    "$REAL_FAS" --sanitize=address -O"$level" \
      "$ROOT/test/sanitize_$case_name.fas" -o "$SANITIZE_TMP/$case_name.O$level"
  done
  run_asan_failure "$SANITIZE_TMP/oob.O$level" stack-buffer-overflow
  run_asan_failure "$SANITIZE_TMP/scope.O$level" stack-use-after-scope
  run_asan_failure "$SANITIZE_TMP/return.O$level" stack-use-after-return
  verify_address_ir "$ROOT/test/sanitize_heap.fas" "$level" "heap.O$level"
  "$REAL_FAS" --sanitize=address -O"$level" "$ROOT/test/sanitize_heap.fas" \
    "$ROOT/test/sanitize_heap.c" -o "$SANITIZE_TMP/heap.O$level"
  run_asan_failure "$SANITIZE_TMP/heap.O$level" heap-buffer-overflow

  "$REAL_FAS" --sanitize=undefined --emit-llvm "$ROOT/test/sanitize_ubsan.fas" \
    >"$SANITIZE_TMP/ubsan.O$level.input.ll"
  "$LLVM_OPT" -passes=verify "$SANITIZE_TMP/ubsan.O$level.input.ll" -disable-output
  "$LLVM_OPT" -S "-passes=default<O$level>" "$SANITIZE_TMP/ubsan.O$level.input.ll" \
    -o "$SANITIZE_TMP/ubsan.O$level.optimized.ll"
  "$LLVM_OPT" -passes=verify "$SANITIZE_TMP/ubsan.O$level.optimized.ll" -disable-output
  "$REAL_FAS" --sanitize=undefined -O"$level" "$ROOT/test/sanitize_ubsan.fas" \
    -o "$SANITIZE_TMP/ubsan.O$level"
  (ulimit -Sv unlimited; "$SANITIZE_TMP/ubsan.O$level") \
    >"$SANITIZE_TMP/ubsan.O$level.out" 2>"$SANITIZE_TMP/ubsan.O$level.err"
  grep -F 'runtime error:' "$SANITIZE_TMP/ubsan.O$level.err" >/dev/null \
    || fail "C signed overflow did not report under UBSan at O$level"

  "$REAL_FAS" --sanitize=undefined --emit-llvm "$ROOT/test/sanitize_wrapping.fas" \
    >"$SANITIZE_TMP/wrapping.O$level.input.ll"
  "$LLVM_OPT" -passes=verify "$SANITIZE_TMP/wrapping.O$level.input.ll" -disable-output
  "$REAL_FAS" --sanitize=undefined -O"$level" "$ROOT/test/sanitize_wrapping.fas" \
    -o "$SANITIZE_TMP/wrapping.O$level"
  (ulimit -Sv unlimited; "$SANITIZE_TMP/wrapping.O$level") \
    >"$SANITIZE_TMP/wrapping.O$level.out" 2>"$SANITIZE_TMP/wrapping.O$level.err" \
    || fail "Fas wrapping arithmetic failed at O$level"
  [ ! -s "$SANITIZE_TMP/wrapping.O$level.out" ] \
    && [ ! -s "$SANITIZE_TMP/wrapping.O$level.err" ] \
    || fail "Fas wrapping arithmetic reported under UBSan at O$level"
done
printf 'sanitize: invalid cases and defined arithmetic passed at O0/O2\n'
