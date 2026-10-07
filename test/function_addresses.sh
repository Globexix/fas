#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=$(printenv CC 2>/dev/null || printf '%s' clang-22)
LLVM_OPT=$(printenv LLVM_OPT 2>/dev/null || printf '%s' opt-22)
OCAML_FAS=$(printenv OCAML_FAS 2>/dev/null || printf '%s' "$ROOT/_build/default/bin/main.exe")
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

fail() {
  echo "function_addresses: $*" >&2
  exit 1
}

"$CC" -Werror -std=c17 "$ROOT/test/function_addresses_oracle.c" -o "$TMP/oracle"
"$OCAML_FAS" --emit-llvm "$ROOT/test/function_addresses.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
grep -Eq 'call void @fas_invoke\(ptr @fas_callback\)' "$TMP/program.ll" || fail "native function address was not passed to C as a function pointer"

for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" -o "$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$ROOT/test/function_addresses.fas" -o "$TMP/fas.O$level"
  timeout 30 "$TMP/fas.O$level" >"$TMP/fas.O$level.out" || fail "Fas O$level execution failed"
  timeout 30 "$TMP/oracle" >"$TMP/oracle.out" || fail "C oracle failed"
  cmp -s "$TMP/fas.O$level.out" "$TMP/oracle.out" || fail "Fas and C oracle outputs differ at O$level"
done

echo "function_addresses: native function address passed to C at O0/O2: ok"
