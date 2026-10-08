#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/widening.fas" >"$TMP/input.ll"
"$LLVM_OPT" -passes=verify "$TMP/input.ll" -disable-output

cat >"$TMP/implicit.fas" <<'EOF'
fn widen_unsigned(value u8) i16 { return value }
fn widen_signed(value i8) i64 { return value }
fn mixed_value(left u32, right i64) i64 { return left + right }
fn call_value(value u8) i64 { return consume(value) }
fn consume(value i64) i64 { return value }
EOF
cat >"$TMP/explicit.fas" <<'EOF'
fn widen_unsigned(value u8) i16 { return zext[i16](value) }
fn widen_signed(value i8) i64 { return sext[i64](value) }
fn mixed_value(left u32, right i64) i64 { return zext[i64](left) + right }
fn call_value(value u8) i64 { return consume(zext[i64](value)) }
fn consume(value i64) i64 { return value }
EOF
"$OCAML_FAS" --emit-llvm "$TMP/implicit.fas" >"$TMP/implicit.ll"
"$OCAML_FAS" --emit-llvm "$TMP/explicit.fas" >"$TMP/explicit.ll"
cmp -s "$TMP/implicit.ll" "$TMP/explicit.ll" || {
  echo "widening: implicit and explicit LLVM differ" >&2
  exit 1
}

for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/input.ll" \
    -o "$TMP/input.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/input.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$TMP/fas.O$level" \
    "$ROOT/test/widening.fas" "$ROOT/test/widening_oracle.c"
  "$CC" -Werror -std=c17 -O"$level" -DWIDENING_ORACLE \
    "$ROOT/test/widening_oracle.c" -o "$TMP/oracle.O$level"
  timeout 30 "$TMP/fas.O$level" >"$TMP/fas.O$level.out"
  timeout 30 "$TMP/oracle.O$level" >"$TMP/oracle.O$level.out"
  cmp -s "$TMP/fas.O$level.out" "$TMP/oracle.O$level.out" || {
    echo "widening: Fas and C results differ at O$level" >&2
    exit 1
  }
done

echo "widening: sign fill, zero fill, mixed comparisons, C calls and LLVM equivalence at O0/O2: ok"
