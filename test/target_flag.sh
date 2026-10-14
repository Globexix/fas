#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
FIXTURES=$ROOT/test/target_flag
TARGET_TMP=$(mktemp -d)
trap 'rm -rf "$TARGET_TMP"' EXIT HUP INT TERM

fail() {
  echo "target_flag: $*" >&2
  exit 1
}

"$OCAML_FAS" --emit-llvm "$FIXTURES/program.fas" >"$TARGET_TMP/default.ll"
"$OCAML_FAS" --target=portable --emit-llvm "$FIXTURES/program.fas" >"$TARGET_TMP/portable.ll"
cmp -s "$TARGET_TMP/default.ll" "$TARGET_TMP/portable.ll" \
  || fail "portable output differs from the default target"
if grep -q 'target-cpu' "$TARGET_TMP/portable.ll"; then
  fail "portable output carries a host CPU attribute"
fi

"$OCAML_FAS" --target=force-native --emit-llvm "$FIXTURES/program.fas" >"$TARGET_TMP/native.ll"
grep -q '^define' "$TARGET_TMP/native.ll" || fail "force-native IR has no definitions"
if grep '^define' "$TARGET_TMP/native.ll" | grep -qv ' #1 {$'; then
  fail "a force-native definition lacks the host CPU attributes"
fi
if grep '^declare' "$TARGET_TMP/native.ll" | grep -q '#1'; then
  fail "a force-native declaration carries host CPU attributes"
fi
grep -q '^attributes #1 = { "target-cpu"="' "$TARGET_TMP/native.ll" \
  || fail "force-native IR has no target-cpu attribute group"
grep -q '"target-features"="' "$TARGET_TMP/native.ll" \
  || fail "force-native IR has no target-features attribute group"
"$LLVM_OPT" -passes=verify "$TARGET_TMP/native.ll" -disable-output
"$LLVM_OPT" -S "-passes=default<O2>" "$TARGET_TMP/native.ll" -o "$TARGET_TMP/native.O2.ll"
"$LLVM_OPT" -passes=verify "$TARGET_TMP/native.O2.ll" -disable-output

"$CC" -Werror -std=c17 -O2 "$FIXTURES/oracle.c" -o "$TARGET_TMP/oracle"
"$TARGET_TMP/oracle" main >"$TARGET_TMP/oracle.main.out"
for target in portable force-native; do
  for level in 0 2; do
    exe="$TARGET_TMP/program.$target.O$level"
    "$OCAML_FAS" --target="$target" "-O$level" -o "$exe" "$FIXTURES/program.fas" \
      2>"$TARGET_TMP/program.$target.O$level.err"
    [ ! -s "$TARGET_TMP/program.$target.O$level.err" ] \
      || fail "$target -O$level build wrote diagnostics"
    timeout 30 "$exe" >"$exe.out" || fail "$target -O$level program failed"
    cmp -s "$exe.out" "$TARGET_TMP/oracle.main.out" \
      || fail "$target -O$level program output differs from the C oracle"
  done
done

"$TARGET_TMP/oracle" intrinsics >"$TARGET_TMP/oracle.intrinsics.out"
if grep -qw bmi2 /proc/cpuinfo; then
  for level in 0 2; do
    exe="$TARGET_TMP/intrinsics.O$level"
    "$OCAML_FAS" --target=force-native "-O$level" -o "$exe" \
      "$FIXTURES/intrinsics.fas" "$FIXTURES/intrinsics.c"
    timeout 30 "$exe" >"$exe.out" || fail "force-native intrinsics -O$level program failed"
    cmp -s "$exe.out" "$TARGET_TMP/oracle.intrinsics.out" \
      || fail "force-native intrinsics -O$level output differs from the C oracle"
  done
else
  echo "target_flag: bmi2 execution skipped (host has no bmi2)" >&2
fi
if "$OCAML_FAS" -O2 -o "$TARGET_TMP/intrinsics.portable" \
  "$FIXTURES/intrinsics.fas" "$FIXTURES/intrinsics.c" 2>"$TARGET_TMP/portable-intrinsics.err"; then
  fail "portable build accepted a bmi2 intrinsic"
fi
grep -q 'bmi2' "$TARGET_TMP/portable-intrinsics.err" \
  || fail "portable bmi2 rejection did not name the feature"

"$OCAML_FAS" --target=force-native -S -o "$TARGET_TMP/native.s" "$FIXTURES/intrinsics.fas"
"$OCAML_FAS" --target=portable -S -o "$TARGET_TMP/portable.s" "$FIXTURES/intrinsics.fas"
grep -q 'popcnt' "$TARGET_TMP/native.s" || fail "force-native assembly has no popcnt"
if grep -q 'popcnt' "$TARGET_TMP/portable.s"; then
  fail "portable assembly uses popcnt"
fi

if "$OCAML_FAS" --target=bogus --emit-llvm "$FIXTURES/program.fas" \
  >/dev/null 2>"$TARGET_TMP/bogus.err"; then
  fail "unknown target value was accepted"
fi
grep -qxF 'unknown --target value `bogus`; accepted values are `portable` and `force-native`' \
  "$TARGET_TMP/bogus.err" || fail "unknown target value message changed"

if FAS_CC=/bin/false "$OCAML_FAS" --target=force-native --emit-llvm "$FIXTURES/program.fas" \
  >/dev/null 2>"$TARGET_TMP/probe.err"; then
  fail "force-native accepted a failing host CPU probe"
fi
grep -qF -- '--target=force-native: cannot determine the host CPU (/bin/false failed (exit 1))' \
  "$TARGET_TMP/probe.err" || fail "host CPU probe failure message changed"

echo "target_flag: portable identity, force-native attributes, O0/O2 execution, intrinsics, asm, and diagnostics verified: ok"
