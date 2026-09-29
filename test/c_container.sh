#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
CONTAINER_TMP=$(mktemp -d)
trap 'rm -rf "$CONTAINER_TMP"' EXIT HUP INT TERM

fail() {
  echo "c_container: $*" >&2
  exit 1
}

expect_failure() {
  status=0
  "$@" >"$CONTAINER_TMP/stdout" 2>"$CONTAINER_TMP/stderr" || status=$?
  [ "$status" -ne 0 ] || fail "command unexpectedly succeeded: $*"
  [ ! -s "$CONTAINER_TMP/stdout" ] || fail "failure wrote to stdout: $*"
}

cat >"$CONTAINER_TMP/program.fas" <<'FAS'
use "C" <<END
int container_value(void) { return 11; }
END
extern "C" {
  fn fas_container_value() i32 { return container_value() + 31 }
}
fn main() i32 { return fas_container_value() - 42 }
FAS

for level in 0 2; do
  "$OCAML_FAS" -O"$level" --emit-llvm "$CONTAINER_TMP/program.fas" \
    >"$CONTAINER_TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$CONTAINER_TMP/program.O$level.ll" -disable-output
  "$LLVM_OPT" "-passes=default<O$level>" -verify-each \
    "$CONTAINER_TMP/program.O$level.ll" -S -o "$CONTAINER_TMP/optimized.O$level.ll"
  "$LLVM_OPT" -passes=verify "$CONTAINER_TMP/optimized.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$CONTAINER_TMP/program.fas" \
    -o "$CONTAINER_TMP/program.O$level"
  "$CONTAINER_TMP/program.O$level" || fail "container executable failed at O$level"
done

cat >"$CONTAINER_TMP/object.fas" <<'FAS'
use "C" <<END
int container_value(void) { return 11; }
END
extern "C" {
  fn fas_container_value() i32 { return container_value() + 31 }
}
FAS
cat >"$CONTAINER_TMP/oracle.c" <<'C'
extern int fas_container_value(void);
int main(void) { return fas_container_value() == 42 ? 0 : 1; }
C
for level in 0 2; do
  "$OCAML_FAS" -c -O"$level" "$CONTAINER_TMP/object.fas" \
    -o "$CONTAINER_TMP/fas.O$level.o"
  "$CC" "$CONTAINER_TMP/oracle.c" "$CONTAINER_TMP/fas.O$level.o" \
    -o "$CONTAINER_TMP/object.O$level"
  "$CONTAINER_TMP/object.O$level" || fail "merged object failed at O$level"
done

cat >"$CONTAINER_TMP/header-only.fas" <<'FAS'
use "C" <stdint.h>
fn main() i32 { return 0 }
FAS
"$OCAML_FAS" --keep -debug "$CONTAINER_TMP/header-only.fas" \
  -o "$CONTAINER_TMP/header-only" >"$CONTAINER_TMP/stdout" \
  2>"$CONTAINER_TMP/stderr"
"$CONTAINER_TMP/header-only" || fail "header-only program failed"
if grep -F "fas: CC command: $CC --target=x86_64-unknown-linux-gnu -fPIC " \
  "$CONTAINER_TMP/stderr" >/dev/null; then
  fail "header-only unit without adapters was compiled"
fi

mkdir "$CONTAINER_TMP/duplicate"
cat >"$CONTAINER_TMP/duplicate/first.fas" <<'FAS'
use "C" <<END
int duplicate(void) { return 1; }
END
FAS
cat >"$CONTAINER_TMP/duplicate/second.fas" <<'FAS'
use "C" <<END
int duplicate(void) { return 2; }
END
FAS
cat >"$CONTAINER_TMP/duplicate/main.fas" <<'FAS'
use "first.fas"
use "second.fas"
fn main() i32 { return 0 }
FAS
expect_failure "$OCAML_FAS" "$CONTAINER_TMP/duplicate/main.fas" \
  -o "$CONTAINER_TMP/duplicate/program"
grep -F "multiple definition of" "$CONTAINER_TMP/stderr" \
  >/dev/null || fail "duplicate C definition omitted the linker's message"

"$OCAML_FAS" -S "$CONTAINER_TMP/program.fas" -o "$CONTAINER_TMP/program.s"
[ -s "$CONTAINER_TMP/program.s" ] || fail "-S omitted Fas assembly"
"$OCAML_FAS" --emit-ir "$CONTAINER_TMP/program.fas" >"$CONTAINER_TMP/program.ir"
[ -s "$CONTAINER_TMP/program.ir" ] || fail "--emit-ir omitted Fas IR"

cat >"$CONTAINER_TMP/broken.fas" <<'FAS'
use "C" <<END
int broken(void) { return missing_name; }
END
fn main() i32 { return 0 }
FAS
for mode in -S --emit-llvm --emit-ir; do
  expect_failure "$OCAML_FAS" "$mode" "$CONTAINER_TMP/broken.fas"
  grep -F "$CONTAINER_TMP/broken.fas:2:" "$CONTAINER_TMP/stderr" >/dev/null \
    || fail "$mode C diagnostic was not mapped to the Fas line"
  grep -F "undeclared identifier 'missing_name'" "$CONTAINER_TMP/stderr" \
    >/dev/null || fail "$mode omitted the first C error"
done

"$OCAML_FAS" --keep -debug "$CONTAINER_TMP/program.fas" \
  -o "$CONTAINER_TMP/kept" >"$CONTAINER_TMP/stdout" 2>"$CONTAINER_TMP/stderr"
grep -F "fas: kept C import unit:" "$CONTAINER_TMP/stderr" >/dev/null \
  || fail "--keep omitted the generated C unit"
grep -F "fas: kept C fragment:" "$CONTAINER_TMP/stderr" >/dev/null \
  || fail "--keep omitted the generated fragment"
grep -F "fas: kept C object:" "$CONTAINER_TMP/stderr" >/dev/null \
  || fail "--keep omitted the generated C object"
grep -F "fas: CC command: $CC --target=x86_64-unknown-linux-gnu -fPIC -O0 -c " \
  "$CONTAINER_TMP/stderr" >/dev/null || fail "-debug omitted the C compile command"

echo "c_container: automatic C build, object merge and mapped errors: ok"
