#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
START_DIR=$(pwd)
CC=$(printenv CC || printf 'clang-22')
LLVM_OPT=$(printenv LLVM_OPT || printf 'opt-22')
LLVM_LLC=$(printenv LLVM_LLC || printf 'llc-22')
OCAML_FAS=$(printenv OCAML_FAS || printf '%s/_build/default/bin/main.exe' "$ROOT")

resolve_tool_path() {
  case "$1" in
    /*) printf '%s\n' "$1" ;;
    */*) printf '%s/%s\n' "$START_DIR" "$1" ;;
    *)
      resolved=$(command -v "$1" 2>/dev/null || true)
      case "$resolved" in
        /*) printf '%s\n' "$resolved" ;;
        */*) printf '%s/%s\n' "$START_DIR" "$resolved" ;;
        *) printf '%s\n' "$1" ;;
      esac
      ;;
  esac
}

CC=$(resolve_tool_path "$CC")
LLVM_OPT=$(resolve_tool_path "$LLVM_OPT")
LLVM_LLC=$(resolve_tool_path "$LLVM_LLC")
OCAML_FAS=$(resolve_tool_path "$OCAML_FAS")
REAL_CC=$(command -v "$CC")
REAL_OPT=$(command -v "$LLVM_OPT")
REAL_LLC=$(command -v "$LLVM_LLC")
unset FAS_OPT_PASSES
DRIVER_TMP=$(mktemp -d)
WORK=$DRIVER_TMP/work
TMPDIR=$DRIVER_TMP/tmp
mkdir -p "$WORK" "$TMPDIR"
export TMPDIR
trap 'rm -rf "$DRIVER_TMP"' EXIT HUP INT TERM

cat >"$WORK/good.fas" <<'FAS'
fn helper() i32 { return 7 }
fn main() i32 { return helper() - 7 }
FAS
cat >"$WORK/bad.fas" <<'FAS'
fn main() i32 { return missing }
FAS
cat >"$WORK/part-a.fas" <<'FAS'
fn helper() i32 { return 7 }
FAS
cat >"$WORK/part-b.fas" <<'FAS'
fn another() i32 { return 3 }
FAS
cat >"$WORK/link.fas" <<'FAS'
extern "C" {
  fn c_link_probe(value i32) i32
}
fn main() i32 { return c_link_probe(1) - 2 }
FAS
cat >"$WORK/helper.c" <<'C'
#include <math.h>
int c_link_probe(int value) {
  return value + (sin((double)value) > 0.0);
}
C
cat >"$WORK/c-sanitize.fas" <<'FAS'
use "C" <<SANITIZE_C
int fas_sanitize_c(void) { return 5; }
SANITIZE_C
extern "C" { fn fas_sanitize_c() i32 }
fn main() i32 { return fas_sanitize_c() - 5 }
FAS
cat >"$WORK/lifetime-defer.fas" <<'FAS'
extern "C" { fn observe(p addr) void }
fn main() i32 {
  {
    value i32 = 5
    defer { observe(&value) }
    observe(&value)
  }
  return 0
}
FAS
cat >"$WORK/use-main.fas" <<'FAS'
use "part-a.fas"
use "part-b.fas"
fn main() i32 { return helper() + another() - 10 }
FAS
cat >"$WORK/use-main-reversed.fas" <<'FAS'
use "part-b.fas"
use "part-a.fas"
fn main() i32 { return helper() + another() - 10 }
FAS
cat >"$WORK/opt-wrap" <<'SH'
#!/bin/sh
printf 'opt %s\n' "$*" >> "$TOOL_LOG"
exec "$REAL_OPT" "$@"
SH
cat >"$WORK/llc-wrap" <<'SH'
#!/bin/sh
printf 'llc %s\n' "$*" >> "$TOOL_LOG"
exec "$REAL_LLC" "$@"
SH
cat >"$WORK/cc-wrap" <<'SH'
#!/bin/sh
printf 'cc %s\n' "$*" >> "$TOOL_LOG"
exec "$REAL_CC" "$@"
SH
chmod +x "$WORK/opt-wrap" "$WORK/llc-wrap" "$WORK/cc-wrap"
LLVM_OPT=$WORK/opt-wrap
LLVM_LLC=$WORK/llc-wrap
CC=$WORK/cc-wrap
FAS_OPT=$WORK/missing-old-opt
FAS_LLC=$WORK/missing-old-llc
FAS_CC=$WORK/missing-old-cc
TOOL_LOG=$WORK/tools.log
export LLVM_OPT LLVM_LLC CC FAS_OPT FAS_LLC FAS_CC REAL_OPT REAL_LLC REAL_CC TOOL_LOG

fail() {
  echo "driver: $*" >&2
  exit 1
}
expect_failure() {
  status=0
  "$@" >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
  [ "$status" -ne 0 ] || fail "command unexpectedly succeeded: $*"
  [ ! -s "$WORK/stdout" ] || fail "failure wrote to stdout: $*"
  [ -s "$WORK/stderr" ] || fail "failure omitted stderr: $*"
}
temps_empty() {
  [ -z "$(find "$TMPDIR" -mindepth 1 -print -quit)" ] || fail "temporary files remain"
}

"$OCAML_FAS" --help >"$WORK/help" 2>"$WORK/stderr"
[ ! -s "$WORK/stderr" ] || fail "help wrote to stderr"
"$OCAML_FAS" -h >"$WORK/short-help" 2>"$WORK/stderr"
cmp "$WORK/help" "$WORK/short-help" >/dev/null || fail "-h differs from --help"
for flag in -o --emit-header --emit-ir --emit-ir-json --emit-llvm --emit-asm -S --emit-obj -c --keep \
  -O0..-O3 -g --no-inline --sanitize=LIST -I -isystem -D -h --help; do
  grep -F -- "$flag" "$WORK/help" >/dev/null || fail "help omitted $flag"
done
if grep -F -- "--emit-ast" "$WORK/help" >/dev/null; then fail "help advertises removed --emit-ast"; fi
grep -F -- "LLVM_OPT, LLVM_LLC, CC" "$WORK/help" >/dev/null || fail "help omitted tool overrides"
grep -F -- "fallback tool aliases" "$WORK/help" >/dev/null || fail "help omitted legacy tool aliases"
grep -F -- "FAS_OPT_PASSES" "$WORK/help" >/dev/null || fail "help omitted pass override"
expect_failure "$OCAML_FAS" --sanitize= "$WORK/good.fas"
grep -Fx -- "--sanitize requires a non-empty list" "$WORK/stderr" >/dev/null || fail "empty sanitizer list diagnostic changed"
for name in memory thread foo; do
  expect_failure "$OCAML_FAS" "--sanitize=$name" "$WORK/good.fas"
  grep -Fx -- "unknown sanitizer \`$name\`" "$WORK/stderr" >/dev/null || fail "unknown sanitizer diagnostic changed"
done
expect_failure "$OCAML_FAS" --sanitize=address --sanitize=undefined "$WORK/good.fas"
grep -Fx -- "conflicting --sanitize options" "$WORK/stderr" >/dev/null || fail "conflicting sanitizer diagnostic changed"
"$OCAML_FAS" --sanitize=address,address --sanitize=address --emit-ir "$WORK/good.fas" \
  >"$WORK/sanitized-custom.ir" 2>"$WORK/stderr"
[ ! -s "$WORK/stderr" ] || fail "repeated sanitizer names were rejected"

for level in 0 1 2 3; do
  "$OCAML_FAS" "-O$level" "$WORK/good.fas" -o "$WORK/opt-$level" >"$WORK/stdout" 2>"$WORK/stderr"
  [ ! -s "$WORK/stdout" ] || fail "executable wrote stdout at -O$level"
  [ ! -s "$WORK/stderr" ] || fail "executable wrote stderr at -O$level"
  "$WORK/opt-$level" || fail "executable failed at -O$level"
done
"$OCAML_FAS" "$WORK/use-main.fas" -o "$WORK/use-input" >"$WORK/stdout" 2>"$WORK/stderr"
"$WORK/use-input" || fail "use dependencies did not share a declaration scope"
"$OCAML_FAS" "$WORK/use-main-reversed.fas" -o "$WORK/use-input-reversed" >"$WORK/stdout" 2>"$WORK/stderr"
"$WORK/use-input-reversed" || fail "use declaration order changed resolution"
expect_failure "$OCAML_FAS" "$WORK/part-a.fas" "$WORK/part-b.fas"
grep -Fx 'multiple input files are not supported; use "path.fas" for dependencies' \
  "$WORK/stderr" >/dev/null || fail "multiple input diagnostic changed"
for mode in -c -S --emit-ir --emit-ir-json --emit-llvm; do
  expect_failure "$OCAML_FAS" "$WORK/link.fas" "$WORK/helper.c" "$mode"
  grep -Fx 'C inputs and link flags require an executable output' \
    "$WORK/stderr" >/dev/null || fail "$mode accepted C link inputs"
done
for mode in -c -S --emit-ir --emit-ir-json --emit-llvm --emit-header; do
  "$OCAML_FAS" "$mode" "$WORK/good.fas" -o /dev/null >"$WORK/stdout" 2>"$WORK/stderr" \
    || fail "$mode could not write to a non-regular output"
  [ ! -s "$WORK/stdout" ] || fail "$mode wrote stdout with an explicit output"
  [ ! -s "$WORK/stderr" ] || fail "$mode wrote stderr with a valid input"
done
"$OCAML_FAS" "$WORK/good.fas" -o /dev/null >"$WORK/stdout" 2>"$WORK/stderr" \
  || fail "executable could not write to a non-regular output"
[ ! -s "$WORK/stdout" ] || fail "executable wrote stdout with an explicit output"
[ ! -s "$WORK/stderr" ] || fail "executable wrote stderr with a valid input"
for level in 0 1 2 3; do
  grep -F -- "default<O$level>" "$TOOL_LOG" >/dev/null || fail "-O$level opt pipeline missing"
  grep -F -- "llc -O$level -relocation-model=pic" "$TOOL_LOG" >/dev/null || fail "-O$level llc PIC model missing"
done
if grep -F -- "-no-pie" "$TOOL_LOG" >/dev/null; then fail "executable link disabled PIE"; fi
grep -F -- "-passes=verify" "$TOOL_LOG" >/dev/null || fail "LLVM verification missing"
[ "$(grep -c -- "-passes=verify" "$TOOL_LOG")" -ge 8 ] || fail "LLVM was not verified before and after optimization"
if grep -F -- "-verify-each" "$TOOL_LOG" >/dev/null; then
  fail "pass-by-pass verification is enabled"
fi
grep -F -- "cc " "$TOOL_LOG" >/dev/null || fail "CC override was ignored"
temps_empty

cat >"$WORK/c-import.fas" <<FAS
use "C" "$ROOT/test/c_import/macros.h"
fn main() i32 { return FAS_MACRO_ALIAS - 41 }
FAS
cat >"$WORK/import-cc" <<'SH'
#!/bin/sh
case " $* " in
  *" -ast-dump=json "*|*" -dD "*|*" -emit-llvm "*) printf '%s\n' "$*" >>"$IMPORT_LOG" ;;
esac
exec "$REAL_CC" "$@"
SH
chmod +x "$WORK/import-cc"
CC="$WORK/import-cc" IMPORT_LOG="$WORK/import-clang.log" \
  "$OCAML_FAS" --keep --emit-ir "$WORK/c-import.fas" \
  >"$WORK/c-import.ir" 2>"$WORK/c-import.stderr"
[ "$(wc -l <"$WORK/import-clang.log")" -eq 4 ] \
  || fail "C import used more than four Clang commands"
[ "$(grep -c -- '-ast-dump=json' "$WORK/import-clang.log")" -eq 2 ] \
  || fail "C import did not share its AST dumps"
[ "$(grep -c -- '-dD' "$WORK/import-clang.log")" -eq 1 ] \
  || fail "C import macro scan count changed"
[ "$(grep -c -- '-emit-llvm' "$WORK/import-clang.log")" -eq 1 ] \
  || fail "C import probe translation unit was not shared"
[ "$(grep -c '^fas: kept ' "$WORK/c-import.stderr")" -eq 2 ] \
  || fail "C import --keep listing changed"
grep -E "^fas: kept C import unit: $TMPDIR/fas-c-import-[^ ]+\\.c$" \
  "$WORK/c-import.stderr" >/dev/null || fail "--keep omitted the C import unit"
grep -E "^fas: kept C bindings: $TMPDIR/c-import.bindings-[^ ]+\\.txt$" \
  "$WORK/c-import.stderr" >/dev/null || fail "--keep C import listing changed"
find "$TMPDIR" -mindepth 1 -delete
temps_empty

: >"$TOOL_LOG"
"$OCAML_FAS" -g --keep "$WORK/link.fas" "$WORK/helper.c" -lm -o "$WORK/link" \
  >"$WORK/stdout" 2>"$WORK/stderr"
[ ! -s "$WORK/stdout" ] || fail "C link wrote to stdout"
"$WORK/link" || fail "Fas C and libm link failed"
grep -E "^cc [^ ]*fas-module-[^ ]*\\.s $WORK/helper.c -lm -o $WORK/\\.fas-output-[^ ]+\\.tmp$" \
  "$TOOL_LOG" >/dev/null || fail "tool log omitted the exact CC argument order"
grep -E "^fas: CC command: $CC [^ ]*fas-module-[^ ]*\\.s $WORK/helper.c -lm -o $WORK/\\.fas-output-[^ ]+\\.tmp$" \
  "$WORK/stderr" >/dev/null || fail "debug log omitted the exact CC command"
asm_path=$(sed -n 's/^fas: CC command: [^ ]* \([^ ]*fas-module-[^ ]*\.s\) .*/\1/p' "$WORK/stderr")
[ -s "$asm_path" ] || fail "--keep did not retain the exact assembly input"

: >"$TOOL_LOG"
"$OCAML_FAS" --sanitize=address --keep "$WORK/c-sanitize.fas" -o "$WORK/c-sanitize" \
  >"$WORK/stdout" 2>"$WORK/stderr"
grep -E '^cc --target=x86_64-unknown-linux-gnu -fPIC -O2 -fsanitize=address -c .*\.c -o ' \
  "$TOOL_LOG" >/dev/null || fail "generated C input was not compiled with ASan"
grep -E '^cc [^ ]*fas-module-[^ ]*\.s .* -fsanitize=address -o ' "$TOOL_LOG" >/dev/null \
  || fail "generated C program link omitted ASan runtime"
: >"$TOOL_LOG"
"$OCAML_FAS" --sanitize=address --keep "$WORK/link.fas" "$WORK/helper.c" -lm \
  -o "$WORK/address-c-link" >"$WORK/stdout" 2>"$WORK/stderr"
grep -F -- "-fsanitize=address $WORK/helper.c -lm -o " "$TOOL_LOG" >/dev/null \
  || fail "C input link omitted ASan runtime"
"$OCAML_FAS" --emit-llvm "$WORK/good.fas" >"$WORK/no-sanitizer.ll" 2>"$WORK/stderr"
"$OCAML_FAS" --sanitize=undefined --emit-llvm "$WORK/good.fas" \
  >"$WORK/undefined.ll" 2>"$WORK/stderr"
cmp "$WORK/no-sanitizer.ll" "$WORK/undefined.ll" >/dev/null \
  || fail "undefined sanitizer changed Fas LLVM"
: >"$TOOL_LOG"
"$OCAML_FAS" --sanitize=undefined "$WORK/c-sanitize.fas" -o "$WORK/undefined-c-link" \
  >"$WORK/stdout" 2>"$WORK/stderr"
grep -E '^cc --target=x86_64-unknown-linux-gnu -fPIC -O2 -fsanitize=undefined -c .*\.c -o ' \
  "$TOOL_LOG" >/dev/null || fail "generated C input was not compiled with UBSan"
grep -E '^cc [^ ]*fas-module-[^ ]*\.s .* -fsanitize=undefined -o ' "$TOOL_LOG" >/dev/null \
  || fail "C program link omitted UBSan runtime"
: >"$TOOL_LOG"
"$OCAML_FAS" --sanitize=address,undefined "$WORK/c-sanitize.fas" \
  -o "$WORK/combined-c-link" >"$WORK/stdout" 2>"$WORK/stderr"
grep -E '^cc --target=x86_64-unknown-linux-gnu -fPIC -O2 -fsanitize=address,undefined -c .*\.c -o ' \
  "$TOOL_LOG" >/dev/null || fail "generated C input omitted a combined sanitizer"
grep -E '^cc [^ ]*fas-module-[^ ]*\.s .* -fsanitize=address,undefined -o ' \
  "$TOOL_LOG" >/dev/null || fail "C program link omitted a combined sanitizer"

"$OCAML_FAS" --emit-llvm -O2 "$WORK/good.fas" >"$WORK/raw.ll" 2>"$WORK/stderr"
[ ! -s "$WORK/stderr" ] || fail "LLVM emission wrote diagnostics"
grep -F "define i32 @main" "$WORK/raw.ll" >/dev/null || fail "LLVM emission omitted main"
grep -F "call i32 @helper" "$WORK/raw.ll" >/dev/null || fail "-O2 changed unoptimized LLVM emission"
"$OCAML_FAS" --sanitize=address --emit-llvm "$WORK/link.fas" >"$WORK/address.ll" 2>"$WORK/stderr"
grep -F "define i32 @main() sanitize_address {" "$WORK/address.ll" >/dev/null || fail "address sanitizer missed Fas definition"
grep -F "declare i32 @c_link_probe(i32)" "$WORK/address.ll" >/dev/null || fail "address sanitizer changed external declaration"
if grep -F "declare i32 @c_link_probe(i32) sanitize_address" "$WORK/address.ll" >/dev/null; then
  fail "address sanitizer marked external declaration"
fi
"$OCAML_FAS" --emit-llvm "$WORK/lifetime-defer.fas" >"$WORK/lifetime-plain.ll" 2>"$WORK/stderr"
if grep -F "llvm.lifetime." "$WORK/lifetime-plain.ll" >/dev/null; then
  fail "lifetime markers appeared without address sanitizer"
fi
"$OCAML_FAS" --sanitize=address --emit-llvm "$WORK/lifetime-defer.fas" \
  >"$WORK/lifetime-address.ll" 2>"$WORK/stderr"
start_line=$(grep -n -F "call void @llvm.lifetime.start.p0" "$WORK/lifetime-address.ll" | cut -d: -f1)
end_line=$(grep -n -F "call void @llvm.lifetime.end.p0" "$WORK/lifetime-address.ll" | cut -d: -f1)
defer_line=$(grep -n -F "call void @observe" "$WORK/lifetime-address.ll" | tail -n 1 | cut -d: -f1)
[ -n "$start_line" ] && [ -n "$end_line" ] && [ -n "$defer_line" ] \
  || fail "address sanitizer omitted a local lifetime marker or deferred call"
[ "$start_line" -lt "$defer_line" ] && [ "$defer_line" -lt "$end_line" ] \
  || fail "local lifetime did not start before use and end after defer"
"$OCAML_FAS" --emit-ir "$WORK/good.fas" >"$WORK/custom.ir" 2>"$WORK/stderr"
[ ! -s "$WORK/stderr" ] || fail "custom IR emission wrote diagnostics"
grep -F "Module {" "$WORK/custom.ir" >/dev/null || fail "custom IR emission omitted module"
"$OCAML_FAS" --emit-llvm -g "$WORK/good.fas" >"$WORK/debug.ll" 2>"$WORK/stderr"
if grep -E 'llvm\.dbg|!DI[A-Za-z]+' "$WORK/debug.ll" >/dev/null; then
  fail "debug mode emitted DWARF metadata"
fi

"$OCAML_FAS" -S "$WORK/good.fas" -o "$WORK/good.s" >"$WORK/stdout" 2>"$WORK/stderr"
[ -s "$WORK/good.s" ] || fail "-S did not produce assembly"
[ ! -s "$WORK/stdout" ] || fail "-S wrote to stdout"
"$OCAML_FAS" --emit-asm "$WORK/good.fas" -o "$WORK/alias.s" >"$WORK/stdout" 2>"$WORK/stderr"
[ -s "$WORK/alias.s" ] || fail "--emit-asm did not produce assembly"
"$OCAML_FAS" -c "$WORK/good.fas" -o "$WORK/good.o" >"$WORK/stdout" 2>"$WORK/stderr"
[ -s "$WORK/good.o" ] || fail "-c did not produce an object"
[ "$(od -An -tx1 -N4 "$WORK/good.o" | tr -d ' \n')" = "7f454c46" ] || fail "-c output is not ELF"
: >"$TOOL_LOG"
"$OCAML_FAS" --sanitize=address -c "$WORK/good.fas" -o "$WORK/address.o" >"$WORK/stdout" 2>"$WORK/stderr"
[ -s "$WORK/address.o" ] || fail "sanitized -c did not produce an object"
nm -u "$WORK/address.o" >"$WORK/address-symbols"
grep -F "__asan_" "$WORK/address-symbols" >/dev/null || fail "sanitized object omitted ASan runtime references"
if grep -F "cc " "$TOOL_LOG" >/dev/null; then fail "sanitized -c linked an executable"; fi
"$OCAML_FAS" --emit-obj "$WORK/good.fas" -o "$WORK/alias.o" >"$WORK/stdout" 2>"$WORK/stderr"
[ -s "$WORK/alias.o" ] || fail "--emit-obj did not produce an object"
cp "$WORK/good.fas" "$WORK/auto.fas"
(cd "$WORK" && "$OCAML_FAS" auto.fas >stdout 2>stderr)
[ -x "$WORK/a.out" ] || fail "default executable output path was not used"
[ ! -s "$WORK/stdout" ] && [ ! -s "$WORK/stderr" ] || fail "default executable wrote diagnostics"
"$WORK/a.out" || fail "default executable output failed"
(cd "$WORK" && "$OCAML_FAS" -S auto.fas >stdout 2>stderr)
[ -s "$WORK/auto.s" ] || fail "default assembly output path was not used"
[ ! -s "$WORK/stdout" ] && [ ! -s "$WORK/stderr" ] || fail "default assembly wrote diagnostics"
(cd "$WORK" && "$OCAML_FAS" -c auto.fas >stdout 2>stderr)
[ -s "$WORK/auto.o" ] || fail "default object output path was not used"
[ ! -s "$WORK/stdout" ] && [ ! -s "$WORK/stderr" ] || fail "default object wrote diagnostics"

: >"$TOOL_LOG"
"$OCAML_FAS" -g --keep "$WORK/good.fas" -o "$WORK/debug" >"$WORK/stdout" 2>"$WORK/stderr"
[ ! -s "$WORK/stdout" ] || fail "--keep wrote to stdout"
grep -F "fas: kept intermediates:" "$WORK/stderr" >/dev/null || fail "--keep omitted paths"
grep -F "LLVM_OPT=$LLVM_OPT" "$WORK/stderr" >/dev/null || fail "--keep omitted tools"
grep -F "passes=default<O0>" "$WORK/stderr" >/dev/null || fail "-g did not default to O0"
grep -F "llc -O0" "$WORK/stderr" >/dev/null || fail "-g llc level missing"
grep -F -- "llc -O0 -relocation-model=pic" "$TOOL_LOG" >/dev/null || fail "-g llc PIC model missing"
[ -n "$(find "$TMPDIR" -mindepth 1 -print -quit)" ] || fail "--keep did not retain intermediates"
"$OCAML_FAS" -g -O3 --keep "$WORK/good.fas" -o "$WORK/debug-o3" >"$WORK/stdout" 2>"$WORK/stderr"
grep -F "passes=default<O3>" "$WORK/stderr" >/dev/null || fail "-g overrode explicit -O3"
find "$TMPDIR" -mindepth 1 -delete
temps_empty
FAS_OPT_PASSES='default<O1>' "$OCAML_FAS" --keep "$WORK/good.fas" -o "$WORK/custom-passes" >"$WORK/stdout" 2>"$WORK/stderr"
grep -F "passes=default<O1>" "$WORK/stderr" >/dev/null || fail "FAS_OPT_PASSES override was not reported"
grep -F -- "-passes=default<O1>" "$TOOL_LOG" >/dev/null || fail "FAS_OPT_PASSES override was ignored"
find "$TMPDIR" -mindepth 1 -delete
temps_empty

: >"$TOOL_LOG"
for level in 0 1 2 3; do
  "$OCAML_FAS" --sanitize=address "-O$level" --keep -S "$WORK/good.fas" \
    -o "$WORK/address-$level.s" >"$WORK/stdout" 2>"$WORK/stderr"
  grep -F -- "-passes=asan,default<O$level>" "$TOOL_LOG" >/dev/null || fail "address pass order missing at -O$level"
  grep -F -- "-asan-use-after-scope" "$TOOL_LOG" >/dev/null || fail "address pass omitted scope instrumentation"
done
FAS_OPT_PASSES='default<O1>' "$OCAML_FAS" --sanitize=address --keep -S "$WORK/good.fas" \
  -o "$WORK/address-custom.s" >"$WORK/stdout" 2>"$WORK/stderr"
grep -F -- "-passes=asan,default<O1>" "$TOOL_LOG" >/dev/null || fail "address pass omitted with FAS_OPT_PASSES"
find "$TMPDIR" -mindepth 1 -delete
temps_empty

(
  unset LLVM_OPT LLVM_LLC CC FAS_OPT FAS_LLC FAS_CC
  FAS_OPT="$WORK/opt-wrap" FAS_LLC="$WORK/llc-wrap" FAS_CC="$WORK/cc-wrap" \
    "$OCAML_FAS" --keep "$WORK/good.fas" -o "$WORK/legacy-tools"
) >"$WORK/stdout" 2>"$WORK/stderr"
grep -F "LLVM_OPT=$WORK/opt-wrap" "$WORK/stderr" >/dev/null || fail "FAS_OPT fallback was ignored"
grep -F "LLVM_LLC=$WORK/llc-wrap" "$WORK/stderr" >/dev/null || fail "FAS_LLC fallback was ignored"
grep -F "CC=$WORK/cc-wrap" "$WORK/stderr" >/dev/null || fail "FAS_CC fallback was ignored"
find "$TMPDIR" -mindepth 1 -delete
temps_empty

"$OCAML_FAS" --emit-llvm -g --no-inline helper "$WORK/good.fas" >"$WORK/noinline.ll" 2>"$WORK/stderr"
grep -F "@helper() noinline #0 {" "$WORK/noinline.ll" >/dev/null || fail "--no-inline missed selected function"
if grep -F "@main() noinline #0 {" "$WORK/noinline.ll" >/dev/null; then fail "--no-inline marked main"; fi
expect_failure "$OCAML_FAS" --no-inline helper "$WORK/good.fas"
cp "$WORK/good.fas" "$WORK/same-path.fas"
expect_failure "$OCAML_FAS" "$WORK/same-path.fas" -o "$WORK/same-path.fas"
cmp "$WORK/good.fas" "$WORK/same-path.fas" >/dev/null || fail "input/output collision changed input"

"$OCAML_FAS" -o - "$WORK/good.fas" >"$WORK/stdout" 2>"$WORK/stderr" && fail "-o - was accepted"
[ ! -s "$WORK/stdout" ] || fail "-o - wrote stdout"
grep -F -- "-o - is not supported" "$WORK/stderr" >/dev/null || fail "-o - error unclear"
expect_failure "$OCAML_FAS" -o
grep -F -- "-o requires an output path" "$WORK/stderr" >/dev/null || fail "missing -o argument error unclear"
expect_failure "$OCAML_FAS" --emit-ast "$WORK/good.fas"
grep -F -- "unknown option: --emit-ast" "$WORK/stderr" >/dev/null || fail "removed AST error unclear"

printf 'stale artifact\n' >"$WORK/compile-fail"
expect_failure "$OCAML_FAS" "$WORK/bad.fas" -o "$WORK/compile-fail"
[ ! -e "$WORK/compile-fail" ] || fail "compile failure left stale output"
temps_empty

ir_status=0
"$OCAML_FAS" --emit-ir "$WORK/bad.fas" >"$WORK/ir-failure-stdout" \
  2>"$WORK/ir-failure-stderr" || ir_status=$?
json_status=0
"$OCAML_FAS" --emit-ir-json "$WORK/bad.fas" -o /dev/null \
  >"$WORK/json-failure-stdout" 2>"$WORK/json-failure-stderr" || json_status=$?
[ "$ir_status" -eq "$json_status" ] || fail "JSON emission changed rejected-program status"
cmp "$WORK/ir-failure-stdout" "$WORK/json-failure-stdout" >/dev/null \
  || fail "rejected JSON emission wrote output"
cmp "$WORK/ir-failure-stderr" "$WORK/json-failure-stderr" >/dev/null \
  || fail "JSON emission changed rejected-program diagnostics"
temps_empty

regular_status=0
"$OCAML_FAS" "$WORK/bad.fas" -o "$WORK/compile-fail-regular" \
  >"$WORK/regular-stdout" 2>"$WORK/regular-stderr" || regular_status=$?
device_status=0
"$OCAML_FAS" "$WORK/bad.fas" -o /dev/null \
  >"$WORK/device-stdout" 2>"$WORK/device-stderr" || device_status=$?
[ "$regular_status" -eq "$device_status" ] || fail "non-regular output changed compile-failure status"
cmp "$WORK/regular-stdout" "$WORK/device-stdout" >/dev/null \
  || fail "non-regular output changed compile-failure stdout"
cmp "$WORK/regular-stderr" "$WORK/device-stderr" >/dev/null \
  || fail "non-regular output changed compile diagnostics"
[ ! -e "$WORK/compile-fail-regular" ] || fail "regular output remained after compile failure"
temps_empty

cat >"$WORK/fail-cc" <<'SH'
#!/bin/sh
while [ "$#" -gt 0 ]; do
  if [ "$1" = "-o" ]; then
    shift
    printf 'partial artifact\n' > "$1"
    echo 'link failed' >&2
    exit 19
  fi
  shift
done
exit 20
SH
chmod +x "$WORK/fail-cc"
printf 'stale artifact\n' >"$WORK/link-fail"
status=0
CC=$WORK/fail-cc "$OCAML_FAS" "$WORK/good.fas" -o "$WORK/link-fail" >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
[ "$status" -ne 0 ] || fail "failed link succeeded"
[ ! -s "$WORK/stdout" ] || fail "failed link wrote stdout"
grep -F "link failed" "$WORK/stderr" >/dev/null || fail "link diagnostic missing"
[ ! -e "$WORK/link-fail" ] || fail "link failure left output"
temps_empty

expect_failure "$OCAML_FAS" "$WORK/missing.fas"
grep -F "cannot read" "$WORK/stderr" >/dev/null || fail "missing input error unclear"
expect_failure "$OCAML_FAS" "$WORK"
grep -F "is a directory" "$WORK/stderr" >/dev/null || fail "directory input error unclear"

status=0
LLVM_OPT=$WORK/missing-opt "$OCAML_FAS" --emit-llvm -o "$WORK/tool-fail.ll" "$WORK/good.fas" >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
[ "$status" -ne 0 ] || fail "missing LLVM_OPT succeeded"
[ ! -s "$WORK/stdout" ] || fail "missing LLVM_OPT wrote stdout"
grep -F "LLVM verification" "$WORK/stderr" >/dev/null || fail "missing tool error unclear"
[ ! -e "$WORK/tool-fail.ll" ] || fail "tool failure left output"
find "$TMPDIR" -mindepth 1 -delete
temps_empty

echo "driver: passed"
