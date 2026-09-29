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
fn main() i32 { return helper() - 7 }
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
for flag in --emit-ir --emit-llvm --emit-asm --emit-obj --keep -O0..-O3 -debug -no-inline; do
  grep -F -- "$flag" "$WORK/help" >/dev/null || fail "help omitted $flag"
done
if grep -F -- "--emit-ast" "$WORK/help" >/dev/null; then fail "help advertises removed --emit-ast"; fi
grep -F -- "LLVM_OPT, LLVM_LLC, CC" "$WORK/help" >/dev/null || fail "help omitted tool overrides"
grep -F -- "fallback tool aliases" "$WORK/help" >/dev/null || fail "help omitted legacy tool aliases"
grep -F -- "FAS_OPT_PASSES" "$WORK/help" >/dev/null || fail "help omitted pass override"

for level in 0 1 2 3; do
  "$OCAML_FAS" "-O$level" "$WORK/good.fas" -o "$WORK/opt-$level" >"$WORK/stdout" 2>"$WORK/stderr"
  [ ! -s "$WORK/stdout" ] || fail "executable wrote stdout at -O$level"
  [ ! -s "$WORK/stderr" ] || fail "executable wrote stderr at -O$level"
  "$WORK/opt-$level" || fail "executable failed at -O$level"
done
"$OCAML_FAS" "$WORK/part-a.fas" "$WORK/part-b.fas" -o "$WORK/multi-input" >"$WORK/stdout" 2>"$WORK/stderr"
"$WORK/multi-input" || fail "multiple input files did not share a declaration scope"
"$OCAML_FAS" "$WORK/part-b.fas" "$WORK/part-a.fas" -o "$WORK/multi-input-reversed" >"$WORK/stdout" 2>"$WORK/stderr"
"$WORK/multi-input-reversed" || fail "multiple input declaration order changed resolution"
for level in 0 1 2 3; do
  grep -F -- "default<O$level>" "$TOOL_LOG" >/dev/null || fail "-O$level opt pipeline missing"
  grep -F -- "llc -O$level" "$TOOL_LOG" >/dev/null || fail "-O$level llc level missing"
done
grep -F -- "-passes=verify" "$TOOL_LOG" >/dev/null || fail "LLVM verification missing"
[ "$(grep -c -- "-passes=verify" "$TOOL_LOG")" -ge 8 ] || fail "LLVM was not verified before and after optimization"
grep -F -- "-verify-each" "$TOOL_LOG" >/dev/null || fail "pass-by-pass verification missing"
grep -F -- "cc " "$TOOL_LOG" >/dev/null || fail "CC override was ignored"
temps_empty

"$OCAML_FAS" --emit-llvm -O2 "$WORK/good.fas" >"$WORK/raw.ll" 2>"$WORK/stderr"
[ ! -s "$WORK/stderr" ] || fail "LLVM emission wrote diagnostics"
grep -F "define i32 @main" "$WORK/raw.ll" >/dev/null || fail "LLVM emission omitted main"
grep -F "call i32 @helper" "$WORK/raw.ll" >/dev/null || fail "-O2 changed unoptimized LLVM emission"
"$OCAML_FAS" --emit-ir "$WORK/good.fas" >"$WORK/custom.ir" 2>"$WORK/stderr"
[ ! -s "$WORK/stderr" ] || fail "custom IR emission wrote diagnostics"
grep -F "Module {" "$WORK/custom.ir" >/dev/null || fail "custom IR emission omitted module"
"$OCAML_FAS" --emit-llvm -debug "$WORK/good.fas" >"$WORK/debug.ll" 2>"$WORK/stderr"
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

"$OCAML_FAS" -debug --keep "$WORK/good.fas" -o "$WORK/debug" >"$WORK/stdout" 2>"$WORK/stderr"
[ ! -s "$WORK/stdout" ] || fail "--keep wrote to stdout"
grep -F "fas: kept intermediates:" "$WORK/stderr" >/dev/null || fail "--keep omitted paths"
grep -F "LLVM_OPT=$LLVM_OPT" "$WORK/stderr" >/dev/null || fail "--keep omitted tools"
grep -F "passes=default<O0>" "$WORK/stderr" >/dev/null || fail "-debug did not default to O0"
grep -F "llc -O0" "$WORK/stderr" >/dev/null || fail "-debug llc level missing"
[ -n "$(find "$TMPDIR" -mindepth 1 -print -quit)" ] || fail "--keep did not retain intermediates"
"$OCAML_FAS" -debug -O3 --keep "$WORK/good.fas" -o "$WORK/debug-o3" >"$WORK/stdout" 2>"$WORK/stderr"
grep -F "passes=default<O3>" "$WORK/stderr" >/dev/null || fail "-debug overrode explicit -O3"
find "$TMPDIR" -mindepth 1 -delete
temps_empty
FAS_OPT_PASSES='default<O1>' "$OCAML_FAS" --keep "$WORK/good.fas" -o "$WORK/custom-passes" >"$WORK/stdout" 2>"$WORK/stderr"
grep -F "passes=default<O1>" "$WORK/stderr" >/dev/null || fail "FAS_OPT_PASSES override was not reported"
grep -F -- "-passes=default<O1>" "$TOOL_LOG" >/dev/null || fail "FAS_OPT_PASSES override was ignored"
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

"$OCAML_FAS" --emit-llvm -debug -no-inline helper "$WORK/good.fas" >"$WORK/noinline.ll" 2>"$WORK/stderr"
grep -F "@helper() noinline {" "$WORK/noinline.ll" >/dev/null || fail "-no-inline missed selected function"
if grep -F "@main() noinline {" "$WORK/noinline.ll" >/dev/null; then fail "-no-inline marked main"; fi
expect_failure "$OCAML_FAS" -no-inline helper "$WORK/good.fas"
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
