#!/bin/sh
set -eu
export DEBUGINFOD_URLS=

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
START_DIR=$(pwd)
CC=$(printenv CC || printf 'clang-22')
LLVM_OPT=$(printenv LLVM_OPT || printf 'opt-22')
LLVM_LLC=$(printenv LLVM_LLC || printf 'llc-22')
OCAML_FAS=$(printenv OCAML_FAS || printf '%s/_build/default/bin/main.exe' "$ROOT")
PROFDATA=$(printenv FAS_PROFDATA || printf 'llvm-profdata-22')

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

fail() {
  echo "profile_tune: $*" >&2
  exit 1
}

CC=$(resolve_tool_path "$CC")
LLVM_OPT=$(resolve_tool_path "$LLVM_OPT")
LLVM_LLC=$(resolve_tool_path "$LLVM_LLC")
OCAML_FAS=$(resolve_tool_path "$OCAML_FAS")
PROFDATA=$(resolve_tool_path "$PROFDATA")

for tool in "$CC" "$LLVM_OPT" "$LLVM_LLC" "$PROFDATA"; do
  command -v "$tool" >/dev/null 2>&1 || fail "required tool not found: $tool (set FAS_PROFDATA if llvm-profdata-22 is named differently)"
done
[ -x "$OCAML_FAS" ] || fail "fas binary not found: $OCAML_FAS (run dune build @all first)"
PROFILE_RT=$("$CC" -print-file-name=libclang_rt.profile-x86_64.a) || fail "cannot query $CC for the profile runtime"
case "$PROFILE_RT" in
  /*) [ -f "$PROFILE_RT" ] || fail "profile runtime is missing: $PROFILE_RT" ;;
  *) fail "libclang_rt.profile-x86_64.a is missing for $CC (install the clang profile runtime)" ;;
esac

export FAS_CC="$CC" FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_PROFDATA="$PROFDATA"

PROFILE_TMP=$(mktemp -d)
WORK=$PROFILE_TMP/work
TMPDIR=$PROFILE_TMP/tmp
mkdir -p "$WORK" "$TMPDIR"
export TMPDIR
trap 'rm -rf "$PROFILE_TMP"' EXIT HUP INT TERM
cd "$WORK"

cat >tune.fas <<'FAS'
use "C" <<PROF
int prof_block_bump(int value) {
  return value * 3 + 1;
}
PROF

extern "C" { fn prof_link_bump(value i32) i32 }

fn main() i32 {
  total i32 = 0
  i i32 = 0
  while i < 4096 {
    if i % 64 == 0 {
      total = total + prof_block_bump(i)
    } else {
      total = total + prof_link_bump(i)
    }
    i = i + 1
  }
  if total == 0 {
    return 1
  }
  return 0
}
FAS

cat >prof_link.c <<'C'
int prof_link_bump(int value) {
  return value * 5 - 2;
}
C

cat >fork.fas <<'FAS'
use "C" <<FORK
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
__attribute__((noinline)) int child_only_path(int value) {
  return value * 7 + 3;
}
int fork_child_bump(int value) {
  pid_t pid = fork();
  if (pid == 0) {
    exit(child_only_path(value) == 3 ? 0 : 1);
  }
  int status = 0;
  waitpid(pid, &status, 0);
  return value * 3 + 1;
}
FORK

fn main() i32 {
  total i32 = 0
  i i32 = 0
  while i < 3 {
    total = total + fork_child_bump(i)
    i = i + 1
  }
  if total == 0 {
    return 1
  }
  return 0
}
FAS

cat >status.fas <<'FAS'
fn main() i32 {
  return 7
}
FAS

cat >abort.fas <<'FAS'
use "C" <<ABORT
#include <stdlib.h>
int abort_now(int value) {
  if (value == 0) {
    abort();
  }
  return value;
}
ABORT

fn main() i32 {
  return abort_now(0)
}
FAS

cat >leave.fas <<'FAS'
use "C" <<LEAVE
#include <unistd.h>
int leave_now(int value) {
  _exit(value);
  return value;
}
LEAVE

fn main() i32 {
  return leave_now(0)
}
FAS

cat >fake-gcc <<'SH'
#!/bin/sh
echo "gcc version 13.2.0 (fake)"
SH
chmod +x fake-gcc

no_tune_temps() {
  leftovers=$(ls -A "$TMPDIR")
  [ -z "$leftovers" ] || fail "temporary files left behind in TMPDIR: $leftovers"
  for leftover in "$WORK"/*.tmp; do
    if [ -e "$leftover" ]; then
      fail "atomic write temporary left behind: $leftover"
    fi
  done
}

tune_expect() {
  name=$1
  expected_status=$2
  expected=$3
  shift 3
  status=0
  "$OCAML_FAS" perf --tune "$@" >"$name.out" 2>"$name.err" || status=$?
  [ "$status" -eq "$expected_status" ] || fail "$name: exit status $status, expected $expected_status"
  actual=$(cat "$name.err")
  [ "$actual" = "$expected" ] || fail "$name: stderr is '$actual', expected '$expected'"
  [ ! -e "$name.fasprof" ] || fail "$name: an output file was written"
  no_tune_temps
}

FAS_VERSION=$("$OCAML_FAS" --version | sed -n '1p' | awk '{ print $2 }')
OPT_LINE=$("$LLVM_OPT" --version | sed -n 's/^[[:space:]]*//;/LLVM version/{p;q;}')
HOST_CPU=$("$LLVM_LLC" --version | sed -n 's/^[[:space:]]*Host CPU: *//p')
{
  echo 'fas profile 3'
  echo "fas $FAS_VERSION"
  echo "llvm $OPT_LINE"
  echo "cpu $HOST_CPU"
  echo 'choice pgo'
} >expected-header.txt

FAS_TUNE_TEST_SCALE=pgo=0.01
export FAS_TUNE_TEST_SCALE

last_line() {
  tail -n 1 "$1"
}

body() {
  tail -n +11 "$1"
}

check_shape() {
  sed -n '6p' "$1" | grep -E -q '^align-loops (def|32|64)$' || fail "$1: line 6 is '$(sed -n '6p' "$1")'"
  sed -n '7p' "$1" | grep -E -q '^align-functions (def|64)$' || fail "$1: line 7 is '$(sed -n '7p' "$1")'"
  sed -n '8p' "$1" | grep -E -q '^options( (unroll|O3))*$' || fail "$1: line 8 is '$(sed -n '8p' "$1")'"
  sed -n '9p' "$1" | grep -E -q '^measured [0-9]+\.[0-9]{4} band [0-9]+\.[0-9]{4} rounds 7 placements (9|3) metric (cycles|wall)$' || fail "$1: line 9 is '$(sed -n '9p' "$1")'"
  [ "$(sed -n '10p' "$1")" = counts ] || fail "$1: line 10 is '$(sed -n '10p' "$1")'"
}

status=0
"$OCAML_FAS" perf --tune tune.fas prof_link.c -o tune.fasprof >tune.out 2>tune.err || status=$?
[ "$status" -eq 0 ] || { cat tune.err >&2; fail "tune run exited $status, expected 0"; }
[ "$(last_line tune.err)" = 'fas perf: wrote tune.fasprof (3 functions)' ] || fail "tune run stderr is '$(cat tune.err)'"
grep -E -q '^fas perf: kept the profile \(pgo [0-9.]+×, band [0-9.]+%' tune.err || fail "tune run did not keep pgo: $(cat tune.err)"
[ ! -s tune.out ] || fail "tune run wrote to stdout"
head -n 5 tune.fasprof >tune-header.txt
cmp tune-header.txt expected-header.txt || fail "tune profile header differs from the expected header"
check_shape tune.fasprof
body tune.fasprof >tune-body.proftext
"$PROFDATA" show tune-body.proftext >tune-show.txt 2>&1 || fail "llvm-profdata cannot show the tune profile body"
grep -F -q 'Total functions: 3' tune-show.txt || fail "tune profile does not hold 3 functions: $(cat tune-show.txt)"
no_tune_temps

status=0
"$OCAML_FAS" --profile tune.fasprof --emit-llvm=optimized tune.fas >tune-profiled.ll 2>tune-profiled.err || status=$?
[ "$status" -eq 0 ] || { cat tune-profiled.err >&2; fail "profiled emit exited $status"; }
[ ! -s tune-profiled.err ] || fail "profiled emit wrote diagnostics: $(cat tune-profiled.err)"
grep -F -q 'function_entry_count", i64 1' tune-profiled.ll || fail "profiled IR has no function entry count"
grep -F -q 'branch_weights", i32 64, i32 4032' tune-profiled.ll || fail "profiled IR has no branch weights from the tune run"
"$OCAML_FAS" --emit-llvm=optimized tune.fas >tune-plain.ll
! grep -F -q 'function_entry_count' tune-plain.ll || fail "unprofiled IR has a function entry count"
! grep -F -q 'branch_weights' tune-plain.ll || fail "unprofiled IR has branch weights"

status=0
"$OCAML_FAS" perf --tune tune.fas prof_link.c -o tune-again.fasprof >tune-again.out 2>tune-again.err || status=$?
[ "$status" -eq 0 ] || fail "second tune run exited $status"
head -n 5 tune-again.fasprof | cmp - expected-header.txt || fail "the second tune run kept another choice"
body tune-again.fasprof | cmp - tune-body.proftext || fail "two tune runs wrote different counts"
no_tune_temps

cat >static.fas <<'FAS'
use "C" <<PROF
__attribute__((noinline)) static int prof_static_step(int value) {
  return value % 7 ? value * 3 : value + 1;
}
int prof_static_sum(int count) {
  int total = 0;
  for (int i = 0; i < count; i++) total += prof_static_step(i);
  return total;
}
PROF

fn main() i32 {
  if prof_static_sum(2048) == 0 {
    return 1
  }
  return 0
}
FAS

cat >cc-unit.sh <<SH
#!/bin/sh
"$CC" "\$@" || exit
case "\$*" in
  *-fprofile-use=*fas-c-import-*.c\ -o*) ;;
  *) exit 0 ;;
esac
set -- "\$@" --end
while [ "\$1" != --end ]; do
  arg=\$1
  shift
  case "\$arg" in
    -c) ;;
    -o) shift ;;
    *) set -- "\$@" "\$arg" ;;
  esac
done
shift
exec "$CC" "\$@" -S -emit-llvm -o "$WORK/static-unit.ll"
SH
chmod +x cc-unit.sh

status=0
"$OCAML_FAS" perf --tune static.fas -o static.fasprof >static.out 2>static.err || status=$?
[ "$status" -eq 0 ] || { cat static.err >&2; fail "static tune run exited $status"; }
grep -E -q '^fas-c-import-[0-9a-f]+\.c;prof_static_step$' static.fasprof || fail "the static C function profile name is not stable: $(grep 'prof_static_step' static.fasprof)"
"$OCAML_FAS" perf --tune static.fas -o static-again.fasprof >static-again.out 2>static-again.err || fail "second static tune run failed"
body static.fasprof >static-body.proftext
body static-again.fasprof | cmp - static-body.proftext || fail "two static tune runs wrote different counts"
FAS_CC="$WORK/cc-unit.sh" "$OCAML_FAS" --profile static.fasprof static.fas -o static.bin 2>static-build.err || { cat static-build.err >&2; fail "static profile build failed"; }
[ -f static-unit.ll ] || fail "the C unit was not compiled with the profile"
grep -E -q 'define internal [^{]*@prof_static_step\(.*!prof' static-unit.ll || fail "the static C function gets no profile counts"
no_tune_temps

status=0
"$OCAML_FAS" perf --tune fork.fas -o fork.fasprof >fork.out 2>fork.err || status=$?
[ "$status" -eq 0 ] || { cat fork.err >&2; fail "fork tune run exited $status, expected 0"; }
[ "$(last_line fork.err)" = 'fas perf: wrote fork.fasprof (3 functions)' ] || fail "fork tune run stderr is '$(cat fork.err)'"
child_count=$(awk '/^child_only_path$/ { found = 1; next } found && /^# Counter Values:$/ { getline; print; exit }' fork.fasprof)
[ "$child_count" = 3 ] || fail "child-only counter is '$child_count', expected 3"
no_tune_temps

tune_expect status 1 'error: the program exited with status 7; no profile written' status.fas -o status.fasprof
tune_expect abort 1 'error: the program was killed by signal SIGABRT; no profile written' abort.fas -o abort.fasprof
tune_expect leave 1 'error: the program wrote no profile (it must return from `main` or call `exit`)' leave.fas -o leave.fasprof
(
  FAS_CC="$WORK/fake-gcc"
  export FAS_CC
  tune_expect cc 1 'error: `fas perf --tune` needs Clang as the C compiler; FAS_CC is `'"$WORK"'/fake-gcc`' tune.fas -o cc.fasprof
)

mkdir defaults
cp tune.fas prof_link.c defaults/
status=0
"$OCAML_FAS" perf --tune defaults/tune.fas defaults/prof_link.c >default.out 2>default.err || status=$?
[ "$status" -eq 0 ] || fail "default output tune run exited $status"
[ "$(last_line default.err)" = 'fas perf: wrote defaults/tune.fasprof (3 functions)' ] || fail "default output stderr is '$(cat default.err)'"
[ -f defaults/tune.fasprof ] || fail "default output defaults/tune.fasprof was not written"
body defaults/tune.fasprof | cmp - tune-body.proftext || fail "default output differs from the -o output"
no_tune_temps

status=0
FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100 "$OCAML_FAS" perf --tune tune.fas prof_link.c -o slow.fasprof >slow.out 2>slow.err || status=$?
[ "$status" -eq 0 ] || { cat slow.err >&2; fail "slow tune run exited $status"; }
grep -E -q '^fas perf: kept the default build \(best (pgo|native|native-pgo) 0\.[0-9]+×, band [0-9.]+%\) \(measured on one input\)$' slow.err || fail "slow tune run did not keep the default build: $(cat slow.err)"
[ "$(sed -n '5p;6p' slow.fasprof)" = "$(printf 'choice default\nalign-loops def')" ] || fail "slow profile header is '$(head -n 7 slow.fasprof)'"
grep -E -q '^measured 1\.0000 band ' slow.fasprof || fail "slow profile measured line is '$(sed -n '9p' slow.fasprof)'"
grep -q 'tuned for' slow.err && fail "a default choice says it was tuned for this CPU"
"$OCAML_FAS" --profile slow.fasprof -c tune.fas -o slow.o
"$OCAML_FAS" -c tune.fas -o plain.o
cmp slow.o plain.o || fail "the kept default build differs from a build without a profile"
no_tune_temps

status=0
FAS_TUNE_TEST_SCALE=native-pgo=0.01 "$OCAML_FAS" perf --tune tune.fas prof_link.c -o native.fasprof >native.out 2>native.err || status=$?
[ "$status" -eq 0 ] || { cat native.err >&2; fail "native tune run exited $status"; }
[ "$(sed -n '5p' native.fasprof)" = 'choice native-pgo' ] || fail "native tune kept '$(sed -n '5p' native.fasprof)'"
grep -F -q "fas perf: tuned for $HOST_CPU; the binary may not run on other CPUs" native.err || fail "native tune run did not name the CPU: $(cat native.err)"
"$OCAML_FAS" --profile native.fasprof --emit-llvm=optimized tune.fas >native-profiled.ll
grep -F -q 'branch_weights' native-profiled.ll || fail "the native-pgo profile carries no counts"
no_tune_temps

status=0
FAS_TUNE_TEST_SCALE=pgo=0.01 "$OCAML_FAS" perf --tune --target=force-native tune.fas prof_link.c -o forced.fasprof >forced.out 2>forced.err || status=$?
[ "$status" -eq 0 ] || { cat forced.err >&2; fail "forced native tune run exited $status"; }
[ "$(sed -n '5p' forced.fasprof)" = 'choice native-pgo' ] || fail "forced native tune kept '$(sed -n '5p' forced.fasprof)'"
grep -E -q '^placement +default +pgo +ratio$' forced.err || fail "forced native tune table names other candidates: $(cat forced.err)"
FAS_TUNE_TEST_SCALE=pgo=100 "$OCAML_FAS" perf --tune --target=force-native tune.fas prof_link.c -o forced-default.fasprof >forced-default.out 2>forced-default.err || fail "forced native default run failed"
[ "$(sed -n '5p' forced-default.fasprof)" = 'choice native' ] || fail "forced native default kept '$(sed -n '5p' forced-default.fasprof)'"
"$OCAML_FAS" --profile forced-default.fasprof -c tune.fas -o forced-default.o
"$OCAML_FAS" --target=force-native -c tune.fas -o forced-plain.o
cmp forced-default.o forced-plain.o || fail "the kept native default differs from --target=force-native"
no_tune_temps

tune_expect combo-static 2 '`--tune` cannot be combined with `--static`' tune.fas --static -o combo-static.fasprof
tune_expect combo-json 2 '`--tune` cannot be combined with `--json`' tune.fas --json -o combo-json.fasprof
tune_expect combo-compare 2 '`--tune` cannot be combined with `--compare`' tune.fas --compare tune.fasprof -o combo-compare.fasprof
tune_expect combo-top 2 '`--tune` cannot be combined with `--top`' tune.fas --top 3 -o combo-top.fasprof
tune_expect combo-asm 2 '`--tune` cannot be combined with `--asm`' tune.fas --asm tune.fas:1 -o combo-asm.fasprof
tune_expect combo-profile 2 '`--tune` cannot be combined with `--profile`' tune.fas --profile tune.fasprof -o combo-profile.fasprof

echo "profile_tune: ok"
