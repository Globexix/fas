#!/bin/sh
set -eu
export DEBUGINFOD_URLS=

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

fail() {
  echo "tune_measure: $*" >&2
  exit 1
}

CC=$(resolve_tool_path "$CC")
LLVM_OPT=$(resolve_tool_path "$LLVM_OPT")
LLVM_LLC=$(resolve_tool_path "$LLVM_LLC")
OCAML_FAS=$(resolve_tool_path "$OCAML_FAS")
export FAS_CC="$CC" FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC"

MEASURE_TMP=$(mktemp -d)
WORK=$MEASURE_TMP/work
TMPDIR=$MEASURE_TMP/tmp
mkdir -p "$WORK" "$TMPDIR"
export TMPDIR
trap 'rm -rf "$MEASURE_TMP"' EXIT HUP INT TERM
cd "$WORK"

echo 'fn main() i32 { return 0 }' >empty.fas

cat >work.fas <<'FAS'
use "C" <<WORK
#include <stdio.h>
int work_report(long total) {
  return printf("total %ld\n", total) > 0 ? 0 : 1;
}
WORK

fn main() i32 {
  total i64 = 0
  i i64 = 0
  while i < 20000 {
    if i % 3 == 0 {
      total = total + i
    } else {
      total = total - 1
    }
    i = i + 1
  }
  return work_report(total)
}
FAS

sed 's/total - 1/total - 2/' work.fas >other.fas

cat >pid.fas <<'FAS'
use "C" <<PID
#include <stdio.h>
#include <unistd.h>
int pid_report(void) {
  return printf("pid %ld\n", (long)getpid()) > 0 ? 0 : 1;
}
PID

fn main() i32 {
  return pid_report()
}
FAS

cat >env.fas <<'FAS'
use "C" <<ENV
#include <stdio.h>
#include <stdlib.h>
int env_report(void) {
  const char *preload = getenv("LD_PRELOAD");
  const char *offset = getenv("FAS_ARENA_OFFSET");
  return printf("preload %s offset %s\n", preload ? "set" : "unset", offset ? offset : "unset") > 0 ? 0 : 1;
}
ENV

fn main() i32 {
  return env_report()
}
FAS

cat >args.fas <<'FAS'
use "C" <<ARGS
#include <stdio.h>
#include <string.h>
#include <unistd.h>
int args_report(void) {
  char buf[4096];
  FILE *f = fopen("/proc/self/cmdline", "rb");
  size_t n = f ? fread(buf, 1, sizeof buf - 1, f) : 0;
  if (f) fclose(f);
  buf[n] = 0;
  size_t i = strlen(buf) + 1;
  while (i < n) {
    const char *a = buf + i;
    if (strcmp(a, "pid") == 0) printf("pid %ld\n", (long)getpid());
    else printf("arg %s\n", a);
    i += strlen(a) + 1;
  }
  return 0;
}
ARGS

fn main() i32 {
  return args_report()
}
FAS

sed 's/else printf("arg %s\\n", a);/else if (strcmp(a, "second") == 0) printf("changed\\n"); else printf("arg %s\\n", a);/' args.fas >args-other.fas
grep -q changed args-other.fas || fail "args-other.fas was not generated"

no_tune_temps() {
  leftovers=$(ls -A "$TMPDIR")
  [ -z "$leftovers" ] || fail "temporary files left behind in TMPDIR: $leftovers"
}

tune() {
  name=$1
  shift
  : >"$name.log"
  status=0
  FAS_TUNE_LOG="$WORK/$name.log" "$OCAML_FAS" perf --tune "$@" >"$name.out" 2>"$name.err" || status=$?
}

count() {
  grep -c -x -F -e "$2" "$1" || true
}

check_counts() {
  log=$1
  placements=$2
  shift 2
  for candidate in default pgo native native-pgo; do
    expected=8
    for confirmed in "$@"; do
      [ "$candidate" = "$confirmed" ] && expected=15
    done
    for placement in $placements; do
      total=0
      for phase in check select confirm; do
        total=$((total + $(count "$log" "$phase $candidate $placement")))
      done
      [ "$total" -eq "$expected" ] || fail "$log: $candidate $placement ran $total times, expected $expected"
    done
  done
}

check_rotation() {
  log=$1
  phase=$2
  grep "^$phase " "$log" >"$log.$phase"
  n=$3
  [ "$(wc -l <"$log.$phase")" -eq $((n * 7)) ] || fail "$log: $(wc -l <"$log.$phase") $phase runs, expected $((n * 7))"
  awk -v n="$n" '
    { line[NR - 1] = $0 }
    END {
      for (r = 0; r < 7; r++)
        for (k = 0; k < n; k++)
          if (line[r * n + k] != line[(r + k) % n]) { print "round " r " position " k ": " line[r * n + k]; exit 1 }
    }' "$log.$phase" || fail "$log: $phase rounds are not rotated"
}

ARENA_PLACEMENTS='def+0 def+65536 def+589824 32+0 32+65536 32+589824 64+0 64+65536 64+589824'


FAS_TUNE_TEST_SCALE=pgo=0.01 tune keep work.fas -o keep.fasprof
[ "$status" -eq 0 ] || { cat keep.err >&2; fail "keep run exited $status"; }
[ "$(sed -n '5p' keep.fasprof)" = 'choice pgo' ] || fail "keep run kept '$(sed -n '5p' keep.fasprof)'"
[ "$(count keep.log 'train portable')" -eq 1 ] || fail "keep: portable training ran $(count keep.log 'train portable') times"
[ "$(count keep.log 'train native')" -eq 1 ] || fail "keep: native training ran $(count keep.log 'train native') times"
[ "$(count keep.log 'reference default')" -eq 2 ] || fail "keep: the reference ran $(count keep.log 'reference default') times"
[ "$(head -n 4 keep.log | tr '\n' ' ')" = 'train portable train native reference default reference default ' ] || fail "keep: the log starts with $(head -n 4 keep.log)"
check_counts keep.log "$ARENA_PLACEMENTS" default pgo
check_rotation keep.log select 36
check_rotation keep.log confirm 18
grep -q '^confirm \(native\|native-pgo\) ' keep.log && fail "keep: confirmation measured another candidate"
grep -E -q '^placement +default +pgo +ratio$' keep.err || fail "keep: no report table: $(cat keep.err)"
[ "$(grep -c -E '^(def|32|64)\+[0-9]+ +[0-9]+ +[0-9]+ +[0-9]+\.[0-9]{3}$' keep.err)" -eq 9 ] || fail "keep: the table has no 9 placement rows: $(cat keep.err)"
grep -E -q '^fas perf: measured on CPU [0-9]+, 7 rounds$' keep.err || fail "keep: no CPU line: $(cat keep.err)"
grep -E -q '^fas perf: kept the profile \(pgo [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%(, loops aligned to (32|64))?\) \(measured on one input\)$' keep.err || fail "keep: no kept line: $(cat keep.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100 tune reject work.fas -o reject.fasprof
[ "$status" -eq 0 ] || { cat reject.err >&2; fail "reject run exited $status"; }
[ "$(sed -n '5p' reject.fasprof)" = 'choice default' ] || fail "reject run kept '$(sed -n '5p' reject.fasprof)'"
check_counts reject.log "$ARENA_PLACEMENTS"
grep -q '^confirm ' reject.log && fail "reject: a confirmation ran without a keep"
grep -E -q '^fas perf: kept the default build \(best (pgo|native|native-pgo) 0\.[0-9]{3}×, band [0-9]+\.[0-9]%\) \(measured on one input\)$' reject.err || fail "reject: no kept line: $(cat reject.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=0.01 tune noarena --no-arena env.fas -o noarena.fasprof
[ "$status" -eq 0 ] || { cat noarena.err >&2; fail "--no-arena run exited $status"; }
check_counts noarena.log 'def 32 64' default pgo
check_rotation noarena.log select 12
grep -q '+' noarena.log && fail "--no-arena used an arena offset"
grep -E -q '^measured [0-9.]+ band [0-9.]+ rounds 7 placements 3 metric (cycles|wall)$' noarena.fasprof || fail "--no-arena profile: $(sed -n '7p' noarena.fasprof)"
no_tune_temps

tune arena env.fas -o arena.fasprof
[ "$status" -eq 1 ] || fail "arena run exited $status, expected 1"
[ "$(cat arena.err)" = 'error: the program behaves differently under the measuring allocator (arena offset 0); rerun with --no-arena' ] || fail "arena run stderr: $(cat arena.err)"
[ ! -e arena.fasprof ] || fail "arena run wrote a profile"
no_tune_temps

tune pid pid.fas -o pid.fasprof
[ "$status" -eq 0 ] || { cat pid.err >&2; fail "pid run exited $status"; }
[ "$(head -n 1 pid.err)" = "fas perf: the program's output differs between two runs of the same build; the profile keeps the default build" ] || fail "pid run stderr: $(cat pid.err)"
[ "$(sed -n '5p;6p' pid.fasprof)" = "$(printf 'choice default\nalign-loops def')" ] || fail "pid profile: $(head -n 7 pid.fasprof)"
grep -q '^select \|^check ' pid.log && fail "pid run measured after the reference check"
"$OCAML_FAS" --profile pid.fasprof -c pid.fas -o pid-profile.o
"$OCAML_FAS" -c pid.fas -o pid-plain.o
cmp pid-profile.o pid-plain.o || fail "the pid profile does not build the default"
no_tune_temps

FAS_TUNE_TEST_CANDIDATE="$WORK/other.fas" tune changed work.fas -o changed.fasprof
[ "$status" -eq 1 ] || fail "changed run exited $status, expected 1"
case "$(cat changed.err)" in
  "internal error: a tuned build changed the program's output (pgo, arena offset 0); kept builds in $TMPDIR/fas-tune-"*/builds) ;;
  *) fail "changed run stderr: $(cat changed.err)" ;;
esac
[ ! -e changed.fasprof ] || fail "changed run wrote a profile"
kept=$(sed -n 's/.*; kept builds in //p' changed.err)
[ -x "$kept/pgo-def" ] && [ -x "$kept/default-def" ] || fail "changed run did not keep the builds in $kept"
grep -q '^select ' changed.log && fail "changed run measured after a mismatch"
rm -rf "${kept%/builds}"
no_tune_temps

FAS_TUNE_TEST_CANDIDATE="$WORK/other.fas" tune changed-noarena --no-arena work.fas -o changed-noarena.fasprof
[ "$status" -eq 1 ] || fail "changed --no-arena run exited $status, expected 1"
case "$(cat changed-noarena.err)" in
  "internal error: a tuned build changed the program's output (pgo, no arena); kept builds in "*) ;;
  *) fail "changed --no-arena stderr: $(cat changed-noarena.err)" ;;
esac
kept=$(sed -n 's/.*; kept builds in //p' changed-noarena.err)
rm -rf "${kept%/builds}"
no_tune_temps

FAS_TUNE_TEST_CANDIDATE="$WORK/work.fas" FAS_TUNE_TEST_SCALE= tune same work.fas -o same.fasprof
[ "$status" -eq 0 ] || { cat same.err >&2; fail "same-source run exited $status"; }
no_tune_temps

check_second() {
  log=$1
  shift
  for candidate in default pgo native native-pgo; do
    expected=1
    for confirmed in "$@"; do
      [ "$candidate" = "$confirmed" ] && expected=8
    done
    for placement in $ARENA_PLACEMENTS; do
      total=$(($(count "$log" "check-with $candidate $placement") + $(count "$log" "confirm-with $candidate $placement")))
      [ "$total" -eq "$expected" ] || fail "$log: second input $candidate $placement ran $total times, expected $expected"
    done
  done
}

FAS_TUNE_TEST_SCALE=pgo=0.01 tune held args.fas -o held.fasprof -- first --check-with -- second
[ "$status" -eq 0 ] || { cat held.err >&2; fail "held run exited $status"; }
[ "$(sed -n '5p' held.fasprof)" = 'choice pgo' ] || fail "held run kept '$(sed -n '5p' held.fasprof)'"
[ "$(head -n 6 held.log | tr '\n' ' ')" = 'train portable train native reference default reference default reference-with default reference-with default ' ] || fail "held: the log starts with $(head -n 6 held.log)"
check_counts held.log "$ARENA_PLACEMENTS" default pgo
check_second held.log default pgo
check_rotation held.log confirm-with 18
grep -q '^select .*' held.log || fail "held: no selection"
[ "$(grep -c '^confirm-with ' held.log)" -eq 126 ] || fail "held: $(grep -c '^confirm-with ' held.log) second-input confirmation runs"
grep -E -q '^fas perf: kept the profile \(pgo [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%(, loops aligned to (32|64))?; second input [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\)$' held.err || fail "held: no kept line: $(cat held.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=0.01,pgo@with=100 tune heldlost args.fas -o heldlost.fasprof -- first --check-with -- second
[ "$status" -eq 0 ] || { cat heldlost.err >&2; fail "held-lost run exited $status"; }
[ "$(sed -n '5p' heldlost.fasprof)" = 'choice default' ] || fail "held-lost run kept '$(sed -n '5p' heldlost.fasprof)'"
grep -E -q '^fas perf: kept the default build \(best pgo [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%; second input 0\.0[0-9]{2}×, band [0-9]+\.[0-9]%, slower at some placement\)$' heldlost.err || fail "held-lost: no kept line: $(cat heldlost.err)"
check_second heldlost.log default pgo
no_tune_temps

tune heldpid args.fas -o heldpid.fasprof -- first --check-with -- pid
[ "$status" -eq 0 ] || { cat heldpid.err >&2; fail "held pid run exited $status"; }
[ "$(head -n 1 heldpid.err)" = "fas perf: the program's output differs between two runs of the same build on the second input; the profile keeps the default build" ] || fail "held pid stderr: $(cat heldpid.err)"
[ "$(sed -n '5p' heldpid.fasprof)" = 'choice default' ] || fail "held pid profile: $(sed -n '5p' heldpid.fasprof)"
grep -q '^check' heldpid.log && fail "held pid run checked after the reference"
no_tune_temps

FAS_TUNE_TEST_CANDIDATE="$WORK/args-other.fas" tune heldchanged args.fas -o heldchanged.fasprof -- first --check-with -- second
[ "$status" -eq 1 ] || fail "held changed run exited $status, expected 1"
case "$(cat heldchanged.err)" in
  "internal error: a tuned build changed the program's output (pgo, arena offset 0, second input); kept builds in $TMPDIR/fas-tune-"*/builds) ;;
  *) fail "held changed stderr: $(cat heldchanged.err)" ;;
esac
[ "$(count heldchanged.log 'check pgo def+0')" -eq 1 ] || fail "held changed: the first input was not checked first"
[ ! -e heldchanged.fasprof ] || fail "held changed run wrote a profile"
kept=$(sed -n 's/.*; kept builds in //p' heldchanged.err)
rm -rf "${kept%/builds}"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=0.01 tune heldfirst args.fas -o heldfirst.fasprof --check-with -- second
[ "$status" -eq 0 ] || { cat heldfirst.err >&2; fail "held-first run exited $status"; }
check_second heldfirst.log default pgo
no_tune_temps

echo "tune_measure: passed"
