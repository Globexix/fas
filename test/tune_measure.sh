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

check_totals() {
  log=$1
  placements=$2
  steps=$3
  shift 3
  expected_names=$(for item in "$@"; do printf '%s\n' "${item%=*}"; done | sort -u)
  logged_names=$(for step in $steps; do grep "^$step " "$log" || true; done | cut -d ' ' -f 2 | sort -u)
  [ "$logged_names" = "$expected_names" ] || fail "$log: $steps ran candidates $(echo $logged_names), expected $(echo $expected_names)"
  for item in "$@"; do
    name=${item%=*}
    expected=${item#*=}
    for placement in $placements; do
      total=0
      for step in $steps; do
        total=$((total + $(count "$log" "$step $name $placement")))
      done
      [ "$total" -eq "$expected" ] || fail "$log: $name $placement ran $total times, expected $expected"
    done
  done
}

check_rotation() {
  log=$1
  step=$2
  shift 2
  grep "^$step " "$log" >"$log.$step"
  total=0
  for n in "$@"; do total=$((total + n * 7)); done
  [ "$(wc -l <"$log.$step")" -eq "$total" ] || fail "$log: $(wc -l <"$log.$step") $step runs, expected $total"
  start=0
  for n in "$@"; do
    tail -n +$((start + 1)) "$log.$step" | head -n $((n * 7)) >"$log.$step.stage"
    awk -v n="$n" '
      { line[NR - 1] = $0 }
      END {
        for (r = 0; r < 7; r++)
          for (k = 0; k < n; k++)
            if (line[r * n + k] != line[(r + k) % n]) { print "round " r " position " k ": " line[r * n + k]; exit 1 }
      }' "$log.$step.stage" || fail "$log: $step rounds are not rotated"
    start=$((start + n * 7))
  done
}

ARENA_PLACEMENTS='def+0 def+65536 def+589824 32+0 32+65536 32+589824 64+0 64+65536 64+589824'


FAS_TUNE_TEST_SCALE=pgo=0.01 tune keep work.fas -o keep.fasprof
[ "$status" -eq 0 ] || { cat keep.err >&2; fail "keep run exited $status"; }
[ "$(sed -n '5p' keep.fasprof)" = 'choice pgo' ] || fail "keep run kept '$(sed -n '5p' keep.fasprof)'"
[ "$(count keep.log 'train portable')" -eq 1 ] || fail "keep: portable training ran $(count keep.log 'train portable') times"
[ "$(count keep.log 'train native')" -eq 1 ] || fail "keep: native training ran $(count keep.log 'train native') times"
[ "$(count keep.log 'reference default')" -eq 2 ] || fail "keep: the reference ran $(count keep.log 'reference default') times"
[ "$(head -n 4 keep.log | tr '\n' ' ')" = 'train portable train native reference default reference default ' ] || fail "keep: the log starts with $(head -n 4 keep.log)"
check_totals keep.log "$ARENA_PLACEMENTS" 'check select confirm' default=15 pgo=22 native=8 native-pgo=8 pgo+unroll=8 pgo+O3=8
check_rotation keep.log select 36 27
check_rotation keep.log confirm 18
grep -q '^confirm \(native\|native-pgo\) ' keep.log && fail "keep: confirmation measured another candidate"
grep -E -q '^placement +default +pgo +ratio$' keep.err || fail "keep: no report table: $(cat keep.err)"
[ "$(grep -c -E '^(def|32|64)\+[0-9]+ +[0-9]+ +[0-9]+ +[0-9]+\.[0-9]{3}$' keep.err)" -eq 18 ] || fail "keep: the tables have no 18 placement rows: $(cat keep.err)"
grep -E -q '^fas perf: options: [a-zA-Z0-9 ]+ did not win \(best [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\)$' keep.err || fail "keep: no options line: $(cat keep.err)"
grep -E -q '^fas perf: measured on CPU [0-9]+, 7 rounds$' keep.err || fail "keep: no CPU line: $(cat keep.err)"
grep -E -q '^fas perf: kept the profile \(pgo [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%(, loops aligned to (32|64))?\) \(measured on one input\)$' keep.err || fail "keep: no kept line: $(cat keep.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100,default+unroll=100,default+O3=100 tune reject work.fas -o reject.fasprof
[ "$status" -eq 0 ] || { cat reject.err >&2; fail "reject run exited $status"; }
[ "$(sed -n '5p' reject.fasprof)" = 'choice default' ] || fail "reject run kept '$(sed -n '5p' reject.fasprof)'"
check_totals reject.log "$ARENA_PLACEMENTS" 'check select confirm' default=15 pgo=8 native=8 native-pgo=8 default+unroll=8 default+O3=8
check_rotation reject.log select 36 27
grep -q '^confirm ' reject.log && fail "reject: a confirmation ran without a keep"
grep -E -q '^fas perf: options: [a-zA-Z0-9 ]+ did not win ' reject.err || fail "reject: no options line: $(cat reject.err)"
grep -E -q '^fas perf: kept the default build \(best (pgo|native|native-pgo) 0\.[0-9]{3}×, band [0-9]+\.[0-9]%\) \(measured on one input\)$' reject.err || fail "reject: no kept line: $(cat reject.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=0.01 tune noarena --no-arena env.fas -o noarena.fasprof
[ "$status" -eq 0 ] || { cat noarena.err >&2; fail "--no-arena run exited $status"; }
check_totals noarena.log 'def 32 64' 'check select confirm' default=15 pgo=22 native=8 native-pgo=8 pgo+unroll=8 pgo+O3=8
check_rotation noarena.log select 12 9
grep -q -E '\+[0-9]+$' noarena.log && fail "--no-arena used an arena offset"
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

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100,default+O3=0.01 tune options-kept work.fas -o options-kept.fasprof
[ "$status" -eq 0 ] || { cat options-kept.err >&2; fail "options-kept run exited $status"; }
[ "$(sed -n '5p;8p' options-kept.fasprof)" = "$(printf 'choice default\noptions O3')" ] || fail "options-kept profile: $(head -n 8 options-kept.fasprof)"
check_totals options-kept.log "$ARENA_PLACEMENTS" 'check select confirm' default=22 pgo=8 native=8 native-pgo=8 default+unroll=8 default+O3=22 default+unroll+O3=8
check_rotation options-kept.log select 36 27 18
check_rotation options-kept.log confirm 18
grep -E -q '^placement +default +default\+O3 +ratio$' options-kept.err || fail "options-kept: no option table: $(cat options-kept.err)"
grep -E -q '^fas perf: options: kept O3 \([0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\)$' options-kept.err || fail "options-kept: no kept options line: $(cat options-kept.err)"
grep -E -q '^fas perf: options: unroll O3 did not win \(best [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\)$' options-kept.err || fail "options-kept: no union line: $(cat options-kept.err)"
grep -E -q '^fas perf: kept the profile \(default [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%(, loops aligned to (32|64))?, options O3\) \(measured on one input\)$' options-kept.err || fail "options-kept: no kept line: $(cat options-kept.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100,default+O3=0.1,default+unroll+O3=0.01 tune options-both work.fas -o options-both.fasprof
[ "$status" -eq 0 ] || { cat options-both.err >&2; fail "options-both run exited $status"; }
[ "$(sed -n '5p;8p' options-both.fasprof)" = "$(printf 'choice default\noptions unroll O3')" ] || fail "options-both profile: $(head -n 8 options-both.fasprof)"
check_totals options-both.log "$ARENA_PLACEMENTS" 'check select confirm' default=22 pgo=8 native=8 native-pgo=8 default+unroll=8 default+O3=29 default+unroll+O3=15
check_rotation options-both.log select 36 27 18
check_rotation options-both.log confirm 18 18
grep -E -q '^fas perf: options: kept O3 \([0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\)$' options-both.err || fail "options-both: no first option line: $(cat options-both.err)"
grep -E -q '^fas perf: options: kept unroll O3 \([0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\)$' options-both.err || fail "options-both: no combined option line: $(cat options-both.err)"
grep -E -q '^fas perf: kept the profile \(default [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%(, loops aligned to (32|64))?, options unroll O3\) \(measured on one input\)$' options-both.err || fail "options-both: no kept line: $(cat options-both.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100 FAS_TUNE_TEST_OPTION_CANDIDATE="$WORK/other.fas" tune options-changed work.fas -o options-changed.fasprof
[ "$status" -eq 1 ] || fail "options-changed run exited $status, expected 1"
case "$(cat options-changed.err)" in
  "internal error: a tuned build changed the program's output (default+unroll, arena offset 0); kept builds in $TMPDIR/fas-tune-"*/builds) ;;
  *) fail "options-changed stderr: $(cat options-changed.err)" ;;
esac
[ ! -e options-changed.fasprof ] || fail "options-changed run wrote a profile"
[ "$(count options-changed.log 'check default+unroll def+0')" -eq 1 ] || fail "options-changed: the option candidate was not checked first"
grep -q -E '^(select|confirm) default\+' options-changed.log && fail "options-changed run measured an option candidate after a mismatch"
kept=$(sed -n 's/.*; kept builds in //p' options-changed.err)
[ -x "$kept/default+unroll-def" ] || fail "options-changed run did not keep the builds in $kept"
rm -rf "${kept%/builds}"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=0.01 tune held args.fas -o held.fasprof -- first --check-with -- second
[ "$status" -eq 0 ] || { cat held.err >&2; fail "held run exited $status"; }
[ "$(sed -n '5p' held.fasprof)" = 'choice pgo' ] || fail "held run kept '$(sed -n '5p' held.fasprof)'"
[ "$(head -n 6 held.log | tr '\n' ' ')" = 'train portable train native reference default reference default reference-with default reference-with default ' ] || fail "held: the log starts with $(head -n 6 held.log)"
check_totals held.log "$ARENA_PLACEMENTS" 'check select confirm' default=15 pgo=22 native=8 native-pgo=8 pgo+unroll=8 pgo+O3=8
check_totals held.log "$ARENA_PLACEMENTS" 'check-with confirm-with' default=8 pgo=8 native=1 native-pgo=1 pgo+unroll=1 pgo+O3=1
check_rotation held.log confirm-with 18
grep -q '^select .*' held.log || fail "held: no selection"
[ "$(grep -c '^confirm-with ' held.log)" -eq 126 ] || fail "held: $(grep -c '^confirm-with ' held.log) second-input confirmation runs"
grep -E -q '^fas perf: kept the profile \(pgo [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%(, loops aligned to (32|64))?; second input [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\)$' held.err || fail "held: no kept line: $(cat held.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=0.01,pgo@with=100,default+unroll=100,default+O3=100 tune heldlost args.fas -o heldlost.fasprof -- first --check-with -- second
[ "$status" -eq 0 ] || { cat heldlost.err >&2; fail "held-lost run exited $status"; }
[ "$(sed -n '5p' heldlost.fasprof)" = 'choice default' ] || fail "held-lost run kept '$(sed -n '5p' heldlost.fasprof)'"
grep -E -q '^fas perf: kept the default build \(best pgo [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%; second input 0\.0[0-9]{2}×, band [0-9]+\.[0-9]%, slower at some placement\)$' heldlost.err || fail "held-lost: no kept line: $(cat heldlost.err)"
check_totals heldlost.log "$ARENA_PLACEMENTS" 'check-with confirm-with' default=8 pgo=8 native=1 native-pgo=1 default+unroll=1 default+O3=1
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
check_totals heldfirst.log "$ARENA_PLACEMENTS" 'check-with confirm-with' default=8 pgo=8 native=1 native-pgo=1 pgo+unroll=1 pgo+O3=1
no_tune_temps

echo "tune_measure: passed"
