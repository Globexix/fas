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
    expected=${item##*=}
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
LAYOUT_PLACEMENTS='def+0 def+65536 def+589824 32+0 32+65536 32+589824 64+0 64+65536 64+589824 def+f64+0 def+f64+65536 def+f64+589824 32+f64+0 32+f64+65536 32+f64+589824 64+f64+0 64+f64+65536 64+f64+589824'
NOARENA_LAYOUT='def def+f64 32 32+f64 64 64+f64'


FAS_TUNE_TEST_SCALE=pgo=0.01 tune keep work.fas -o keep.fasprof
[ "$status" -eq 0 ] || { cat keep.err >&2; fail "keep run exited $status"; }
[ "$(sed -n '5p' keep.fasprof)" = 'choice pgo' ] || fail "keep run kept '$(sed -n '5p' keep.fasprof)'"
[ "$(count keep.log 'train portable')" -eq 1 ] || fail "keep: portable training ran $(count keep.log 'train portable') times"
[ "$(count keep.log 'train native')" -eq 1 ] || fail "keep: native training ran $(count keep.log 'train native') times"
[ "$(count keep.log 'reference default')" -eq 2 ] || fail "keep: the reference ran $(count keep.log 'reference default') times"
[ "$(head -n 4 keep.log | tr '\n' ' ')" = 'train portable train native reference default reference default ' ] || fail "keep: the log starts with $(head -n 4 keep.log)"
check_totals keep.log "$ARENA_PLACEMENTS" 'check select confirm' default=15 pgo=22 native=8 native-pgo=8 pgo+interleave=4=8
check_rotation keep.log select 36 18
check_rotation keep.log confirm 18
check_totals keep.log "$LAYOUT_PLACEMENTS" layout pgo=7
grep -q '^confirm \(native\|native-pgo\) ' keep.log && fail "keep: confirmation measured another candidate"
grep -E -q '^placement +default +pgo +ratio$' keep.err || fail "keep: no report table: $(cat keep.err)"
grep -E -q '^placement +pgo +pgo\+interleave=4 +ratio$' keep.err || fail "keep: no option table: $(cat keep.err)"
[ "$(grep -c -E '^(def|32|64)\+[0-9]+ +[0-9]+ +[0-9]+ +[0-9]+\.[0-9]{3}$' keep.err)" -eq 18 ] || fail "keep: the tables have no 18 placement rows: $(cat keep.err)"
grep -E -q '^fas perf: measured on CPU [0-9]+, 7 rounds$' keep.err || fail "keep: no CPU line: $(cat keep.err)"
grep -E -q '^fas perf: kept the profile \(pgo [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%(, loops aligned to (32|64))?(, functions aligned to 64)?\) \(measured on one input\)$' keep.err || fail "keep: no kept line: $(cat keep.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=0.01,pgo-def-f64=0.1 tune layout-kept work.fas -o layout-kept.fasprof
[ "$status" -eq 0 ] || { cat layout-kept.err >&2; fail "layout-kept run exited $status"; }
[ "$(sed -n '5,7p' layout-kept.fasprof)" = "$(printf 'choice pgo\nalign-loops def\nalign-functions 64')" ] || fail "layout-kept profile: $(head -n 8 layout-kept.fasprof)"
check_totals layout-kept.log "$LAYOUT_PLACEMENTS" layout pgo=7
grep -E -q '^fas perf: kept the profile \(pgo [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%, functions aligned to 64\) \(measured on one input\)$' layout-kept.err || fail "layout-kept: no kept line: $(cat layout-kept.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=0.01,pgo-32=2,pgo-64=2,pgo-def-f64=2,pgo-32-f64=2,pgo-64-f64=2 tune layout-none work.fas -o layout-none.fasprof
[ "$status" -eq 0 ] || { cat layout-none.err >&2; fail "layout-none run exited $status"; }
[ "$(sed -n '5,7p' layout-none.fasprof)" = "$(printf 'choice pgo\nalign-loops def\nalign-functions def')" ] || fail "layout-none profile: $(head -n 8 layout-none.fasprof)"
check_totals layout-none.log "$LAYOUT_PLACEMENTS" layout pgo=7
grep -E -q '^fas perf: kept the profile \(pgo [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\) \(measured on one input\)$' layout-none.err || fail "layout-none: no kept line: $(cat layout-none.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=0.01,pgo-64=0.1,pgo-64-f64=0.1 tune layout-fallback work.fas -o layout-fallback.fasprof
[ "$status" -eq 0 ] || { cat layout-fallback.err >&2; fail "layout-fallback run exited $status"; }
[ "$(sed -n '5,7p' layout-fallback.fasprof)" = "$(printf 'choice pgo\nalign-loops 64\nalign-functions def')" ] || fail "layout-fallback profile: $(head -n 8 layout-fallback.fasprof)"
check_totals layout-fallback.log "$LAYOUT_PLACEMENTS" layout pgo=7
grep -E -q '^fas perf: kept the profile \(pgo [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%, loops aligned to 64\) \(measured on one input\)$' layout-fallback.err || fail "layout-fallback: no kept line: $(cat layout-fallback.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100,default-32=2,default-64=2,default-def-f64=2,default-32-f64=2,default-64-f64=2,default+interleave=4=100 tune reject work.fas -o reject.fasprof
[ "$status" -eq 0 ] || { cat reject.err >&2; fail "reject run exited $status"; }
[ "$(sed -n '5p' reject.fasprof)" = 'choice default' ] || fail "reject run kept '$(sed -n '5p' reject.fasprof)'"
check_totals reject.log "$ARENA_PLACEMENTS" 'check select' default=15 pgo=8 native=8 native-pgo=8 default+interleave=4=8
check_rotation reject.log select 36 18
grep -q '^confirm ' reject.log && fail "reject: a confirmation ran without a keep"
grep -E -q '^fas perf: kept the default build \(best (pgo|native|native-pgo) 0\.[0-9]{3}×, band [0-9]+\.[0-9]%\) \(measured on one input\)$' reject.err || fail "reject: no kept line: $(cat reject.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=0.01 tune noarena --no-arena env.fas -o noarena.fasprof
[ "$status" -eq 0 ] || { cat noarena.err >&2; fail "--no-arena run exited $status"; }
check_totals noarena.log 'def 32 64' 'check select confirm' default=15 pgo=22 native=8 native-pgo=8 pgo+O3=8
check_rotation noarena.log select 12 6
check_totals noarena.log "$NOARENA_LAYOUT" layout pgo=7
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

cat >vec.fas <<'FAS'
use "C" <stdio.h>

var buf arr[256, u32] = {}

fn fill(n usize) i32 {
    for i usize = 0; i < n; i += 1 {
        buf[i] = trunc[u32](i) * 2654435761 + 12345
    }
    return 0
}

fn total(n usize) u64 {
    sum u64 = 0
    for i usize = 0; i < n; i += 1 {
        sum += zext[u64](buf[i])
    }
    return sum
}

fn permute_lanes() i32 {
    raw vec[16,u8] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
    pidx vec[16,u8] = {15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0}
    moved vec[16,u8] = permute(raw, pidx)
    for k usize = 0; k < 16; k += 1 {
        lane u32 = zext[u32](moved[k])
        printed i32 = printf(c"%u\n", lane)
        if printed <= 0 { return 1 }
    }
    return 0
}

fn main() i32 {
    if fill(256) != 0 { return 3 }
    printed i32 = printf(c"sum %lu\n", total(256))
    if printed <= 0 { return 1 }
    if permute_lanes() != 0 { return 2 }
    return 0
}
FAS
sed 's/sum %lu/sum changed %lu/' vec.fas >vec-other.fas
grep -q 'sum changed' vec-other.fas || fail "vec-other.fas was not generated"
cat >identical.fas <<'FAS'
use "C" <<WORK
#include <stdio.h>
unsigned long seed_value(void) { return 88172645463325252UL; }
int work_report(unsigned long total) {
  return printf("total %lu\n", total) > 0 ? 0 : 1;
}
WORK

fn main() i32 {
  total u64 = 0
  state u64 = seed_value()
  i1 i64 = 0
  while i1 < 8 {
    state ^= state << 13
    state ^= state >> 7
    state ^= state << 17
    state ^= state << 13
    state ^= state >> 7
    state ^= state << 17
    state ^= state << 13
    state ^= state >> 7
    state ^= state << 17
    state ^= state << 13
    state ^= state >> 7
    state ^= state << 17
    state ^= state << 13
    state ^= state >> 7
    state ^= state << 17
    state ^= state << 13
    state ^= state >> 7
    state ^= state << 17
    state ^= state << 13
    state ^= state >> 7
    state ^= state << 17
    state ^= state << 13
    state ^= state >> 7
    state ^= state << 17
    total = total + state
    i1 = i1 + 1
  }
  return work_report(total)
}
FAS

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100,default+unroll=1200=0.01,default+interleave=4=100,default+max-bandwidth=100,default+inline=1000=100 tune options-unroll vec.fas -o options-unroll.fasprof
[ "$status" -eq 0 ] || { cat options-unroll.err >&2; fail "options-unroll run exited $status"; }
[ "$(sed -n '5p;8p' options-unroll.fasprof)" = "$(printf 'choice default\noptions unroll=1200')" ] || fail "options-unroll profile: $(head -n 8 options-unroll.fasprof)"
check_totals options-unroll.log "$ARENA_PLACEMENTS" 'check select confirm' default=22 pgo=8 native=8 native-pgo=8 default+unroll=600=8 default+unroll=1200=22 default+unroll=2400=8 default+O3=8 default+unroll=1200+O3=8 default+interleave=4=8 default+max-bandwidth=8
check_rotation options-unroll.log select 36 63 18
check_rotation options-unroll.log confirm 18
grep -E -q '^placement +default +default\+unroll=1200 +ratio$' options-unroll.err || fail "options-unroll: no option table: $(cat options-unroll.err)"
grep -q -x -F 'fas perf: options: default+unroll=4800 builds the same binary as default+unroll=2400; not measured' options-unroll.err || fail "options-unroll: no unroll=4800 line: $(cat options-unroll.err)"
grep -E -q '^fas perf: kept options unroll=1200 \([0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\) \(measured on one input\)$' options-unroll.err || fail "options-unroll: no kept options line: $(cat options-unroll.err)"
grep -E -q '^fas perf: kept no more options \(best O3 [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\) \(measured on one input\)$' options-unroll.err || fail "options-unroll: no second option line: $(cat options-unroll.err)"
grep -E -q '^fas perf: kept the profile \(default [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%(, loops aligned to (32|64))?(, functions aligned to 64)?, options unroll=1200\) \(measured on one input\)$' options-unroll.err || fail "options-unroll: no kept line: $(cat options-unroll.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100,default+O3=0.01,default+unroll=2400=0.5,default+unroll=2400+O3=0.001,default+interleave=4=100,default+max-bandwidth=100,default+inline=1000=100 tune options-o3 vec.fas -o options-o3.fasprof
[ "$status" -eq 0 ] || { cat options-o3.err >&2; fail "options-o3 run exited $status"; }
[ "$(sed -n '5p;8p' options-o3.fasprof)" = "$(printf 'choice default\noptions unroll=2400 O3')" ] || fail "options-o3 profile: $(head -n 8 options-o3.fasprof)"
check_totals options-o3.log "$ARENA_PLACEMENTS" 'check select confirm' default=22 pgo=8 native=8 native-pgo=8 default+unroll=600=8 default+unroll=1200=8 default+unroll=2400=8 default+O3=29 default+unroll=2400+O3=15 default+interleave=4=8 default+max-bandwidth=8 default+O3+interleave=4=8 default+O3+max-bandwidth=8
check_rotation options-o3.log select 36 63 36
check_rotation options-o3.log confirm 18 18
grep -E -q '^fas perf: kept options O3 \([0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\) \(measured on one input\)$' options-o3.err || fail "options-o3: no first option line: $(cat options-o3.err)"
grep -E -q '^fas perf: kept options unroll=2400 O3 \([0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\) \(measured on one input\)$' options-o3.err || fail "options-o3: no second option line: $(cat options-o3.err)"
grep -E -q '^fas perf: kept the profile \(default [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%(, loops aligned to (32|64))?(, functions aligned to 64)?, options unroll=2400 O3\) \(measured on one input\)$' options-o3.err || fail "options-o3: no kept line: $(cat options-o3.err)"
no_tune_temps

cat >fixed-loop.fas <<'FAS'
use "C" <stdio.h>

fn churn(seed u64) u64 {
    acc u64 = seed
    for i usize = 0; i < 512; i += 1 {
        acc = acc * 6364136223846793005 + i
        acc = acc ^ (acc >> 29)
        acc = acc * 1442695040888963407 + 1
        acc = acc ^ (acc << 7)
    }
    return acc
}

fn main() i32 {
    printed i32 = printf(c"%lu\n", churn(300))
    if printed <= 0 { return 1 }
    return 0
}
FAS

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100,default+unroll=4800=0.01,default+interleave=4=100,default+max-bandwidth=100,default+inline=1000=100 tune options-fixed-loop fixed-loop.fas -o options-fixed-loop.fasprof
[ "$status" -eq 0 ] || { cat options-fixed-loop.err >&2; fail "options-fixed-loop run exited $status"; }
[ "$(sed -n '5p;8p' options-fixed-loop.fasprof)" = "$(printf 'choice default\noptions unroll=4800')" ] || fail "options-fixed-loop profile: $(head -n 8 options-fixed-loop.fasprof)"
check_totals options-fixed-loop.log "$ARENA_PLACEMENTS" 'check select confirm' default=22 pgo=8 native=8 native-pgo=8 default+unroll=4800=15
check_rotation options-fixed-loop.log select 36 18
check_rotation options-fixed-loop.log confirm 18
grep -q -x -F 'fas perf: options: default+unroll=600 builds the same binary as default; not measured' options-fixed-loop.err || fail "options-fixed-loop: no unroll=600 line: $(cat options-fixed-loop.err)"
grep -q -x -F 'fas perf: options: default+unroll=1200 builds the same binary as default; not measured' options-fixed-loop.err || fail "options-fixed-loop: no unroll=1200 line: $(cat options-fixed-loop.err)"
grep -q -x -F 'fas perf: options: default+unroll=2400 builds the same binary as default; not measured' options-fixed-loop.err || fail "options-fixed-loop: no unroll=2400 line: $(cat options-fixed-loop.err)"
grep -q -x -F 'fas perf: options: default+O3 builds the same binary as default; not measured' options-fixed-loop.err || fail "options-fixed-loop: no O3 line: $(cat options-fixed-loop.err)"
grep -q -x -F 'fas perf: options: default+unroll=4800+O3 builds the same binary as default+unroll=4800; not measured' options-fixed-loop.err || fail "options-fixed-loop: no unroll=4800+O3 line: $(cat options-fixed-loop.err)"
grep -E -q '^fas perf: kept options unroll=4800 \([0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\) \(measured on one input\)$' options-fixed-loop.err || fail "options-fixed-loop: no kept options line: $(cat options-fixed-loop.err)"
grep -E -q '^fas perf: kept the profile \(default [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%(, loops aligned to (32|64))?(, functions aligned to 64)?, options unroll=4800\) \(measured on one input\)$' options-fixed-loop.err || fail "options-fixed-loop: no kept line: $(cat options-fixed-loop.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100 FAS_TUNE_TEST_OPTION_CANDIDATE="$WORK/vec-other.fas" tune options-changed vec.fas -o options-changed.fasprof
[ "$status" -eq 1 ] || fail "options-changed run exited $status, expected 1"
case "$(cat options-changed.err)" in
  "fas perf: options: default+unroll=4800 builds the same binary as default+unroll=2400; not measured
internal error: a tuned build changed the program's output (default+unroll=600, arena offset 0); kept builds in $TMPDIR/fas-tune-"*/builds) ;;
  *) fail "options-changed stderr: $(cat options-changed.err)" ;;
esac
[ ! -e options-changed.fasprof ] || fail "options-changed run wrote a profile"
[ "$(count options-changed.log 'check default+unroll=600 def+0')" -eq 1 ] || fail "options-changed: the option candidate was not checked first"
grep -q -E '^(select|confirm) default\+' options-changed.log && fail "options-changed run measured an option candidate after a mismatch"
kept=$(sed -n 's/.*; kept builds in //p' options-changed.err)
[ -x "$kept/default+unroll=600-def" ] || fail "options-changed run did not keep the builds in $kept"
rm -rf "${kept%/builds}"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100,default+unroll=600=0.01,default+interleave=4=100,default+max-bandwidth=100,default+inline=1000=100 tune identical identical.fas -o identical.fasprof
[ "$status" -eq 0 ] || { cat identical.err >&2; fail "identical run exited $status"; }
grep -q -x -F 'fas perf: options: default+unroll=1200 builds the same binary as default+unroll=600; not measured' identical.err || fail "identical: no unroll=1200 line: $(cat identical.err)"
grep -q -x -F 'fas perf: options: default+unroll=2400 builds the same binary as default+unroll=600; not measured' identical.err || fail "identical: no unroll=2400 line: $(cat identical.err)"
grep -q -x -F 'fas perf: options: default+unroll=4800 builds the same binary as default+unroll=600; not measured' identical.err || fail "identical: no unroll=4800 line: $(cat identical.err)"
grep -q -x -F 'fas perf: options: default+O3 builds the same binary as default; not measured' identical.err || fail "identical: no O3 line: $(cat identical.err)"
grep -E -q '^(check|select|confirm) default\+(unroll=1200|unroll=2400|unroll=4800|O3) ' identical.log && fail "identical: an identical build was measured"
[ "$(sed -n '8p' identical.fasprof)" = 'options unroll=600' ] || fail "identical profile: $(sed -n '8p' identical.fasprof)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100 tune loopless empty.fas -o loopless.fasprof
[ "$status" -eq 0 ] || { cat loopless.err >&2; fail "loopless run exited $status"; }
[ "$(grep -c -x -E 'fas perf: options: default\+(unroll=600|unroll=1200|unroll=2400|unroll=4800|O3|inline=1000|interleave=4|max-bandwidth) builds the same binary as default; not measured' loopless.err)" -eq 8 ] || fail "loopless: option candidates were not all dropped: $(cat loopless.err)"
grep -E -q '^(check|select|confirm) default\+' loopless.log && fail "loopless: an option candidate was measured"
[ "$(sed -n '8p' loopless.fasprof)" = options ] || fail "loopless profile line 8: $(sed -n '8p' loopless.fasprof)"
no_tune_temps

cat >fix4.fas <<'FAS'
use "C" <stdio.h>

var src arr[1024, u8] = {}
var other arr[1024, u8] = {}
var dst arr[1024, u32] = {}

fn fill(n usize) i32 {
    for i usize = 0; i < n; i += 1 {
        src[i] = trunc[u8](i * 7 + 3)
        other[i] = trunc[u8](i * 13 + 5)
    }
    return 0
}

fn blend(n usize) u64 {
    sum u64 = 0
    for i usize = 0; i < n; i += 1 {
        dst[i] = zext[u32](src[i]) * 3 + zext[u32](other[i])
    }
    for i usize = 0; i < n; i += 1 {
        sum += zext[u64](dst[i])
    }
    return sum
}

fn heavy(x u64) u64 {
    a0 u64 = x * 6364136223846793005 + 1442695040888963407
    a1 u64 = a0 * 2654435761 + 1
    a2 u64 = a1 + (a1 << 3)
    a3 u64 = a2 ^ (a2 >> 6)
    a4 u64 = a3 * 2654435761 + 4
    a5 u64 = a4 + (a4 << 6)
    a6 u64 = a5 ^ (a5 >> 9)
    a7 u64 = a6 * 2654435761 + 7
    a8 u64 = a7 + (a7 << 9)
    a9 u64 = a8 ^ (a8 >> 12)
    a10 u64 = a9 * 2654435761 + 10
    a11 u64 = a10 + (a10 << 1)
    a12 u64 = a11 ^ (a11 >> 15)
    a13 u64 = a12 * 2654435761 + 13
    a14 u64 = a13 + (a13 << 4)
    a15 u64 = a14 ^ (a14 >> 18)
    a16 u64 = a15 * 2654435761 + 16
    a17 u64 = a16 + (a16 << 7)
    a18 u64 = a17 ^ (a17 >> 4)
    a19 u64 = a18 * 2654435761 + 19
    a20 u64 = a19 + (a19 << 10)
    a21 u64 = a20 ^ (a20 >> 7)
    a22 u64 = a21 * 2654435761 + 22
    a23 u64 = a22 + (a22 << 2)
    a24 u64 = a23 ^ (a23 >> 10)
    a25 u64 = a24 * 2654435761 + 25
    a26 u64 = a25 + (a25 << 5)
    a27 u64 = a26 ^ (a26 >> 13)
    a28 u64 = a27 * 2654435761 + 28
    a29 u64 = a28 + (a28 << 8)
    a30 u64 = a29 ^ (a29 >> 16)
    a31 u64 = a30 * 2654435761 + 31
    a32 u64 = a31 + (a31 << 11)
    a33 u64 = a32 ^ (a32 >> 19)
    a34 u64 = a33 * 2654435761 + 34
    a35 u64 = a34 + (a34 << 3)
    a36 u64 = a35 ^ (a35 >> 5)
    a37 u64 = a36 * 2654435761 + 37
    a38 u64 = a37 + (a37 << 6)
    a39 u64 = a38 ^ (a38 >> 8)
    a40 u64 = a39 * 2654435761 + 40
    a41 u64 = a40 + (a40 << 9)
    a42 u64 = a41 ^ (a41 >> 11)
    a43 u64 = a42 * 2654435761 + 43
    a44 u64 = a43 + (a43 << 1)
    a45 u64 = a44 ^ (a44 >> 14)
    a46 u64 = a45 * 2654435761 + 46
    a47 u64 = a46 + (a46 << 4)
    a48 u64 = a47 ^ (a47 >> 17)
    a49 u64 = a48 * 2654435761 + 49
    a50 u64 = a49 + (a49 << 7)
    a51 u64 = a50 ^ (a50 >> 3)
    a52 u64 = a51 * 2654435761 + 52
    a53 u64 = a52 + (a52 << 10)
    a54 u64 = a53 ^ (a53 >> 6)
    a55 u64 = a54 * 2654435761 + 55
    a56 u64 = a55 + (a55 << 2)
    a57 u64 = a56 ^ (a56 >> 9)
    a58 u64 = a57 * 2654435761 + 58
    a59 u64 = a58 + (a58 << 5)
    a60 u64 = a59 ^ (a59 >> 12)
    a61 u64 = a60 * 2654435761 + 61
    a62 u64 = a61 + (a61 << 8)
    a63 u64 = a62 ^ (a62 >> 15)
    a64 u64 = a63 * 2654435761 + 64
    a65 u64 = a64 + (a64 << 11)
    a66 u64 = a65 ^ (a65 >> 18)
    a67 u64 = a66 * 2654435761 + 67
    a68 u64 = a67 + (a67 << 3)
    a69 u64 = a68 ^ (a68 >> 4)
    a70 u64 = a69 * 2654435761 + 70
    return a70 ^ x
}

fn churn(seed u64) u64 {
    acc u64 = seed
    for i usize = 0; i < 300; i += 1 {
        acc = heavy(acc) + zext[u64](dst[i])
        acc = heavy(acc ^ 5) + zext[u64](dst[i + 1])
    }
    return acc
}

fn main() i32 {
    if fill(1024) != 0 { return 3 }
    printed i32 = printf(c"blend %lu\n", blend(1024))
    if printed <= 0 { return 1 }
    printed2 i32 = printf(c"churn %lu\n", churn(300) ^ heavy(9))
    if printed2 <= 0 { return 4 }
    return 0
}
FAS

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100,default+unroll=2400=0.01,default+unroll=2400+inline=1000=0.005,default+unroll=2400+inline=1000+interleave=4=0.01,default+unroll=2400+inline=1000+max-bandwidth=0.0025,default+unroll=2400+inline=1000+interleave=4+max-bandwidth=0.005 tune options-stages fix4.fas -o options-stages.fasprof
[ "$status" -eq 0 ] || { cat options-stages.err >&2; fail "options-stages run exited $status"; }
[ "$(sed -n '5p;8p' options-stages.fasprof)" = "$(printf 'choice default\noptions unroll=2400 inline=1000 max-bandwidth')" ] || fail "options-stages profile: $(head -n 8 options-stages.fasprof)"
grep -q -x -F 'fas perf: options: default+unroll=2400+O3+inline=1000 builds the same binary as default+unroll=2400+inline=1000; not measured' options-stages.err || fail "options-stages: no stage 3 O3 line: $(cat options-stages.err)"
grep -q -x -F 'fas perf: options: default+unroll=2400+O3+inline=1000+max-bandwidth builds the same binary as default+unroll=2400+inline=1000+max-bandwidth; not measured' options-stages.err || fail "options-stages: no stage 4 O3 line: $(cat options-stages.err)"
[ "$(grep -c -E '^fas perf: kept options ' options-stages.err)" -eq 3 ] || fail "options-stages: expected three kept option stages: $(cat options-stages.err)"
grep -E -q '^fas perf: kept options unroll=2400 \([0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\) \(measured on one input\)$' options-stages.err || fail "options-stages: no first option line: $(cat options-stages.err)"
grep -E -q '^fas perf: kept options unroll=2400 inline=1000 \([0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\) \(measured on one input\)$' options-stages.err || fail "options-stages: no second option line: $(cat options-stages.err)"
grep -E -q '^fas perf: kept options unroll=2400 inline=1000 max-bandwidth \([0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\) \(measured on one input\)$' options-stages.err || fail "options-stages: no third option line: $(cat options-stages.err)"
grep -E -q '^fas perf: kept no more options \(best interleave=4 [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\) \(measured on one input\)$' options-stages.err || fail "options-stages: no last option line: $(cat options-stages.err)"
grep -E -q '^fas perf: kept the profile \(default [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%(, loops aligned to (32|64))?(, functions aligned to 64)?, options unroll=2400 inline=1000 max-bandwidth\) \(measured on one input\)$' options-stages.err || fail "options-stages: no kept line: $(cat options-stages.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100,default+unroll=2400=2,default+unroll=4800=2,default+O3=2,default+inline=1000=2,default+interleave=4=2,default+max-bandwidth=2 tune options-none fix4.fas -o options-none.fasprof
[ "$status" -eq 0 ] || { cat options-none.err >&2; fail "options-none run exited $status"; }
[ "$(sed -n '5p;8p' options-none.fasprof)" = "$(printf 'choice default\noptions')" ] || fail "options-none profile: $(head -n 8 options-none.fasprof)"
[ "$(grep -c -E '^fas perf: kept no options \(best [^)]*\)' options-none.err)" -eq 1 ] || fail "options-none: no single first stage line: $(cat options-none.err)"
[ "$(grep -c -E '^fas perf: kept (no )?(more )?options ' options-none.err)" -eq 1 ] || fail "options-none: an option stage ran after nothing was kept: $(cat options-none.err)"
grep -E -q '^(select|confirm) default\+(unroll|O3|inline|interleave|max-bandwidth)' options-none.log || fail "options-none: no option candidate was measured"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100,default+inline=1000=0.01,default+unroll=2400=2,default+unroll=4800=2,default+O3=2,default+interleave=4=2,default+max-bandwidth=2,default+unroll=2400+inline=1000=0.02,default+unroll=4800+inline=1000=0.02,default+O3+inline=1000=0.02,default+inline=1000+interleave=4=0.02,default+inline=1000+max-bandwidth=0.02 tune options-inline fix4.fas -o options-inline.fasprof
[ "$status" -eq 0 ] || { cat options-inline.err >&2; fail "options-inline run exited $status"; }
[ "$(sed -n '5p;8p' options-inline.fasprof)" = "$(printf 'choice default\noptions inline=1000')" ] || fail "options-inline profile: $(head -n 8 options-inline.fasprof)"
[ "$(grep -c -E '^fas perf: kept options inline=1000 \(' options-inline.err)" -eq 1 ] || fail "options-inline: no first option line: $(cat options-inline.err)"
[ "$(grep -c -E '^fas perf: kept no more options \(best [^)]*\)' options-inline.err)" -eq 1 ] || fail "options-inline: no stopping line: $(cat options-inline.err)"
[ "$(grep -c -E '^fas perf: kept (no )?(more )?options ' options-inline.err)" -eq 2 ] || fail "options-inline: an option stage ran after the stop: $(cat options-inline.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100,default+unroll=2400=0.01,default+unroll=2400+inline=1000=0.005,default+unroll=2400+inline=1000@with=1 tune options-held-stop fix4.fas -o options-held-stop.fasprof -- first --check-with -- second
[ "$status" -eq 0 ] || { cat options-held-stop.err >&2; fail "options-held-stop run exited $status"; }
[ "$(sed -n '5p;8p' options-held-stop.fasprof)" = "$(printf 'choice default\noptions unroll=2400')" ] || fail "options-held-stop profile: $(head -n 8 options-held-stop.fasprof)"
[ "$(grep -c -E '^fas perf: kept options ' options-held-stop.err)" -eq 1 ] || fail "options-held-stop: expected one kept option stage: $(cat options-held-stop.err)"
[ "$(grep -c -E '^fas perf: kept no more options \(best ' options-held-stop.err)" -eq 1 ] || fail "options-held-stop: no stopping line: $(cat options-held-stop.err)"
grep -q -F 'fas perf: kept options unroll=2400 inline=1000 ' options-held-stop.err && fail "options-held-stop: the second-input rejection was kept: $(cat options-held-stop.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=0.01 tune held args.fas -o held.fasprof -- first --check-with -- second
[ "$status" -eq 0 ] || { cat held.err >&2; fail "held run exited $status"; }
[ "$(sed -n '5p' held.fasprof)" = 'choice pgo' ] || fail "held run kept '$(sed -n '5p' held.fasprof)'"
[ "$(head -n 6 held.log | tr '\n' ' ')" = 'train portable train native reference default reference default reference-with default reference-with default ' ] || fail "held: the log starts with $(head -n 6 held.log)"
check_totals held.log "$ARENA_PLACEMENTS" 'check select confirm' default=15 pgo=15 native=8 native-pgo=8
check_totals held.log "$ARENA_PLACEMENTS" 'check-with confirm-with' default=8 pgo=8 native=1 native-pgo=1
check_rotation held.log confirm-with 18
grep -q '^select .*' held.log || fail "held: no selection"
[ "$(grep -c '^confirm-with ' held.log)" -eq 126 ] || fail "held: $(grep -c '^confirm-with ' held.log) second-input confirmation runs"
grep -E -q '^fas perf: kept the profile \(pgo [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%(, loops aligned to (32|64))?(, functions aligned to 64)?; second input [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\)$' held.err || fail "held: no kept line: $(cat held.err)"
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=0.01,pgo@with=100,default-32=2,default-64=2,default-def-f64=2,default-32-f64=2,default-64-f64=2 tune heldlost args.fas -o heldlost.fasprof -- first --check-with -- second
[ "$status" -eq 0 ] || { cat heldlost.err >&2; fail "held-lost run exited $status"; }
[ "$(sed -n '5p' heldlost.fasprof)" = 'choice default' ] || fail "held-lost run kept '$(sed -n '5p' heldlost.fasprof)'"
grep -E -q '^fas perf: kept the default build \(best pgo [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%; second input 0\.0[0-9]{2}×, band [0-9]+\.[0-9]%, slower at some placement\)$' heldlost.err || fail "held-lost: no kept line: $(cat heldlost.err)"
check_totals heldlost.log "$ARENA_PLACEMENTS" 'check-with confirm-with' default=8 pgo=8 native=1 native-pgo=1
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
check_totals heldfirst.log "$ARENA_PLACEMENTS" 'check-with confirm-with' default=8 pgo=8 native=1 native-pgo=1
no_tune_temps

FAS_TUNE_TEST_SCALE=pgo=100,native=100,native-pgo=100,default+O3=0.01,default+unroll=2400=0.5,default+unroll=2400+O3=0.001,default+O3@with=0.5,default+unroll=2400+O3@with=0.25,default+interleave=4=100,default+max-bandwidth=100,default+inline=1000=100 tune heldoptions vec.fas -o heldoptions.fasprof -- first --check-with -- second
[ "$status" -eq 0 ] || { cat heldoptions.err >&2; fail "held options run exited $status"; }
[ "$(sed -n '5p;8p' heldoptions.fasprof)" = "$(printf 'choice default\noptions unroll=2400 O3')" ] || fail "held options profile: $(head -n 8 heldoptions.fasprof)"
stage1=$(grep -E '^fas perf: kept options O3 \(' heldoptions.err) || fail "held options: no first option line: $(cat heldoptions.err)"
stage2=$(grep -E '^fas perf: kept options unroll=2400 O3 \(' heldoptions.err) || fail "held options: no second option line: $(cat heldoptions.err)"
final=$(grep -E '^fas perf: kept the profile \(' heldoptions.err) || fail "held options: no kept line: $(cat heldoptions.err)"
printf '%s\n' "$stage1" | grep -E -q '^fas perf: kept options O3 \([0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%; second input [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\)$' || fail "held options: first option line: $stage1"
printf '%s\n' "$stage2" | grep -E -q '^fas perf: kept options unroll=2400 O3 \([0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%; second input [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\)$' || fail "held options: second option line: $stage2"
printf '%s\n' "$final" | grep -E -q '^fas perf: kept the profile \(default [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%(, loops aligned to (32|64))?(, functions aligned to 64)?, options unroll=2400 O3; second input [0-9]+\.[0-9]{3}×, band [0-9]+\.[0-9]%\)$' || fail "held options: kept line: $final"
held_values() { printf '%s\n' "$1" | sed -n 's/.*; second input \([0-9.]*\)×, band \([0-9.]*\)%).*/\1 \2/p'; }
g1=$(held_values "$stage1" | cut -d ' ' -f 1)
b1=$(held_values "$stage1" | cut -d ' ' -f 2)
g2=$(held_values "$stage2" | cut -d ' ' -f 1)
b2=$(held_values "$stage2" | cut -d ' ' -f 2)
fg=$(held_values "$final" | cut -d ' ' -f 1)
fb=$(held_values "$final" | cut -d ' ' -f 2)
awk -v g1="$g1" -v b1="$b1" -v g2="$g2" -v b2="$b2" -v fg="$fg" -v fb="$fb" 'BEGIN {
  dg = fg - g1 * g2
  if (dg < 0) dg = -dg
  db = fb - (b1 + b2)
  if (db < 0) db = -db
  exit (dg <= 0.0006 + 0.0005 * (g1 + g2) && db <= 0.15 + 0.000001) ? 0 : 1
}' || fail "held options: combined second input $fg, band $fb does not match the kept stages ($g1 $b1, $g2 $b2): $(cat heldoptions.err)"
no_tune_temps

OPTION_SCALE=pgo=100,native=100,native-pgo=100,default+unroll=1200=0.01,default+interleave=4=100,default+max-bandwidth=100,default+inline=1000=100

FAS_TUNE_TEST_BUILD_SECONDS=default=1,default+unroll=2400=40,default+O3=40 FAS_TUNE_TEST_SCALE=$OPTION_SCALE tune options-slow vec.fas -o options-slow.fasprof
[ "$status" -eq 0 ] || { cat options-slow.err >&2; fail "options-slow run exited $status"; }
grep -q -x -F 'fas perf: options: default+unroll=2400 skipped (build took 40s, base 1s)' options-slow.err || fail "options-slow: no skip line: $(cat options-slow.err)"
grep -q -E '^[a-z]+ default\+unroll=1200 ' options-slow.log || fail "options-slow: unroll=1200 was not measured after the skip"
grep -q '^fas perf: kept options unroll=1200 (' options-slow.err || fail "options-slow: no kept options line after the skip: $(cat options-slow.err)"
! grep -q -E '^[a-z]+ default\+unroll=2400 ' options-slow.log || fail "options-slow: skipped candidate was measured"
grep -q -x -F 'fas perf: options: default+O3 skipped (build took 40s, base 1s)' options-slow.err || fail "options-slow: no O3 skip line: $(cat options-slow.err)"
! grep -q 'O3' options-slow.log || fail "options-slow: skipped O3 was measured"
! grep -q 'unroll=1200+O3' options-slow.err || fail "options-slow: skipped O3 was proposed again: $(cat options-slow.err)"
no_tune_temps

FAS_TUNE_TEST_TEXT_BYTES=default+unroll=2400=104857600 FAS_TUNE_TEST_SCALE=$OPTION_SCALE tune options-text vec.fas -o options-text.fasprof
[ "$status" -eq 0 ] || { cat options-text.err >&2; fail "options-text run exited $status"; }
grep -q -x -F 'fas perf: options: default+unroll=2400 skipped (.text 100 MiB)' options-text.err || fail "options-text: no skip line: $(cat options-text.err)"
! grep -q -E '^[a-z]+ default\+unroll=2400 ' options-text.log || fail "options-text: skipped candidate was measured"
grep -q '^fas perf: kept options unroll=1200 (' options-text.err || fail "options-text: no kept options line after the skip: $(cat options-text.err)"
no_tune_temps

FAS_TUNE_TEST_FAIL_BUILD=default+unroll=2400 FAS_TUNE_TEST_SCALE=$OPTION_SCALE tune options-failed vec.fas -o options-failed.fasprof
[ "$status" -eq 0 ] || { cat options-failed.err >&2; fail "options-failed run exited $status"; }
grep -q -x -F 'fas perf: options: default+unroll=2400 skipped (build failed)' options-failed.err || fail "options-failed: no skip line: $(cat options-failed.err)"
! grep -q -E '^[a-z]+ default\+unroll=2400 ' options-failed.log || fail "options-failed: skipped candidate was measured"
grep -q '^fas perf: kept options unroll=1200 (' options-failed.err || fail "options-failed: tuning did not continue: $(cat options-failed.err)"
[ -s options-failed.fasprof ] || fail "options-failed: no profile written"
no_tune_temps

FAS_TUNE_TEST_BUILD_SECONDS=default=5,default+unroll=2400=45 FAS_TUNE_TEST_SCALE=$OPTION_SCALE tune options-nine-x vec.fas -o options-nine-x.fasprof
[ "$status" -eq 0 ] || { cat options-nine-x.err >&2; fail "options-nine-x run exited $status"; }
! grep -q 'default+unroll=2400 skipped' options-nine-x.err || fail "options-nine-x: 9x build was skipped: $(cat options-nine-x.err)"
grep -q -E '^[a-z]+ default\+unroll=2400 ' options-nine-x.log || fail "options-nine-x: 9x build was not measured"
no_tune_temps

SLOW_ALL=default=1
for option in unroll=600 unroll=1200 unroll=2400 unroll=4800 O3 inline=1000 interleave=4 max-bandwidth; do
  SLOW_ALL=$SLOW_ALL,default+$option=40
done
FAS_TUNE_TEST_BUILD_SECONDS=$SLOW_ALL tune options-all-skipped vec.fas -o options-all-skipped.fasprof
[ "$status" -eq 0 ] || { cat options-all-skipped.err >&2; fail "options-all-skipped run exited $status"; }
[ "$(grep -c '^fas perf: options: ' options-all-skipped.err)" -eq 8 ] || fail "options-all-skipped: expected 8 option lines: $(cat options-all-skipped.err)"
[ "$(grep -c -x -E 'fas perf: options: default\+[^ ]+ skipped \(build took 40s, base 1s\)' options-all-skipped.err)" -eq 8 ] || fail "options-all-skipped: an option line is not a skip line: $(cat options-all-skipped.err)"
! grep -q -E '^fas perf: kept (no )?(more )?options ' options-all-skipped.err || fail "options-all-skipped: kept an option: $(cat options-all-skipped.err)"
! grep -q -E '^[a-z]+ default\+' options-all-skipped.log || fail "options-all-skipped: a skipped option was checked, selected or confirmed"
no_tune_temps

FAS_TUNE_TEST_FAIL_BUILD=pgo tune choice-failed vec.fas -o choice-failed.fasprof
[ "$status" -ne 0 ] || fail "choice-failed: a failed choice build did not stop tuning"
[ "$(cat choice-failed.err)" = 'error: build of pgo failed' ] || fail "choice-failed: unexpected stderr: $(cat choice-failed.err)"
no_tune_temps

echo "tune_measure: passed"
