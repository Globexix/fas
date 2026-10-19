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
  echo "tune_session: $*" >&2
  exit 1
}

command -v script >/dev/null 2>&1 || {
  echo "tune_session: skipped (no script command)"
  exit 0
}

CC=$(resolve_tool_path "$CC")
LLVM_OPT=$(resolve_tool_path "$LLVM_OPT")
LLVM_LLC=$(resolve_tool_path "$LLVM_LLC")
OCAML_FAS=$(resolve_tool_path "$OCAML_FAS")
export FAS_CC="$CC" FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC"

SESSION_TMP=$(mktemp -d)
WORK=$SESSION_TMP/work
TMPDIR=$SESSION_TMP/tmp
FAKE_BIN=$SESSION_TMP/bin
SUDO_BIN=$SESSION_TMP/sudo-bin
mkdir -p "$WORK" "$TMPDIR" "$FAKE_BIN" "$SUDO_BIN"
export TMPDIR FAKE_BIN
trap 'rm -rf "$SESSION_TMP"' EXIT HUP INT TERM
cd "$WORK"

cat >"$FAKE_BIN/systemctl" <<'SH'
#!/bin/sh
d=$FAKE_ROOT/units
case $1 in
show) if [ -e "$d/$5" ]; then cat "$d/$5"; else echo; fi ;;
set-property) printf '%s\n' "${4#AllowedCPUs=}" >"$d/$3" ;;
list-units) for f in "$d"/session-*.scope; do [ -e "$f" ] && echo "${f##*/} loaded active running"; done ;;
*) exit 1 ;;
esac
exit 0
SH
cat >"$FAKE_BIN/taskset" <<'SH'
#!/bin/sh
d=$FAKE_ROOT/procs
case $1 in
-pc) [ -d "$d/$2" ] || exit 1; echo "pid $2's current affinity list: $(cat "$d/$2/mask")" ;;
-a) [ -d "$d/$4" ] || exit 1; echo "$3" >"$d/$4/mask" ;;
*) exit 1 ;;
esac
SH
cat >"$FAKE_BIN/pgrep" <<'SH'
#!/bin/sh
ls "$FAKE_ROOT/procs"
SH
cat >"$FAKE_BIN/ps" <<'SH'
#!/bin/sh
[ -e "$FAKE_ROOT/procs/$4/start" ] && cat "$FAKE_ROOT/procs/$4/start"
exit 0
SH
cat >"$SUDO_BIN/sudo" <<'SH'
#!/bin/sh
[ "$1 $2 $3" = "-- sh -c" ] || exit 97
[ -n "${8:-}" ] && [ "$8" = "$FAKE_ROOT/root" ] || exit 98
echo "$5 $6" >>"$FAKE_ROOT/sudo.log"
echo "$7" >"$FAKE_ROOT/sudo.pid"
shift
PATH=$FAKE_BIN:$PATH "$@"
SH
chmod +x "$FAKE_BIN"/* "$SUDO_BIN/sudo"

put() {
  mkdir -p "$(dirname "$1")"
  printf '%s\n' "$2" >"$1"
}

fake_root() {
  FAKE_ROOT=$1
  shift
  r=$FAKE_ROOT/root
  rm -rf "$FAKE_ROOT"
  mkdir -p "$r/run" "$FAKE_ROOT/units" "$FAKE_ROOT/procs"
  for entry in "$@"; do
    c=${entry%%:*}
    put "$r/sys/devices/system/cpu/cpu$c/topology/thread_siblings_list" "${entry#*:}"
    put "$r/sys/devices/system/cpu/cpu$c/cpufreq/scaling_governor" schedutil
    for s in 0:0 1:1 2:400; do
      put "$r/sys/devices/system/cpu/cpu$c/cpuidle/state${s%%:*}/latency" "${s#*:}"
      put "$r/sys/devices/system/cpu/cpu$c/cpuidle/state${s%%:*}/disable" 0
    done
  done
  put "$r/proc/sys/kernel/nmi_watchdog" 1
  put "$r/proc/irq/default_smp_affinity" ff
  put "$r/proc/irq/1/smp_affinity_list" 0-3
  put "$r/proc/irq/9/smp_affinity_list" 2
  put "$r/sys/devices/virtual/workqueue/cpumask" ff
  put "$FAKE_ROOT/units/system.slice" ""
  put "$FAKE_ROOT/units/init.scope" 1-3
  put "$FAKE_ROOT/units/session-2.scope" ""
  export FAKE_ROOT
}

add_proc() {
  put "$FAKE_ROOT/procs/$1/mask" "$2"
  put "$FAKE_ROOT/procs/$1/start" "Mon_Oct_20_$1"
}

snapshot() {
  (cd "$FAKE_ROOT" && find root units procs -type f | sort | while read -r f; do printf '%s=%s\n' "$f" "$(cat "$f")"; done)
}

value() {
  cat "$FAKE_ROOT/$1"
}

expect() {
  [ "$(value "$1")" = "$2" ] || fail "$3: $1 is $(value "$1"), expected $2"
}

FAS_TUNE_TEST_TOOLS="$WORK/tools" "$OCAML_FAS" perf --tune missing.fas 2>/dev/null || true
mkdir -p "$WORK/tools"
FAS_TUNE_TEST_TOOLS="$WORK/tools" "$OCAML_FAS" perf --tune missing.fas || fail "tools did not build"
SCRIPT=$(cat "$WORK/tools/isolate.sh")
isolate() {
  PATH=$FAKE_BIN:$PATH sh -c "$SCRIPT" "$1" "$2" 0 "$FAKE_ROOT/root"
}

fake_root "$SESSION_TMP/fake" 0:0,2 1:1,3 2:0,2 3:1,3
add_proc 100 0-3
add_proc 101 1
add_proc 102 0,2
add_proc 103 3
snapshot >before
isolate on 1 >on.out || fail "on failed"
[ "$(cat on.out)" = "isolated 1,3" ] || fail "on printed $(cat on.out)"
cp "$FAKE_ROOT/root/run/fas-tune-isolate" first.state
expect root/sys/devices/system/cpu/cpu1/cpufreq/scaling_governor performance "on"
expect root/sys/devices/system/cpu/cpu3/cpufreq/scaling_governor performance "on"
expect root/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor schedutil "on"
expect root/sys/devices/system/cpu/cpu1/cpuidle/state1/disable 0 "on"
expect root/sys/devices/system/cpu/cpu1/cpuidle/state2/disable 1 "on"
expect root/sys/devices/system/cpu/cpu3/cpuidle/state2/disable 1 "on"
expect root/sys/devices/system/cpu/cpu0/cpuidle/state2/disable 0 "on"
expect root/proc/sys/kernel/nmi_watchdog 0 "on"
expect root/proc/irq/default_smp_affinity 5 "on"
expect root/sys/devices/virtual/workqueue/cpumask 5 "on"
expect root/proc/irq/1/smp_affinity_list 0,2 "on"
expect units/system.slice 0,2 "on"
expect units/init.scope 0,2 "on"
expect units/session-2.scope 0,2 "on"
expect procs/100/mask 0,2 "on"
expect procs/101/mask 1 "on"
expect procs/102/mask 0,2 "on"
expect procs/103/mask 3 "on"
isolate on 1 >/dev/null || fail "second on failed"
cmp -s first.state "$FAKE_ROOT/root/run/fas-tune-isolate" || fail "a second on rewrote the saved values"
add_proc 104 0,2
add_proc 105 0
isolate off 1 >off.out || fail "off failed"
[ "$(cat off.out)" = "released 1,3" ] || fail "off printed $(cat off.out)"
[ ! -e "$FAKE_ROOT/root/run/fas-tune-isolate" ] || fail "off left the state file"
[ ! -e "$FAKE_ROOT/root/run/fas-tune-isolate.affinity" ] || fail "off left the affinity file"
expect procs/104/mask 0-3 "a process started during isolation"
expect procs/105/mask 0 "a process pinned during isolation"
rm -rf "$FAKE_ROOT/procs/104" "$FAKE_ROOT/procs/105"
snapshot >after
diff before after >restore.diff || fail "off did not restore every value: $(cat restore.diff)"

add_proc 106 0-3
put "$FAKE_ROOT/procs/106/start" "Mon_reused"
isolate on 1 >/dev/null
put "$FAKE_ROOT/procs/106/start" "Tue_other"
put "$FAKE_ROOT/procs/106/mask" 0
isolate off 1 >/dev/null
expect procs/106/mask 0 "a reused process id"
rm -rf "$FAKE_ROOT/procs/106"

printf '' | isolate hold 1 >hold.out || fail "hold failed"
[ "$(cat hold.out)" = "isolated 1,3
released 1,3" ] || fail "hold printed $(cat hold.out)"
snapshot >after
diff before after >restore.diff || fail "hold did not restore every value: $(cat restore.diff)"

mkfifo hold.fifo
PATH=$FAKE_BIN:$PATH sh -c "$SCRIPT" hold 1 0 "$FAKE_ROOT/root" <hold.fifo >hold.out &
holder=$!
exec 7>hold.fifo
tries=0
until grep -q isolated hold.out 2>/dev/null; do
  tries=$((tries + 1))
  [ "$tries" -lt 100 ] || fail "hold did not isolate"
  sleep 0.05
done
kill -INT "$holder" 2>/dev/null || true
kill -TERM "$holder" 2>/dev/null || true
kill -HUP "$holder" 2>/dev/null || true
sleep 0.1
expect procs/100/mask 0,2 "hold after signals"
exec 7>&-
wait "$holder" || fail "hold exited with a failure after signals"
snapshot >after
diff before after >restore.diff || fail "hold did not restore after signals: $(cat restore.diff)"

isolate off 1 >/dev/null || fail "off without state failed"
expect procs/102/mask 0-3 "off without state"
expect procs/101/mask 1 "off without state"
status=0
isolate on 7 2>bad.err || status=$?
[ "$status" -eq 2 ] && grep -q 'no CPU 7' bad.err || fail "unknown CPU accepted"
status=0
isolate on x 2>bad.err || status=$?
[ "$status" -eq 2 ] && grep -q 'must be a number' bad.err || fail "non-numeric CPU accepted"

wide=
c=0
while [ "$c" -lt 40 ]; do
  case $c in
    1 | 21) sib=1,21 ;;
    *) sib=$c ;;
  esac
  wide="$wide $c:$sib"
  c=$((c + 1))
done
fake_root "$SESSION_TMP/wide" $wide
isolate on 21 >/dev/null || fail "wide on failed"
expect root/proc/irq/default_smp_affinity ff,ffdffffd "a 40-CPU mask"
expect root/sys/devices/virtual/workqueue/cpumask ff,ffdffffd "a 40-CPU mask"
isolate off 21 >/dev/null
expect root/proc/irq/default_smp_affinity ff "a 40-CPU restore"

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

real_root() {
  set --
  for f in /sys/devices/system/cpu/cpu[0-9]*/topology/thread_siblings_list; do
    c=${f#/sys/devices/system/cpu/cpu}
    c=${c%%/*}
    set -- "$@" "$c:$(cat "$f")"
  done
  fake_root "$SESSION_TMP/real" "$@"
  add_proc 200 "0-$(($# - 1))"
  add_proc 201 0
  snapshot >"$WORK/real.before"
}

no_temps() {
  leftovers=$(ls -A "$TMPDIR")
  [ -z "$leftovers" ] || fail "$1: temporary files left behind: $leftovers"
}

session() {
  name=$1
  answers=$2
  shift 2
  : >"$name.log"
  rm -f "$FAKE_ROOT/sudo.log" "$name.fasprof"
  status=0
  printf "$answers" | PATH="$SUDO_BIN:$PATH" FAS_TUNE_TEST_ROOT="$FAKE_ROOT/root" \
    FAS_TUNE_TEST_SCALE=pgo=0.01 FAS_TUNE_LOG="$WORK/$name.log" \
    script -qec "$OCAML_FAS perf --tune work.fas -o $name.fasprof $*" /dev/null >"$name.out" 2>&1 || status=$?
}

plain() {
  name=$1
  shift
  : >"$name.log"
  rm -f "$FAKE_ROOT/sudo.log" "$name.fasprof"
  status=0
  PATH="$SUDO_BIN:$PATH" FAS_TUNE_TEST_ROOT="$FAKE_ROOT/root" FAS_TUNE_TEST_SCALE=pgo=0.01 \
    FAS_TUNE_LOG="$WORK/$name.log" "$OCAML_FAS" perf --tune work.fas -o "$name.fasprof" "$@" \
    </dev/null >"$name.out" 2>"$name.err" || status=$?
}

restored() {
  snapshot >"$WORK/real.after"
  diff "$WORK/real.before" "$WORK/real.after" >"$WORK/real.diff" || fail "$1: fake root not restored: $(cat "$WORK/real.diff")"
  [ ! -e "$FAKE_ROOT/root/run/fas-tune-isolate" ] || fail "$1: state file left"
}

real_root

plain notty
[ "$status" -eq 0 ] || fail "no tty: exit $status: $(cat notty.err)"
[ -s notty.fasprof ] || fail "no tty: no profile"
! grep -q 'continue?\|sudo\|tuning work.fas' notty.err || fail "no tty: prompted: $(cat notty.err)"
[ ! -e "$FAKE_ROOT/sudo.log" ] || fail "no tty: ran sudo"
no_temps "no tty"

session decline 'n\n'
[ "$status" -eq 0 ] || fail "declined: exit $status: $(cat decline.out)"
grep -q 'continue? \[Y/n\]' decline.out || fail "declined: no continue prompt: $(cat decline.out)"
grep -q 'fas perf: stopped; no profile written' decline.out || fail "declined: no stop message"
[ ! -e decline.fasprof ] || fail "declined: profile written"
[ "$(cat decline.log)" = "train portable" ] || fail "declined: ran $(cat decline.log)"
[ ! -e "$FAKE_ROOT/sudo.log" ] || fail "declined: ran sudo"
no_temps "declined"

session nosudo 'y\n\n'
[ "$status" -eq 0 ] || fail "no sudo: exit $status: $(cat nosudo.out)"
for text in 'tuning work.fas for this machine' \
  'runs: 2 training, 2 reference, 288 selection (4 builds x 9 placements x 8), 126 confirmation (2 x 9 x 7); 12 builds' \
  'one training run took' 'Leave the machine alone' 'use sudo? [y/N]' \
  'keep other processes off CPUs' 'move interrupts and kernel workqueues off CPUs' \
  'set the performance governor and disable idle states deeper than 1 us' \
  'turn off the NMI watchdog' 'Every change is undone when tuning ends, also on Ctrl-C.'; do
  grep -q -F "$text" nosudo.out || fail "no sudo: missing '$text': $(cat nosudo.out)"
done
[ -s nosudo.fasprof ] || fail "no sudo: no profile"
[ ! -e "$FAKE_ROOT/sudo.log" ] || fail "no sudo: ran sudo"
no_temps "no sudo"

session noarena 'y\nn\n' --no-arena
[ "$status" -eq 0 ] || fail "no arena: exit $status"
grep -q -F '96 selection (4 builds x 3 placements x 8), 42 confirmation (2 x 3 x 7)' noarena.out || fail "no arena: wrong counts: $(cat noarena.out)"

session yes '' --yes
[ "$status" -eq 0 ] || fail "--yes: exit $status: $(cat yes.out)"
grep -q 'tuning work.fas for this machine' yes.out || fail "--yes: no announcement"
! grep -q 'continue?\|use sudo?' yes.out || fail "--yes: prompted: $(cat yes.out)"
[ ! -e "$FAKE_ROOT/sudo.log" ] || fail "--yes: ran sudo"
[ -s yes.fasprof ] || fail "--yes: no profile"

session held 'y\ny\n'
[ "$status" -eq 0 ] || fail "sudo: exit $status: $(cat held.out)"
cpu=$(sed -n 's/^  CPU \([0-9]*\).*/\1/p' held.out)
[ -n "$cpu" ] || fail "sudo: no CPU line: $(cat held.out)"
[ "$(cat "$FAKE_ROOT/sudo.log")" = "hold $cpu" ] || fail "sudo: called $(cat "$FAKE_ROOT/sudo.log")"
grep -q "fas perf: isolated CPUs .* for the measurement" held.out || fail "sudo: no isolation line: $(cat held.out)"
grep -q "fas perf: restored CPUs" held.out || fail "sudo: no restore line: $(cat held.out)"
[ -s held.fasprof ] || fail "sudo: no profile"
grep -q '^select ' held.log || fail "sudo: did not measure"
restored "sudo"
no_temps "sudo"

: >interrupt.log
rm -f "$FAKE_ROOT/sudo.log" "$FAKE_ROOT/sudo.pid" interrupt.fasprof
(printf 'y\ny\n'; sleep 30) | PATH="$SUDO_BIN:$PATH" FAS_TUNE_TEST_ROOT="$FAKE_ROOT/root" \
  FAS_TUNE_LOG="$WORK/interrupt.log" \
  script -qec "$OCAML_FAS perf --tune work.fas -o interrupt.fasprof" /dev/null >interrupt.out 2>&1 &
waiter=$!
tries=0
until grep -q '^select ' interrupt.log 2>/dev/null; do
  tries=$((tries + 1))
  [ "$tries" -lt 600 ] || fail "interrupt: measuring never started: $(cat interrupt.out)"
  sleep 0.05
done
[ -s "$FAKE_ROOT/sudo.pid" ] || fail "interrupt: sudo not running"
grep -q '^200 ' "$FAKE_ROOT/root/run/fas-tune-isolate.affinity" || fail "interrupt: not isolated"
kill -INT "$(cat "$FAKE_ROOT/sudo.pid")"
tries=0
while kill -0 "$(cat "$FAKE_ROOT/sudo.pid")" 2>/dev/null; do
  tries=$((tries + 1))
  [ "$tries" -lt 200 ] || fail "interrupt: fas did not stop"
  sleep 0.05
done
kill "$waiter" 2>/dev/null || true
wait "$waiter" 2>/dev/null || true
grep -q 'fas perf: interrupted; no profile written' interrupt.out || fail "interrupt: no message: $(cat interrupt.out)"
grep -q 'fas perf: restored CPUs' interrupt.out || fail "interrupt: not restored: $(cat interrupt.out)"
[ ! -e interrupt.fasprof ] || fail "interrupt: profile written"
restored "interrupt"
no_temps "interrupt"

printf '# cpu 0 pid 0\nfile\t%s\t%s\n' "$FAKE_ROOT/root/proc/sys/kernel/nmi_watchdog" 1 >"$FAKE_ROOT/root/run/fas-tune-isolate"
put "$FAKE_ROOT/root/proc/sys/kernel/nmi_watchdog" 0
plain stalenotty
grep -q 'an earlier tune session left CPU 0 isolated' stalenotty.err || fail "stale without tty: no notice: $(cat stalenotty.err)"
[ ! -e "$FAKE_ROOT/sudo.log" ] || fail "stale without tty: ran sudo"
[ -e "$FAKE_ROOT/root/run/fas-tune-isolate" ] || fail "stale without tty: state removed"
session stale 'y\nn\n'
[ "$(head -n 1 "$FAKE_ROOT/sudo.log")" = "off 0" ] || fail "stale: sudo not called with off first: $(cat "$FAKE_ROOT/sudo.log" 2>/dev/null)"
grep -q 'restore it now with sudo? \[Y/n\]' stale.out || fail "stale: no restore prompt: $(cat stale.out)"
restored "stale"

printf '# cpu 0 pid %s\n' "$$" >"$FAKE_ROOT/root/run/fas-tune-isolate"
session busy 'y\nn\n'
grep -q "another tune session (process $$) holds an isolated CPU" busy.out || fail "busy: no notice: $(cat busy.out)"
! grep -q 'use sudo?' busy.out || fail "busy: offered sudo"
[ ! -e "$FAKE_ROOT/sudo.log" ] || fail "busy: ran sudo"
rm -f "$FAKE_ROOT/root/run/fas-tune-isolate"

echo "tune_session: ok"
