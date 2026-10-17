#!/bin/sh
set -eu
export DEBUGINFOD_URLS=

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
START_DIR=$(pwd)
CC=$(printenv CC || printf 'clang-22')
LLVM_OPT=$(printenv LLVM_OPT || printf 'opt-22')
LLVM_LLC=$(printenv LLVM_LLC || printf 'llc-22')
OCAML_FAS=$(printenv OCAML_FAS || printf '%s/_build/default/bin/main.exe' "$ROOT")
LLVM_OBJCOPY=$(printenv LLVM_OBJCOPY || printf 'llvm-objcopy-22')
OBJDUMP_BIN=$(printenv FAS_OBJDUMP || printf 'llvm-objdump-22')
SYMBOLIZER_BIN=$(printenv FAS_SYMBOLIZER || printf 'llvm-symbolizer-22')

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
export FAS_CC="$CC" FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC"

PERF_TMP=$(mktemp -d)
WORK=$PERF_TMP/work
TMPDIR=$PERF_TMP/tmp
mkdir -p "$WORK" "$TMPDIR"
export TMPDIR
trap 'rm -rf "$PERF_TMP"' EXIT HUP INT TERM

fail() {
  echo "perf_command: $*" >&2
  exit 1
}

cat >"$WORK/nest.fas" <<'FAS'
fn mix(x u64) u64 {
    return (x ^ (x >> 31)) * 0x9E3779B97F4A7C15
}

var buf arr[65536, u32] = {}

fn tile(data addr, rows usize, cols usize) u64 {
    total u64 = 0
    for r usize = 0; r < rows; r += 1 {
        for c usize = 0; c < cols; c += 1 {
            total += zext[u64](data[u32, r * cols + c])
        }
    }
    return total
}

fn helper(x u64) u64 {
    return mix(x) + 1
}

fn divide(n u64, d u64) u64 {
    acc u64 = 0
    for i u64 = 0; i < n; i += 1 {
        acc += i / (d + i % 3)
    }
    return acc
}

fn main() i32 {
    seed u64 = 1
    for i u64 = 0; i < 65536; i += 1 {
        seed = mix(seed + i)
        buf[i] = trunc[u32](seed)
    }
    sum u64 = 0
    for pass usize = 0; pass < 3000; pass += 1 {
        sum += tile(&buf, 256, 256)
        sum += helper(sum)
    }
    sum += divide(65536, seed & 255)
    return trunc[i32](sum % 256)
}
FAS
cat >"$WORK/flat.fas" <<'FAS'
fn main() i32 { return 0 }
FAS
cat >"$WORK/bad.fas" <<'FAS'
fn main() i32 { return missing }
FAS

cat >"$WORK/perf_c_names.fas" <<'FAS'
use "C" <<C
#include <stdint.h>
static inline uint64_t c_step(uint64_t x) { return x * 3 + 1; }
C

fn main() i32 {
    acc u64 = 1
    for i u64 = 0; i < 65536; i += 1 {
        acc = c_step(acc) ^ i
    }
    return trunc[i32](acc & 127)
}
FAS
cat >"$WORK/perf_generic_names.fas" <<'FAS'
var buf arr[65536, u32] = {}

fn sum[T](data addr, n usize) T {
    total T = 0
    for i usize = 0; i < n; i += 1 {
        total += data[T, i]
    }
    return total
}

fn main() i32 {
    a u32 = sum[u32](&buf, 65536)
    b u64 = sum[u64](&buf, 32768)
    return trunc[i32](zext[u64](a) + b)
}
FAS
cat >"$WORK/perf_c_names.expected" <<'EXPECTED'
perf_c_names.fas  static (no run)
perf_c_names.fas:8 for i u64 = 0; i < 65536; i += 1 {       main
      not vectorized: value that could not be identified as reduction is used outside the loop   6 instr/iter   loads 0 stores 0 /iter   call kept: c_step (its definition is unavailable)
EXPECTED
cat >"$WORK/perf_generic_names.expected" <<'EXPECTED'
perf_generic_names.fas  static (no run)
perf_generic_names.fas:5 for i usize = 0; i < n; i += 1 {         sum[u32], sum[u64], inlined into main:12, main:13
      unrolled x16   2 instr/iter   loads 0 stores 0 /iter   1.0 cycles/iter   bound: register dependency
EXPECTED
cat >"$WORK/nest-default.expected" <<'EXPECTED'
nest.fas  static (no run)
nest.fas:9    for r usize = 0; r < rows; r += 1 {      tile, inlined into main:37
      27 instr/iter   loads 4 stores 0 /iter
nest.fas:10   for c usize = 0; c < cols; c += 1 {      tile, inlined into main:37
      vectorized x2 interleave 2   unrolled x2   15 instr/iter (4 elements)   loads 4 stores 0 /iter   3.9 cycles/iter
nest.fas:23   for i u64 = 0; i < n; i += 1 {           divide, inlined into main:40
      not vectorized: Cannot vectorize early exit loop with reductions or recurrences   loads 0 stores 0 /iter   trap check
      in main:40 (copy 1): 19 instr/iter   41.3 cycles/iter   bound: register dependency
      in main:40 (copy 2): 20 instr/iter
nest.fas:31   for i u64 = 0; i < 65536; i += 1 {       main
      not vectorized: value that could not be identified as reduction is used outside the loop   unrolled x2   16 instr/iter   loads 0 stores 2 /iter   17.0 cycles/iter   bound: register dependency
nest.fas:36   for pass usize = 0; pass < 3000; pass +… main
      41 instr/iter   loads 4 stores 0 /iter
EXPECTED

cat >"$WORK/nest-no-inline.expected" <<'EXPECTED'
nest.fas  static (no run)
nest.fas:9    for r usize = 0; r < rows; r += 1 {      tile, inlined into main:37
      27 instr/iter   loads 4 stores 0 /iter
nest.fas:10   for c usize = 0; c < cols; c += 1 {      tile, inlined into main:37
      vectorized x2 interleave 2   unrolled x2   15 instr/iter (4 elements)   loads 4 stores 0 /iter   3.9 cycles/iter
nest.fas:23   for i u64 = 0; i < n; i += 1 {           divide, inlined into main:40
      not vectorized: Cannot vectorize early exit loop with reductions or recurrences   loads 0 stores 0 /iter   trap check
      in main:40 (copy 1): 19 instr/iter   41.3 cycles/iter   bound: register dependency
      in main:40 (copy 2): 20 instr/iter
nest.fas:31   for i u64 = 0; i < 65536; i += 1 {       main
      not vectorized: value that could not be identified as reduction is used outside the loop   unrolled x2   16 instr/iter   loads 0 stores 2 /iter   17.0 cycles/iter   bound: register dependency
nest.fas:36   for pass usize = 0; pass < 3000; pass +… main
      40 instr/iter   loads 4 stores 0 /iter   call kept: helper (it should never be inlined (cost=never): noinline function attribute)
EXPECTED

cat >"$WORK/flat.expected" <<'EXPECTED'
flat.fas  static (no run)
no loops in Fas code
EXPECTED
cat >"$WORK/loop_climb.expected" <<'EXPECTED'
loop_climb.fas  static (no run)
loop_climb.fas:7 while ip > 0 {                           put, inlined into main:25
      not vectorized: value that could not be identified as reduction is used outside the loop   15 instr/iter   loads 0 stores 1 /iter   8.0 cycles/iter   bound: register dependency
loop_climb.fas:12 while n > 0 {                            put, inlined into main:25
      not vectorized: value that could not be identified as reduction is used outside the loop   unrolled x4
      in main:25 (copy 1): 6 instr/iter   loads 1 stores 1 /iter   1.6 cycles/iter
      in main:25 (copy 2): 11 instr/iter   loads 4 stores 4 /iter   4.1 cycles/iter   bound: ZnAGU0
loop_climb.fas:24 for r u64 = 0; r < 3000; r += 1 {        main
      in main (copy 1): 11 instr/iter   loads 1 stores 3 /iter   2.8 cycles/iter
      in main (copy 2): 34 instr/iter   loads 1 stores 4 /iter
      in main (copy 3): 49 instr/iter   loads 2 stores 5 /iter
      in main (copy 4): 52 instr/iter   loads 2 stores 5 /iter
      in main (copy 5): 65 instr/iter   loads 6 stores 9 /iter
EXPECTED
cat >"$WORK/loop_siblings.expected" <<'EXPECTED'
loop_siblings.fas  static (no run)
loop_siblings.fas:3 while v > 1000 {                         fold, inlined into main:20
      not vectorized: value that could not be identified as reduction is used outside the loop   6 instr/iter   loads 0 stores 0 /iter   7.0 cycles/iter   bound: register dependency
loop_siblings.fas:19 for r u64 = 0; r < 3000; r += 1 {        main
      loads 0 stores 0 /iter
      in main (copy 1): 37 instr/iter
      in main (copy 2): 27 instr/iter
EXPECTED
cat >"$WORK/loop_continue.expected" <<'EXPECTED'
loop_continue.fas  static (no run)
loop_continue.fas:4 while v < limit {                        scan, inlined into main:17
      not vectorized: Cannot vectorize early exit loop   loads 0 stores 0 /iter
      in main:17 (copy 1): 9 instr/iter
      in main:17 (copy 2): 13 instr/iter
loop_continue.fas:16 for r u64 = 0; r < 3000; r += 1 {        main
      19 instr/iter   loads 0 stores 0 /iter   4.9 cycles/iter
EXPECTED

strip_native() {
  sed -E -e '/^native target /d' -e '/^    change: /s/   native: ([^ ]| [^ ])*//' -e '/^ *native: loop shape differs$/d' -e 's/   native: vectorized x[0-9]+( interleave [0-9]+)?, loop shape differs//' -e 's/   native: ((vectorized x[0-9]+( interleave [0-9]+)?|not vectorized) \([^)]*\), )?([0-9.]+ cycles\/iter \([^)]*\)|loop shape differs)//' "$1" >"$1.portable"
  mv "$1.portable" "$1"
}

run_perf() {
  status=0
  "$OCAML_FAS" perf "$@" >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
  strip_native "$WORK/stderr"
  if grep -q '__fas_' "$WORK/stdout" "$WORK/stderr"; then
    fail "perf output shows an adapter symbol: $*"
  fi
}

expect_exit() {
  [ "$status" -eq "$1" ] || fail "$2: expected exit $1, got $status: $(cat "$WORK/stderr")"
}

expect_empty_temp() {
  [ -z "$(ls -A "$TMPDIR")" ] || fail "$1: temporary files left in $TMPDIR"
}

check_json() {
  python3 -c '
import json, sys
doc = json.load(open(sys.argv[1]))
mode = sys.argv[2]
def fail(message):
    sys.exit("json check: " + message)
base_top = ["file", "format", "gone", "rows", "run", "version"]
if sorted(doc) == base_top:
    native_summary = None
elif sorted(doc) == sorted(base_top + ["native_summary"]):
    native_summary = doc["native_summary"]
else:
    fail("top-level keys")
if doc["format"] != "fas-perf" or doc["version"] != 1:
    fail("format or version")
if (mode == "run") != (doc["run"] is not None):
    fail("run presence does not match the mode")
if doc["run"] is not None:
    if sorted(doc["run"]) != ["branch_misses", "branches", "cache_misses", "cycles", "exit", "instructions", "samples", "seconds"]:
        fail("run keys")
    if doc["run"]["samples"] < 1:
        fail("run has no samples")
row_keys = ["change", "copies", "dso", "facts", "file", "function", "inlined_into", "kind", "line", "places", "share", "shown", "source", "symbol"]
if mode == "run":
    row_keys = sorted(row_keys + ["branch_miss_rate", "cache_miss_per_kilo"])
native_row_keys = sorted(row_keys + ["native_cycles_per_iter", "native_elements", "native_shape_differs"])
pairs = 0
fact_keys = ["bound", "calls", "calls_kept", "cycles_per_iter", "instrs", "loads", "not_vectorized", "reloads", "spills", "stores", "trap", "unroll", "vectorized"]
shown_keys = sorted(["bound", "call_kept", "calls", "cycles", "instrs", "loads", "native", "reloads", "spills", "stores", "trap", "unroll", "vector"])
if not isinstance(doc["gone"], list) or not all(isinstance(text, str) for text in doc["gone"]):
    fail("gone is not a list of strings")
for row in doc["rows"]:
    if sorted(row) == native_row_keys:
        if not isinstance(row["native_shape_differs"], bool):
            fail("native_shape_differs not a boolean")
        cycles = row["native_cycles_per_iter"]
        if cycles is not None and not isinstance(cycles, (int, float)):
            fail("native_cycles_per_iter not a number")
        elements = row["native_elements"]
        if elements is not None and not isinstance(elements, int):
            fail("native_elements not an integer")
        if row["native_shape_differs"] and cycles is not None:
            fail("native fields set on a differing loop shape")
        portable = row["facts"]["cycles_per_iter"]
        if cycles is not None and portable is not None and portable > 0:
            pairs += 1
    elif sorted(row) != row_keys:
        fail("row keys")
    if row["kind"] not in ["function", "loop", "outside", "rest"]:
        fail("kind " + str(row["kind"]))
    change = row["change"]
    if change is not None and change != "new":
        if not isinstance(change, dict) or not change or any(not isinstance(pair, list) or len(pair) != 2 for pair in change.values()):
            fail("change is neither null, new, nor a field map")
    share = row["share"]
    if (share is None) != (doc["run"] is None):
        fail("share presence")
    if share is not None and not 0 <= share <= 1:
        fail("share outside 0..1")
    if mode == "run":
        rate = row["branch_miss_rate"]
        if rate is not None and not 0 <= rate <= 1:
            fail("branch_miss_rate outside 0..1")
        cache = row["cache_miss_per_kilo"]
        if cache is not None and cache < 0:
            fail("cache_miss_per_kilo below zero")
    if row["facts"] is None:
        if row["shown"] is not None:
            fail("shown without facts")
    else:
        if sorted(row["shown"]) != shown_keys or not all(isinstance(pair, list) and len(pair) == 2 and all(isinstance(text, str) for text in pair) for pair in row["shown"].values()):
            fail("shown keys or values")
        if sorted(row["facts"]) != fact_keys:
            fail("fact keys")
        if row["facts"]["vectorized"] is not None and sorted(row["facts"]["vectorized"]) != ["factor", "interleave"]:
            fail("vectorized keys")
        if (row["facts"]["spills"] is None) != (row["facts"]["reloads"] is None):
            fail("spill and reload presence differs")
        if not isinstance(row["facts"]["loads"], int) or not isinstance(row["facts"]["stores"], int):
            fail("loads or stores not an integer")
        for call in row["facts"]["calls_kept"]:
            if sorted(call) != ["callee", "reason"]:
                fail("calls_kept keys")
if (native_summary is not None) != (pairs > 0):
    fail("native summary presence")
if native_summary is not None and sorted(native_summary) != ["faster", "same", "slower"]:
    fail("native summary keys")
if native_summary is not None and sum(native_summary.values()) != pairs:
    fail("native summary total")
' "$1" "$2" || fail "$3: JSON schema check failed"
}

run_perf "$WORK/nest.fas" --static
expect_exit 0 "static nest"
[ ! -s "$WORK/stdout" ] || fail "static nest wrote to stdout"
diff "$WORK/nest-default.expected" "$WORK/stderr" >/dev/null ||
  fail "static nest output differs: $(diff "$WORK/nest-default.expected" "$WORK/stderr")"
grep -q '^      vectorized x2 interleave 2   unrolled x2   15 instr/iter (4 elements)   loads 4 stores 0 /iter   3.9 cycles/iter$' "$WORK/stderr" ||
  fail "innermost static loop row has no cycles/iter"
expect_empty_temp "static nest"

run_perf "$WORK/nest.fas" --static --no-inline helper
expect_exit 0 "no-inline helper"
diff "$WORK/nest-no-inline.expected" "$WORK/stderr" >/dev/null ||
  fail "no-inline output differs: $(diff "$WORK/nest-no-inline.expected" "$WORK/stderr")"

status=0
FAS_MCA=/nonexistent "$OCAML_FAS" perf "$WORK/nest.fas" --static >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
expect_exit 0 "missing mca"
[ ! -s "$WORK/stdout" ] || fail "missing mca wrote to stdout"
grep -qx '      27 instr/iter   loads 4 stores 0 /iter' "$WORK/stderr" || fail "missing mca changed the loop row: $(cat "$WORK/stderr")"
! grep -q 'cycles/iter\|bound:' "$WORK/stderr" || fail "missing mca still shows cycle facts"
! grep -q 'native' "$WORK/stderr" || fail "missing mca still shows native facts"
expect_empty_temp "missing mca"
expect_empty_temp "no-inline helper"

MCA_BIN=$(printenv FAS_MCA || printf 'llvm-mca-22')
if command -v "$MCA_BIN" >/dev/null 2>&1; then
  status=0
  FAS_MCA="$MCA_BIN" "$OCAML_FAS" perf "$WORK/nest.fas" --static >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
  expect_exit 0 "native static"
  [ ! -s "$WORK/stdout" ] || fail "native static wrote to stdout"
  python3 -I -c '
import re, sys
lines = open(sys.argv[1], encoding="utf-8").read().splitlines()
match = re.fullmatch(r"native target \(predicted by llvm-mca\): faster on (\d+) loops, slower on (\d+), same on (\d+)", lines[1])
if match is None:
    sys.exit("native header missing: " + lines[1])
body = "\n".join(lines[2:])
if body.count("(−") != int(match.group(1)) or body.count("(+") != int(match.group(2)):
    sys.exit("native tokens do not match the header")
' "$WORK/stderr" || fail "native static header and rows disagree: $(cat "$WORK/stderr")"
  expect_empty_temp "native static"

  status=0
  FAS_MCA="$MCA_BIN" "$OCAML_FAS" perf "$WORK/nest.fas" --static --json >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
  expect_exit 0 "native json static"
  [ ! -s "$WORK/stderr" ] || fail "native json static wrote to stderr: $(cat "$WORK/stderr")"
  check_json "$WORK/stdout" static "native json static"
  expect_empty_temp "native json static"

  if grep -qw avx2 /proc/cpuinfo; then
    cat >"$WORK/avx_sum.fas" <<'FAS'
var data arr[4096, u32] = {}

fn sum_words(n usize) u32 {
    total u32 = 0
    for i usize = 0; i < n; i += 1 {
        total += data[i]
    }
    return total
}

fn main() i32 {
    for i usize = 0; i < 4096; i += 1 {
        data[i] = trunc[u32](i)
    }
    if sum_words(4096) == 0 {
        return 1
    }
    return 0
}
FAS
    status=0
    FAS_MCA="$MCA_BIN" "$OCAML_FAS" perf "$WORK/avx_sum.fas" --static >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
    expect_exit 0 "native avx2 static"
    sed -n '/^avx_sum\.fas:5 /{n;p;}' "$WORK/stderr" | grep -Eq 'vectorized x4 interleave 2   unrolled x4   [0-9]+ instr/iter \(8 elements\)   loads [0-9]+ stores 0 /iter   [0-9.]+ cycles/iter   native: [0-9.]+ cycles/iter \(−[0-9]+%\)$' ||
      fail "native avx2 loop is not predicted faster at equal element counts: $(cat "$WORK/stderr")"
    expect_empty_temp "native avx2 static"

    cat >"$WORK/shape_store.fas" <<'FAS'
struct Cell {
    a u32
    b u32
}

var cells arr[4096, Cell] = {}

fn fill(src addr, n usize) u32 {
    for i usize = 0; i < n; i += 1 {
        cells[i].a = src[u32, i]
    }
    return cells[0].a
}

fn main(argc i32, argv addr) i32 {
    src arr[4096, u32] = {}
    for i usize = 0; i < 4096; i += 1 {
        src[i] = trunc[u32](i)
    }
    n usize = 4000 + bitcast[usize](sext[i64](argc))
    if fill(&src, n) == 7 {
        return 1
    }
    return 0
}
FAS
    status=0
    FAS_MCA="$MCA_BIN" "$OCAML_FAS" perf "$WORK/shape_store.fas" --no-inline fill --static >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
    expect_exit 0 "native avx2 shape differs static"
    shape_block=$(awk '/^shape_store\.fas:9 /{p=1; print; next} p && /^[^ ]/{exit} p{print}' "$WORK/stderr")
    printf '%s\n' "$shape_block" | grep -qx '      unrolled x4   native: vectorized x8, loop shape differs' ||
      fail "native avx2 shape row lacks the vector form: $shape_block"
    printf '%s\n' "$shape_block" | grep -q '^      in fill (copy 2): ' ||
      fail "portable shape_store loop has no remainder copy: $shape_block"
    if printf '%s\n' "$shape_block" | sed 's/   native: .*//' | grep -q vectorized; then
      fail "portable shape_store loop is vectorized: $shape_block"
    fi
    expect_empty_temp "native avx2 shape differs static"
  else
    echo "perf_command: native avx2 check skipped: no avx2 in /proc/cpuinfo" >&2
  fi
  expect_empty_temp "native section"
else
  echo "perf_command: native section skipped: $MCA_BIN not found" >&2
fi

run_perf "$WORK/flat.fas" --static
expect_exit 0 "flat"
diff "$WORK/flat.expected" "$WORK/stderr" >/dev/null ||
  fail "flat output differs: $(diff "$WORK/flat.expected" "$WORK/stderr")"

run_perf "$ROOT/test/loop_climb.fas" --static
expect_exit 0 "loop climb static"
diff "$WORK/loop_climb.expected" "$WORK/stderr" >/dev/null ||
  fail "loop climb static output differs: $(diff "$WORK/loop_climb.expected" "$WORK/stderr")"
expect_empty_temp "loop climb static"

run_perf "$ROOT/test/loop_siblings.fas" --static
expect_exit 0 "loop siblings static"
diff "$WORK/loop_siblings.expected" "$WORK/stderr" >/dev/null ||
  fail "loop siblings static output differs: $(diff "$WORK/loop_siblings.expected" "$WORK/stderr")"
expect_empty_temp "loop siblings static"

run_perf "$ROOT/test/loop_continue.fas" --static
expect_exit 0 "loop continue static"
diff "$WORK/loop_continue.expected" "$WORK/stderr" >/dev/null ||
  fail "loop continue static output differs: $(diff "$WORK/loop_continue.expected" "$WORK/stderr")"
expect_empty_temp "loop continue static"

cat >"$WORK/objdump-record" <<RECORD
#!/bin/sh
printf 'objdump %s\n' "\$*" >>"$WORK/tools.log"
exec "$OBJDUMP_BIN" "\$@"
RECORD
cat >"$WORK/symbolizer-record" <<RECORD
#!/bin/sh
printf 'symbolizer %s\n' "\$*" >>"$WORK/tools.log"
exec "$SYMBOLIZER_BIN" "\$@"
RECORD
chmod +x "$WORK/objdump-record" "$WORK/symbolizer-record"
status=0
FAS_OBJDUMP="$WORK/objdump-record" FAS_SYMBOLIZER="$WORK/symbolizer-record" \
  "$OCAML_FAS" perf "$ROOT/test/loop_climb.fas" --static >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
expect_exit 0 "recorded debuginfod tools"
strip_native "$WORK/stderr"
diff "$WORK/loop_climb.expected" "$WORK/stderr" >/dev/null ||
  fail "recorded tools changed the output"
grep -q '^objdump ' "$WORK/tools.log" && grep -q '^symbolizer ' "$WORK/tools.log" ||
  fail "objdump or symbolizer was not recorded"
! grep -qv -- '--no-debuginfod' "$WORK/tools.log" ||
  fail "tool call without --no-debuginfod: $(grep -v -- '--no-debuginfod' "$WORK/tools.log" | head -n 1)"
expect_empty_temp "recorded debuginfod tools"
cat >"$WORK/perf-record" <<RECORD
#!/bin/sh
printf 'perf %s DEBUGINFOD_URLS=%s\n' "\$1" "\${DEBUGINFOD_URLS-unset}" >>"$WORK/perf.log"
exit 0
RECORD
chmod +x "$WORK/perf-record"
status=0
DEBUGINFOD_URLS=https://debuginfod.invalid FAS_PERF="$WORK/perf-record" \
  "$OCAML_FAS" perf "$ROOT/test/loop_climb.fas" >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
expect_exit 1 "recorded perf environment"
grep -q '^fas perf: no run: perf recorded no cycle samples' "$WORK/stderr" || fail "recorded perf message"
grep -qx 'perf record DEBUGINFOD_URLS=' "$WORK/perf.log" ||
  fail "perf record without empty debuginfod: $(cat "$WORK/perf.log")"
expect_empty_temp "recorded perf environment"

run_perf "$WORK/perf_c_names.fas" --static
expect_exit 0 "c names static"
diff "$WORK/perf_c_names.expected" "$WORK/stderr" >/dev/null ||
  fail "c names static output differs: $(diff "$WORK/perf_c_names.expected" "$WORK/stderr")"
expect_empty_temp "c names static"

run_perf "$WORK/perf_c_names.fas" --static --asm "$WORK/perf_c_names.fas:8"
expect_exit 0 "c names asm"
grep -q '^ *callq c_step  *perf_c_names\.fas:9$' "$WORK/stderr" ||
  fail "c names asm has no callq c_step: $(cat "$WORK/stderr")"
expect_empty_temp "c names asm"

run_perf "$WORK/perf_c_names.fas" --static --json
expect_exit 0 "c names json"
check_json "$WORK/stdout" static "c names json"
python3 -c 'import json, sys; doc = json.load(open(sys.argv[1])); callees = [c["callee"] for row in doc["rows"] if row["facts"] for c in row["facts"]["calls_kept"]]; sys.exit(0 if callees == ["c_step"] else 1)' "$WORK/stdout" ||
  fail "c names json callee is not c_step"
expect_empty_temp "c names json"

run_perf "$WORK/perf_generic_names.fas" --static
expect_exit 0 "generic names static"
diff "$WORK/perf_generic_names.expected" "$WORK/stderr" >/dev/null ||
  fail "generic names static output differs: $(diff "$WORK/perf_generic_names.expected" "$WORK/stderr")"
expect_empty_temp "generic names static"

run_perf "$WORK/perf_generic_names.fas" --static --json
expect_exit 0 "generic names json"
python3 -c 'import json, sys; doc = json.load(open(sys.argv[1])); rows = doc["rows"]; sys.exit(0 if len(rows) == 1 and [c["function"] for c in rows[0]["copies"]] == ["sum[u32]", "sum[u64]"] else 1)' "$WORK/stdout" ||
  fail "generic names json copies are not sum[u32] and sum[u64]"
expect_empty_temp "generic names json"
cat >"$WORK/perf_generic_names.json.expected" <<'EXPECTED'
{"format":"fas-perf","version":1,"file":"perf_generic_names.fas","run":null,"rows":[{"share":null,"kind":"loop","file":"perf_generic_names.fas","line":5,"source":"for i usize = 0; i < n; i += 1 {","function":"sum[u32]","inlined_into":{"function":"main","line":12},"symbol":null,"dso":null,"places":null,"facts":{"instrs":2,"loads":0,"stores":0,"spills":null,"reloads":null,"vectorized":null,"not_vectorized":null,"unroll":"unrolled x16","cycles_per_iter":1.04,"bound":"register dependency","calls":[],"calls_kept":[],"trap":false},"shown":{"vector":["","not vectorized"],"unroll":["","unrolled x16"],"instrs":["instr/iter","2"],"loads":["loads","0"],"stores":["stores","0"],"spills":["spills","none"],"reloads":["reloads","none"],"cycles":["cycles/iter","1.0"],"native":["native:","none"],"bound":["bound:","register dependency"],"calls":["calls:","none"],"call_kept":["call kept:","none"],"trap":["","none"]},"copies":[{"share":null,"kind":"loop","file":"perf_generic_names.fas","line":5,"source":"for i usize = 0; i < n; i += 1 {","function":"sum[u32]","inlined_into":{"function":"main","line":12},"symbol":null,"dso":null,"places":null,"facts":{"instrs":2,"loads":0,"stores":0,"spills":null,"reloads":null,"vectorized":null,"not_vectorized":null,"unroll":"unrolled x16","cycles_per_iter":1.04,"bound":"register dependency","calls":[],"calls_kept":[],"trap":false},"shown":{"vector":["","not vectorized"],"unroll":["","unrolled x16"],"instrs":["instr/iter","2"],"loads":["loads","0"],"stores":["stores","0"],"spills":["spills","none"],"reloads":["reloads","none"],"cycles":["cycles/iter","1.0"],"native":["native:","none"],"bound":["bound:","register dependency"],"calls":["calls:","none"],"call_kept":["call kept:","none"],"trap":["","none"]}},{"share":null,"kind":"loop","file":"perf_generic_names.fas","line":5,"source":"for i usize = 0; i < n; i += 1 {","function":"sum[u64]","inlined_into":{"function":"main","line":13},"symbol":null,"dso":null,"places":null,"facts":{"instrs":2,"loads":0,"stores":0,"spills":null,"reloads":null,"vectorized":null,"not_vectorized":null,"unroll":"unrolled x16","cycles_per_iter":1.04,"bound":"register dependency","calls":[],"calls_kept":[],"trap":false},"shown":{"vector":["","not vectorized"],"unroll":["","unrolled x16"],"instrs":["instr/iter","2"],"loads":["loads","0"],"stores":["stores","0"],"spills":["spills","none"],"reloads":["reloads","none"],"cycles":["cycles/iter","1.0"],"native":["native:","none"],"bound":["bound:","register dependency"],"calls":["calls:","none"],"call_kept":["call kept:","none"],"trap":["","none"]}}],"change":null}],"gone":[]}
EXPECTED
diff "$WORK/perf_generic_names.json.expected" "$WORK/stdout" >/dev/null ||
  fail "generic names json bytes differ: $(diff "$WORK/perf_generic_names.json.expected" "$WORK/stdout")"

cat >"$WORK/identical_copies.fas" <<'FAS'
var buf arr[65536, u32] = {}

fn sum[T](data addr, n usize) T {
    total T = 0
    for i usize = 0; i < n; i += 1 {
        total += data[T, i]
    }
    return total
}

fn main() i32 {
    a u32 = sum[u32](&buf, 65536)
    b u32 = sum[u32](&buf, 32768)
    return trunc[i32](zext[u64](a) + zext[u64](b))
}
FAS
cat >"$WORK/identical_copies.expected" <<'EXPECTED'
identical_copies.fas  static (no run)
identical_copies.fas:5 for i usize = 0; i < n; i += 1 {         sum[u32], inlined into main:12, main:13
      vectorized x4 interleave 2   unrolled x16   2 instr/iter (8 elements)   loads 0 stores 0 /iter   1.0 cycles/iter   bound: register dependency
EXPECTED
run_perf "$WORK/identical_copies.fas" --static
expect_exit 0 "identical copies static"
diff "$WORK/identical_copies.expected" "$WORK/stderr" >/dev/null ||
  fail "identical copies static output differs: $(diff "$WORK/identical_copies.expected" "$WORK/stderr")"
expect_empty_temp "identical copies static"
cat >"$WORK/spill_counts.fas" <<'FAS'
fn step(x u64) u64 {
    return x ^ (x >> 7)
}

fn main() i32 {
    a0 u64 = 1
    a1 u64 = 2
    a2 u64 = 3
    a3 u64 = 4
    a4 u64 = 5
    a5 u64 = 6
    a6 u64 = 7
    a7 u64 = 8
    a8 u64 = 9
    a9 u64 = 10
    a10 u64 = 11
    a11 u64 = 12
    a12 u64 = 13
    a13 u64 = 14
    a14 u64 = 15
    a15 u64 = 16
    a16 u64 = 17
    a17 u64 = 18
    a18 u64 = 19
    a19 u64 = 20
    for i u64 = 0; i < 100000; i += 1 {
        a0 = a0 * 31 + a1 + i
        a1 = a1 * 31 + a2 + i
        a2 = a2 * 31 + a3 + i
        a3 = a3 * 31 + a4 + i
        a4 = a4 * 31 + a5 + i
        a5 = a5 * 31 + a6 + i
        a6 = a6 * 31 + a7 + i
        a7 = a7 * 31 + a8 + i
        a8 = a8 * 31 + a9 + i
        a9 = a9 * 31 + a10 + i
        a10 = a10 * 31 + a11 + i
        a11 = a11 * 31 + a12 + i
        a12 = a12 * 31 + a13 + i
        a13 = a13 * 31 + a14 + i
        a14 = a14 * 31 + a15 + i
        a15 = a15 * 31 + a16 + i
        a16 = a16 * 31 + a17 + i
        a17 = a17 * 31 + a18 + i
        a18 = a18 * 31 + a19 + i
        a19 = a19 * 31 + a0 + i
    }
    return trunc[i32](step(a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19) & 127)
}
FAS
cat >"$WORK/spill_counts.expected" <<'EXPECTED'
spill_counts.fas  static (no run)
spill_counts.fas:26 for i u64 = 0; i < 100000; i += 1 {      main
      not vectorized: value that could not be identified as reduction is used outside the loop   172 instr/iter   loads 37 stores 39 /iter   spills 39 reloads 37   43.1 cycles/iter
EXPECTED
run_perf "$WORK/spill_counts.fas" --static
expect_exit 0 "spill counts static"
diff "$WORK/spill_counts.expected" "$WORK/stderr" >/dev/null ||
  fail "spill counts static output differs: $(diff "$WORK/spill_counts.expected" "$WORK/stderr")"
expect_empty_temp "spill counts static"
cat >"$WORK/sum_counts.fas" <<'FAS'
var buf arr[65536, u32] = {}

fn main() i32 {
    seed u32 = 1
    for i usize = 0; i < 65536; i += 1 {
        seed = seed * 1664525 + 1013904223
        buf[i] = seed
    }
    total u64 = 0
    for i usize = 0; i < 65536; i += 1 {
        total += zext[u64](buf[i])
    }
    return trunc[i32](total & 127)
}
FAS
cat >"$WORK/sum_counts.expected" <<'EXPECTED'
sum_counts.fas  static (no run)
sum_counts.fas:5 for i usize = 0; i < 65536; i += 1 {     main
      not vectorized: value that could not be identified as reduction is used outside the loop   unrolled x4   15 instr/iter   loads 0 stores 4 /iter   7.0 cycles/iter   bound: register dependency
sum_counts.fas:10 for i usize = 0; i < 65536; i += 1 {     main
      vectorized x4 interleave 2   unrolled x4   19 instr/iter (8 elements)   loads 8 stores 0 /iter   4.9 cycles/iter
EXPECTED
run_perf "$WORK/sum_counts.fas" --static
expect_exit 0 "sum counts static"
diff "$WORK/sum_counts.expected" "$WORK/stderr" >/dev/null ||
  fail "sum counts static output differs: $(diff "$WORK/sum_counts.expected" "$WORK/stderr")"
expect_empty_temp "sum counts static"
cat >"$WORK/rmw_counts.fas" <<'FAS'
var buf arr[65536, u32] = {}

fn main() i32 {
    for i usize = 0; i < 65535; i += 1 {
        buf[i + 1] += buf[i]
    }
    return trunc[i32](zext[u64](buf[65535]) & 127)
}
FAS
cat >"$WORK/rmw_counts.expected" <<'EXPECTED'
rmw_counts.fas  static (no run)
rmw_counts.fas:4 for i usize = 0; i < 65535; i += 1 {     main
      unrolled x5   13 instr/iter   loads 5 stores 5 /iter   5.1 cycles/iter
EXPECTED
run_perf "$WORK/rmw_counts.fas" --static
expect_exit 0 "rmw counts static"
diff "$WORK/rmw_counts.expected" "$WORK/stderr" >/dev/null ||
  fail "rmw counts static output differs: $(diff "$WORK/rmw_counts.expected" "$WORK/stderr")"
expect_empty_temp "rmw counts static"

cat >"$WORK/multi_caller.fas" <<'FAS'
var buf arr[65536, u32] = {}

fn scan(data addr, n usize) u64 {
    total u64 = 0
    for i usize = 0; i < n; i += 1 {
        total += zext[u64](data[u32, i])
    }
    return total
}

fn main() i32 {
    sum u64 = 0
    for pass usize = 0; pass < 3000; pass += 1 {
        sum += scan(&buf, pass + 65536)
        sum += scan(&buf, pass + 4096)
        sum += scan(&buf, pass + 1024)
        sum += scan(&buf, pass + 256)
        sum += scan(&buf, pass + 64)
    }
    return trunc[i32](sum & 127)
}
FAS

cat >"$WORK/repeated_call.fas" <<'FAS'
use "C" <stdio.h>

fn main() i32 {
    for i i32 = 0; i < 1000; i += 1 {
        putchar(i & 63)
        putchar(i & 31)
        putchar(i & 15)
    }
    return 0
}
FAS

run_perf "$WORK/multi_caller.fas" --static
expect_exit 0 "multi caller static"
[ "$(grep -c '^multi_caller\.fas:5 ' "$WORK/stderr")" -eq 1 ] ||
  fail "multi caller loop is not one row: $(cat "$WORK/stderr")"
grep -qF 'scan, inlined into main:14, main:15 (+3 more)' "$WORK/stderr" ||
  fail "multi caller owner has no (+3 more): $(cat "$WORK/stderr")"
[ "$(grep -c '^      in ' "$WORK/stderr")" -ge 2 ] ||
  fail "multi caller copies with different facts share one line: $(cat "$WORK/stderr")"
expect_empty_temp "multi caller static"

run_perf "$WORK/multi_caller.fas" --static --json
expect_exit 0 "multi caller json"
check_json "$WORK/stdout" static "multi caller json"
python3 -c 'import json, sys; rows = [r for r in json.load(open(sys.argv[1]))["rows"] if r["line"] == 5]; sys.exit(0 if len(rows) == 1 and len(rows[0]["copies"]) >= 4 and all("copies" not in c for c in rows[0]["copies"]) else 1)' "$WORK/stdout" ||
  fail "multi caller json copies"
expect_empty_temp "multi caller json"

run_perf "$WORK/multi_caller.fas" --static --asm "$WORK/multi_caller.fas:5@2"
expect_exit 0 "multi caller copy 2"
grep -Eq 'copy 2 of [0-9]+$' "$WORK/stderr" || fail "multi caller copy 2 has no copy label: $(head -n 1 "$WORK/stderr")"
expect_empty_temp "multi caller copy 2"

run_perf "$WORK/multi_caller.fas" --static --asm "$WORK/multi_caller.fas:5@99"
expect_exit 2 "multi caller copy out of range"
grep -Eq 'multi_caller\.fas:5 has [0-9]+ copies; use @1\.\.@[0-9]+$' "$WORK/stderr" ||
  fail "multi caller copy out of range message: $(cat "$WORK/stderr")"
expect_empty_temp "multi caller copy out of range"

run_perf "$WORK/repeated_call.fas" --static
expect_exit 0 "repeated call static"
grep -qF 'calls: putchar@plt ×3' "$WORK/stderr" || fail "repeated call is not deduplicated: $(cat "$WORK/stderr")"
expect_empty_temp "repeated call static"

for fixture in perf_c_names perf_generic_names; do
  "$OCAML_FAS" -O2 -c "$WORK/$fixture.fas" -o "$WORK/$fixture.plain.o" ||
    fail "$fixture plain build failed"
  "$OCAML_FAS" -g -O2 -c "$WORK/$fixture.fas" -o "$WORK/$fixture.debug.o" ||
    fail "$fixture debug build failed"
  "$LLVM_OBJCOPY" --dump-section .text="$WORK/$fixture.plain.text" "$WORK/$fixture.plain.o"
  "$LLVM_OBJCOPY" --dump-section .text="$WORK/$fixture.debug.text" "$WORK/$fixture.debug.o"
  cmp -s "$WORK/$fixture.plain.text" "$WORK/$fixture.debug.text" ||
    fail "$fixture: .text differs with -g"
done

run_perf "$WORK/bad.fas" --static
expect_exit 1 "bad source"
grep -q 'error' "$WORK/stderr" || fail "bad source printed no diagnostic"
expect_empty_temp "bad source"

run_perf "$WORK/nest.fas" --static --json
expect_exit 0 "json static"
[ ! -s "$WORK/stderr" ] || fail "json static wrote to stderr: $(cat "$WORK/stderr")"
check_json "$WORK/stdout" static "json static"
expect_empty_temp "json static"

run_perf "$WORK/nest.fas" --static --asm "$WORK/nest.fas:9"
expect_exit 0 "asm known line"
[ ! -s "$WORK/stdout" ] || fail "asm known line wrote to stdout"
grep -q '^L[0-9][0-9]*:$' "$WORK/stderr" || fail "asm listing has no label line"
grep -q 'nest\.fas:9$' "$WORK/stderr" || fail "asm listing has no instruction at nest.fas:9"
expect_empty_temp "asm known line"

run_perf "$WORK/nest.fas" --static --asm "$WORK/nest.fas:999"
expect_exit 2 "asm unknown line"
grep -q 'no loop at .*nest\.fas:999; loops: nest\.fas:9, nest\.fas:10, nest\.fas:23, nest\.fas:31, nest\.fas:36$' "$WORK/stderr" ||
  fail "asm unknown line message: $(cat "$WORK/stderr")"
expect_empty_temp "asm unknown line"

run_perf "$WORK/nest.fas" --static --json --asm "$WORK/nest.fas:9"
expect_exit 2 "json with asm"
grep -q -- '--json and --asm cannot be combined' "$WORK/stderr" || fail "json with asm message"
expect_empty_temp "json with asm"

run_perf "$WORK/nest.fas" --static --top 0
expect_exit 2 "top zero"
grep -q -- '--top requires a number from 1 to 50' "$WORK/stderr" || fail "top message"

run_perf "$WORK/nest.fas" --static -- 1
expect_exit 2 "program arguments"
grep -q -- '`--` arguments are not accepted' "$WORK/stderr" || fail "argument message"


status=0
FAS_PERF=/nonexistent "$OCAML_FAS" perf "$WORK/flat.fas" >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
expect_exit 1 "missing perf"
grep -q '^flat.fas  static (no run)$' "$WORK/stderr" || fail "missing perf static report"
grep -q '^fas perf: no run: cannot run /nonexistent: ' "$WORK/stderr" || fail "missing perf message"
expect_empty_temp "missing perf"

status=0
FAS_PERF=false "$OCAML_FAS" perf "$WORK/flat.fas" >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
expect_exit 1 "false perf"
grep -q '^fas perf: no run: perf recorded no cycle samples' "$WORK/stderr" || fail "false perf message"
expect_empty_temp "false perf"

cat >"$WORK/hot_c.c" <<'C'
#include <stdint.h>
uint64_t c_scan(const uint32_t *data, uint64_t n) {
    uint64_t total = 0;
    for (uint64_t i = 0; i < n; i += 1) {
        if (data[i] % 3 == 1) {
            total += data[i];
        }
    }
    return total;
}
C
for level in 0 2; do
  "$CC" -O$level -c "$WORK/hot_c.c" -o "$WORK/hot_c.O$level.plain.o"
  "$CC" -O$level -g -c "$WORK/hot_c.c" -o "$WORK/hot_c.O$level.debug.o"
  "$LLVM_OBJCOPY" --dump-section .text="$WORK/hot_c.O$level.plain.text" "$WORK/hot_c.O$level.plain.o"
  "$LLVM_OBJCOPY" --dump-section .text="$WORK/hot_c.O$level.debug.text" "$WORK/hot_c.O$level.debug.o"
  [ -s "$WORK/hot_c.O$level.plain.text" ] || fail "C text is empty at -O$level"
  cmp -s "$WORK/hot_c.O$level.plain.text" "$WORK/hot_c.O$level.debug.text" ||
    fail "-g changes C machine code at -O$level"
done

PERF=$(printenv FAS_PERF || printf 'perf')
if "$PERF" stat true >/dev/null 2>&1; then
  cat >"$WORK/long.fas" <<'FAS'
use "C" <stdio.h>

fn mix(x u64) u64 {
    return (x ^ (x >> 31)) * 0x9E3779B97F4A7C15
}

fn scan(data addr, n usize) u64 {
    total u64 = 0
    for i usize = 0; i < n; i += 1 {
        if data[u32, i] % 3 == 1 {
            total += zext[u64](data[u32, i])
        }
    }
    return total
}

var buf arr[65536, u32] = {}

fn main() i32 {

    seed u64 = 1
    for i u64 = 0; i < 65536; i += 1 {
        seed = mix(seed + i)
        buf[i] = trunc[u32](seed)
    }
    sum u64 = 0
    for pass usize = 0; pass < 30000; pass += 1 {
        sum += scan(&buf, 65536)
    }
    puts(c"perf run done")
    if sum == 1 {
        return 0
    }
    return 3
}
FAS
  cat >"$WORK/copy.fas" <<'FAS'
var src arr[65536, u8] = {}
var dst arr[65536, u8] = {}

fn duplicate(d addr, s addr) void {
    copy(d[arr[65536, u8]], s[arr[65536, u8]])
    return
}

fn main() i32 {
    for pass usize = 0; pass < 20000; pass += 1 {
        duplicate(&dst, &src)
    }
    return zext[i32](dst[0])
}
FAS

  run_perf "$WORK/long.fas"
  expect_exit 3 "long run"
  [ "$(cat "$WORK/stdout")" = 'perf run done' ] || fail "long run stdout: $(cat "$WORK/stdout")"
  head -n 1 "$WORK/stderr" | grep -Eq '^long\.fas  [0-9]+\.[0-9]{2} s  [0-9]+\.[0-9]{2} G cycles  IPC [0-9.]+  branch miss [0-9.]+%  cache miss [0-9.]+/1k instr  [0-9]+ samples  exit 3$' ||
    fail "long run header: $(head -n 1 "$WORK/stderr")"
  sed -n 2p "$WORK/stderr" | awk '{ p = $1; sub("%", "", p); exit !(p + 0 >= 80 && $2 == "long.fas:9") }' ||
    fail "long run first row is not the dominating loop: $(sed -n 2p "$WORK/stderr")"
  expect_empty_temp "long run"

  run_perf "$WORK/copy.fas"
  expect_exit 0 "copy run"
  grep -Eq '^ *[0-9]+%  outside Fas: [^ ]+ \(libc\.so\.6\)$' "$WORK/stderr" || fail "copy run has no libc row: $(cat "$WORK/stderr")"
  expect_empty_temp "copy run"

  run_perf "$WORK/copy.fas" --json
  expect_exit 0 "copy json"
  [ ! -s "$WORK/stderr" ] || fail "copy json wrote to stderr: $(cat "$WORK/stderr")"
  check_json "$WORK/stdout" run "copy json"
  expect_empty_temp "copy json"

  run_perf "$WORK/long.fas" --json
  expect_exit 3 "long json"
  [ "$(head -n 1 "$WORK/stdout")" = 'perf run done' ] || fail "long json: program output missing before the report"
  tail -n +2 "$WORK/stdout" >"$WORK/report.json"
  check_json "$WORK/report.json" run "long json"
  python3 -c 'import json, sys; doc = json.load(open(sys.argv[1])); row = doc["rows"][0]; sys.exit(0 if row["file"] == "long.fas" and row["line"] == 9 and row["share"] >= 0.8 and doc["run"]["exit"] == 3 else 1)' "$WORK/report.json" ||
    fail "long json first row is not the dominating loop"
  expect_empty_temp "long json"

  run_perf "$WORK/long.fas" --asm "$WORK/long.fas:9"
  expect_exit 3 "long asm"
  [ "$(cat "$WORK/stdout")" = 'perf run done' ] || fail "long asm stdout: $(cat "$WORK/stdout")"
  sed -n 2p "$WORK/stderr" | awk '{ p = $1; sub("%", "", p); exit !(p + 0 >= 80 && $2 == "long.fas:9") }' ||
    fail "long asm first row is not the dominating loop: $(sed -n 2p "$WORK/stderr")"
  grep -q 'long\.fas:9$' "$WORK/stderr" || fail "long asm has no instruction at long.fas:9"
  expect_empty_temp "long asm"

  cat >"$WORK/two_hot.fas" <<'FAS'
var buf arr[8192, u32] = {}

fn scan(data addr, n usize) u64 {
    total u64 = 0
    for i usize = 0; i < n; i += 1 {
        total += zext[u64](data[u32, i])
    }
    return total
}

fn main() i32 {
    sum u64 = 0
    for pass usize = 0; pass < 1000000; pass += 1 {
        sum += scan(&buf, 8192 - pass % 4096)
        sum += scan(&buf, 4096 + pass % 4096)
    }
    return trunc[i32](sum & 127)
}
FAS

  run_perf "$WORK/two_hot.fas"
  expect_exit 0 "two hot run"
  [ "$(grep -c 'two_hot\.fas:5 ' "$WORK/stderr")" -eq 1 ] ||
    fail "two hot copies are not one row: $(cat "$WORK/stderr")"
  sed -n 2p "$WORK/stderr" | awk '{ p = $1; sub("%", "", p); exit !(p + 0 >= 80 && $2 == "two_hot.fas:5") }' ||
    fail "two hot row share is below 80 percent: $(sed -n 2p "$WORK/stderr")"
  expect_empty_temp "two hot run"

cat >"$WORK/branch_rates.fas" <<'FAS'
use "C" <<SINK
#include <stdint.h>
static volatile uint64_t sink_value;
void sink(uint64_t x) { sink_value = x; }
SINK

extern "C" {
    fn sink(x u64) void
}

fn noisy(n u64) u64 {
    x u64 = 88172645463325252
    for i u64 = 0; i < n; i += 1 {
        x = x ^ (x << 13)
        x = x ^ (x >> 7)
        x = x ^ (x << 17)
        if (x & 1) == 1 {
            sink(x)
        }
    }
    return x
}

fn quiet(n u64) u64 {
    for i u64 = 0; i < n; i += 1 {
        if (i & 1023) != 1023 {
            sink(i)
        }
    }
    return n
}

fn main() i32 {
    sum u64 = 0
    for pass usize = 0; pass < 100000; pass += 1 {
        sum += noisy(4096)
        sum += quiet(4096)
    }
    return trunc[i32](sum & 127)
}
FAS
  run_perf "$WORK/branch_rates.fas" --json
  check_json "$WORK/stdout" run "branch rates json"
  python3 -c '
import json, sys
rows = json.load(open(sys.argv[1]))["rows"]
def rate(line):
    found = [r for r in rows if r["file"] == "branch_rates.fas" and r["line"] == line and r["kind"] == "loop"]
    if len(found) != 1 or found[0]["branch_miss_rate"] is None:
        sys.exit("no branch miss rate for the loop at line %d" % line)
    return found[0]["branch_miss_rate"]
noisy = rate(13)
quiet = rate(25)
if not noisy > quiet:
    sys.exit("noisy loop rate %g is not above quiet loop rate %g" % (noisy, quiet))
' "$WORK/stdout" || fail "branch miss rate ordering"
  expect_empty_temp "branch rates run"

cat >"$WORK/hot_c.fas" <<'FAS'
extern "C" {
    fn c_scan(data addr, n u64) u64
}

var buf arr[65536, u32] = {}

fn main() i32 {
    sum u64 = 0
    for pass usize = 0; pass < 20000; pass += 1 {
        sum += c_scan(&buf, 65536)
    }
    return trunc[i32](sum & 127)
}
FAS
  run_perf "$WORK/hot_c.fas" "$WORK/hot_c.c" --json
  check_json "$WORK/stdout" run "C input json"
  python3 -c '
import json, sys
row = json.load(open(sys.argv[1]))["rows"][0]
if row["file"] != "hot_c.c" or row["line"] != 4 or row["function"] != "c_scan" or row["share"] < 0.8:
    sys.exit("first row is not the C loop: " + json.dumps(row)[:300])
' "$WORK/stdout" || fail "C input first row"
  expect_empty_temp "C input run"
else
  echo "perf_command: run section skipped: perf stat true failed" >&2
fi

mkdir -p "$WORK/cmp/a" "$WORK/cmp/b" "$WORK/cmp/c" "$WORK/cmp/d"
cat >"$WORK/cmp/a/cmp.fas" <<'FAS'
var buf arr[65536, u32] = {}

fn main() i32 {
    for i u64 = 1; i < 65536; i += 1 {
        buf[i] = buf[i - 1] + 1
    }
    return trunc[i32](zext[u64](buf[65535]) & 127)
}
FAS
cat >"$WORK/cmp/b/cmp.fas" <<'FAS'
var buf arr[65536, u32] = {}

fn main() i32 {
    for i u64 = 1; i < 65536; i += 1 {
        buf[i] = buf[i] + 1
    }
    return trunc[i32](zext[u64](buf[65535]) & 127)
}
FAS
cat >"$WORK/cmp/c/cmp.fas" <<'FAS'
var buf arr[65536, u32] = {}

fn main() i32 {
    for i u64 = 1; i < 65536; i += 1 {
        buf[i] = buf[i - 1] + 1
    }
    for j u64 = 1; j < 65536; j += 1 {
        buf[j] = buf[j] + 1
    }
    return trunc[i32](zext[u64](buf[65535]) & 127)
}
FAS

run_perf "$WORK/cmp/a/cmp.fas" --static --json
expect_exit 0 "compare baseline json"
check_json "$WORK/stdout" static "compare baseline json"
cp "$WORK/stdout" "$WORK/cmp/a.json"
expect_empty_temp "compare baseline json"

run_perf "$WORK/cmp/a/cmp.fas" --static --compare "$WORK/cmp/a.json"
expect_exit 0 "compare with the same program"
if grep -q 'change:\|new row\|gone:' "$WORK/stderr"; then
  fail "same program compares as changed: $(cat "$WORK/stderr")"
fi
grep -Fq 'compared with a.json' "$WORK/stderr" || fail "same program compare header: $(cat "$WORK/stderr")"
expect_empty_temp "compare with the same program"

run_perf "$WORK/cmp/b/cmp.fas" --static --compare "$WORK/cmp/a.json"
expect_exit 0 "compare changed text"
cat >"$WORK/cmp/b.expected" <<'EXPECTED'
cmp.fas  static (no run)  compared with a.json
cmp.fas:4     for i u64 = 1; i < 65536; i += 1 {       main
    change: not vectorized → x4 interleave 2   unrolled x5 → unrolled x2   instr/iter 14 → 16   loads 0 → 4   stores 5 → 4   cycles/iter 3.5 → 4.1
      vectorized x4 interleave 2   unrolled x2   16 instr/iter (8 elements)   loads 4 stores 4 /iter   4.1 cycles/iter
EXPECTED
diff "$WORK/cmp/b.expected" "$WORK/stderr" || fail "compare changed text: $(cat "$WORK/stderr")"
expect_empty_temp "compare changed text"

run_perf "$WORK/cmp/b/cmp.fas" --static --json --compare "$WORK/cmp/a.json"
expect_exit 0 "compare changed json"
check_json "$WORK/stdout" static "compare changed json"
python3 -c '
import json, sys
doc = json.load(open(sys.argv[1]))
if doc["gone"] != []:
    sys.exit("gone is not empty: " + json.dumps(doc["gone"]))
rows = doc["rows"]
if len(rows) != 1 or rows[0]["line"] != 4:
    sys.exit("expected one loop row at line 4")
expected = {
    "instrs": [14, 16],
    "loads": [0, 4],
    "stores": [5, 4],
    "vectorized": [None, {"factor": 4, "interleave": 2}],
    "unroll": ["unrolled x5", "unrolled x2"],
    "cycles_per_iter": [3.55, 4.11],
}
change = {k: v for k, v in rows[0]["change"].items() if not k.startswith("native_")}
if change != expected:
    sys.exit("change is " + json.dumps(rows[0]["change"]))
' "$WORK/stdout" || fail "compare changed json fields"
expect_empty_temp "compare changed json"

run_perf "$WORK/cmp/c/cmp.fas" --static --json --compare "$WORK/cmp/a.json"
expect_exit 0 "compare new row json"
check_json "$WORK/stdout" static "compare new row json"
python3 -c '
import json, sys
doc = json.load(open(sys.argv[1]))
changes = {row["line"]: row["change"] for row in doc["rows"]}
if changes != {4: None, 7: "new"} or doc["gone"] != []:
    sys.exit("changes are " + json.dumps(changes) + " gone " + json.dumps(doc["gone"]))
' "$WORK/stdout" || fail "compare new row json fields"
expect_empty_temp "compare new row json"

run_perf "$WORK/cmp/c/cmp.fas" --static --json
expect_exit 0 "compare two-loop baseline json"
cp "$WORK/stdout" "$WORK/cmp/c.json"
expect_empty_temp "compare two-loop baseline json"

run_perf "$WORK/cmp/a/cmp.fas" --static --compare "$WORK/cmp/c.json"
expect_exit 0 "compare gone row"
grep -Fxq 'gone: loop cmp.fas:7' "$WORK/stderr" || fail "compare gone row: $(cat "$WORK/stderr")"
expect_empty_temp "compare gone row"

run_perf "$WORK/cmp/a/cmp.fas" --static --json --compare "$WORK/cmp/c.json"
expect_exit 0 "compare gone row json"
check_json "$WORK/stdout" static "compare gone row json"
python3 -c '
import json, sys
doc = json.load(open(sys.argv[1]))
if doc["gone"] != ["loop cmp.fas:7"]:
    sys.exit("gone is " + json.dumps(doc["gone"]))
' "$WORK/stdout" || fail "compare gone row json fields"
expect_empty_temp "compare gone row json"

run_perf "$WORK/cmp/b/cmp.fas" --static --json
expect_exit 0 "compare baseline b json"
cp "$WORK/stdout" "$WORK/cmp/b.json"
expect_empty_temp "compare baseline b json"

cat >"$WORK/cmp/d/cmp.fas" <<'FAS'
var buf arr[65536, u16] = {}

fn main() i32 {
    for i u64 = 1; i < 65536; i += 1 {
        buf[i] = buf[i] + 1
    }
    return trunc[i32](zext[u64](buf[65535]) & 127)
}
FAS
run_perf "$WORK/cmp/d/cmp.fas" --static --compare "$WORK/cmp/b.json"
expect_exit 0 "compare vector width change"
grep -Fxq '    change: x4 interleave 2 → x8 interleave 2   unrolled x2 → unrolled x3   instr/iter 16 → 21   loads 4 → 6   stores 4 → 6   cycles/iter 4.1 → 6.1   bound: register dependency (new)' "$WORK/stderr" ||
  fail "compare vector width change: $(cat "$WORK/stderr")"
expect_empty_temp "compare vector width change"

run_perf "$WORK/cmp/d/cmp.fas" --static --json --compare "$WORK/cmp/b.json"
expect_exit 0 "compare vector width change json"
check_json "$WORK/stdout" static "compare vector width change json"
python3 -c '
import json, sys
doc = json.load(open(sys.argv[1]))
changes = [{k: v for k, v in row["change"].items() if not k.startswith("native_")} if isinstance(row["change"], dict) else row["change"] for row in doc["rows"]]
expected = {
    "vectorized": [{"factor": 4, "interleave": 2}, {"factor": 8, "interleave": 2}],
    "unroll": ["unrolled x2", "unrolled x3"],
    "instrs": [16, 21],
    "loads": [4, 6],
    "stores": [4, 6],
    "cycles_per_iter": [4.11, 6.1],
    "bound": [None, "register dependency"],
}
if changes != [expected]:
    sys.exit("changes are " + json.dumps(changes))
' "$WORK/stdout" || fail "compare vector width change json fields"
expect_empty_temp "compare vector width change json"

python3 -c '
import json, sys
with open(sys.argv[1], encoding="utf-8") as source:
    doc = json.load(source)
for row in doc["rows"]:
    row.pop("shown", None)
with open(sys.argv[2], "w", encoding="utf-8") as target:
    json.dump(doc, target)
' "$WORK/cmp/a.json" "$WORK/cmp/old.json"
run_perf "$WORK/cmp/a/cmp.fas" --static --compare "$WORK/cmp/old.json"
expect_exit 1 "compare row text missing"
grep -Fxq -- "--compare: $WORK/cmp/old.json lacks row text; rerun it with this fas" "$WORK/stderr" ||
  fail "compare row text missing message: $(cat "$WORK/stderr")"
expect_empty_temp "compare row text missing"

mkdir -p "$WORK/cmp/k" "$WORK/cmp/m" "$WORK/cmp/n" "$WORK/cmp/p"
cat >"$WORK/cmp/k/cmp.fas" <<'FAS'
use "C" <<C
#include <stdint.h>
static inline uint64_t c_step(uint64_t x) { return x * 3 + 1; }
C

var buf arr[65536, u32] = {}

fn main() i32 {
    acc u64 = 1
    for i u64 = 1; i < 65536; i += 1 {
        acc = c_step(acc) ^ i
    }
    return trunc[i32](acc & 127)
}
FAS
cat >"$WORK/cmp/m/cmp.fas" <<'FAS'
use "C" <<C
#include <stdint.h>
static inline uint64_t c_step(uint64_t x) { return x * 3 + 1; }
C

var buf arr[65536, u32] = {}

fn main() i32 {
    acc u64 = 1
    for i u64 = 1; i < 65536; i += 1 {
        buf[i] = buf[i] + 1
    }
    return trunc[i32](acc & 127)
}
FAS
run_perf "$WORK/cmp/k/cmp.fas" --static --json
expect_exit 0 "compare scalar baseline json"
check_json "$WORK/stdout" static "compare scalar baseline json"
cp "$WORK/stdout" "$WORK/cmp/k.json"
expect_empty_temp "compare scalar baseline json"

run_perf "$WORK/cmp/m/cmp.fas" --static --json
expect_exit 0 "compare vector baseline json"
check_json "$WORK/stdout" static "compare vector baseline json"
cp "$WORK/stdout" "$WORK/cmp/m.json"
expect_empty_temp "compare vector baseline json"

cat >"$WORK/cmp/m.expected" <<'EXPECTED'
cmp.fas  static (no run)  compared with k.json
cmp.fas:10    for i u64 = 1; i < 65536; i += 1 {       main
    change: not vectorized → x4 interleave 2   unrolled x2 (new)   instr/iter 6 → 16   loads 0 → 4   stores 0 → 4   cycles/iter 4.1 (new)   call kept: c_step (its definition is unavailable) (gone)
      vectorized x4 interleave 2   unrolled x2   16 instr/iter (8 elements)   loads 4 stores 4 /iter   4.1 cycles/iter
EXPECTED
run_perf "$WORK/cmp/m/cmp.fas" --static --compare "$WORK/cmp/k.json"
expect_exit 0 "compare scalar to vector change"
diff "$WORK/cmp/m.expected" "$WORK/stderr" || fail "compare scalar to vector change: $(cat "$WORK/stderr")"
expect_empty_temp "compare scalar to vector change"

cat >"$WORK/cmp/k.expected" <<'EXPECTED'
cmp.fas  static (no run)  compared with m.json
cmp.fas:10    for i u64 = 1; i < 65536; i += 1 {       main
    change: x4 interleave 2 → not vectorized   unrolled x2 (gone)   instr/iter 16 → 6   loads 4 → 0   stores 4 → 0   cycles/iter 4.1 (gone)   call kept: c_step (its definition is unavailable) (new)
      not vectorized: value that could not be identified as reduction is used outside the loop   6 instr/iter   loads 0 stores 0 /iter   call kept: c_step (its definition is unavailable)
EXPECTED
run_perf "$WORK/cmp/k/cmp.fas" --static --compare "$WORK/cmp/m.json"
expect_exit 0 "compare vector to scalar change"
diff "$WORK/cmp/k.expected" "$WORK/stderr" || fail "compare vector to scalar change: $(cat "$WORK/stderr")"
expect_empty_temp "compare vector to scalar change"

cat >"$WORK/cmp/n/cmp.fas" <<'FAS'
use "C" <<C
#include <stdint.h>
static inline uint64_t c_step(uint64_t x) { return x * 3 + 1; }
C

var buf arr[65536, u32] = {}

fn main() i32 {
    acc u64 = 1
    for i u64 = 1; i < 65536; i += 1 {
        if buf[i] == 7 {
            return 1
        }
        acc = c_step(acc) ^ i
    }
    return trunc[i32](acc & 127)
}
FAS
cat >"$WORK/cmp/p/cmp.fas" <<'FAS'
use "C" <<C
#include <stdint.h>
static inline uint64_t c_step(uint64_t x) { return x * 3 + 1; }
C

var buf arr[65536, u32] = {}

fn main() i32 {
    acc u64 = 1
    for i u64 = 1; i < 65536; i += 1 {
        if buf[i] == 7 {
            return 1
        }
        buf[i] = buf[i] + 1
    }
    return trunc[i32](acc & 127)
}
FAS
status=0
FAS_MCA=/nonexistent "$OCAML_FAS" perf "$WORK/cmp/p/cmp.fas" --static --json >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
expect_exit 0 "compare reason baseline json"
check_json "$WORK/stdout" static "compare reason baseline json"
cp "$WORK/stdout" "$WORK/cmp/p.json"
expect_empty_temp "compare reason baseline json"

cat >"$WORK/cmp/n.expected" <<'EXPECTED'
cmp.fas  static (no run)  compared with p.json
cmp.fas:10    for i u64 = 1; i < 65536; i += 1 {       main
    change: not vectorized: Early exit loop with store but no supported condition load → value that could not be identified as reduction is used outside the loop   unrolled x3 (gone)   instr/iter 18 → 6   loads 3 → 0   stores 3 → 0   call kept: c_step (its definition is unavailable) (new)
      not vectorized: value that could not be identified as reduction is used outside the loop   6 instr/iter   loads 0 stores 0 /iter   call kept: c_step (its definition is unavailable)
EXPECTED
status=0
FAS_MCA=/nonexistent "$OCAML_FAS" perf "$WORK/cmp/n/cmp.fas" --static --compare "$WORK/cmp/p.json" >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
expect_exit 0 "compare reason change"
[ ! -s "$WORK/stdout" ] || fail "compare reason change wrote to stdout"
diff "$WORK/cmp/n.expected" "$WORK/stderr" || fail "compare reason change: $(cat "$WORK/stderr")"
expect_empty_temp "compare reason change"

if "$PERF" stat true >/dev/null 2>&1; then
  mkdir -p "$WORK/cmp/x"
  cat >"$WORK/cmp/x/cmp.fas" <<'FAS'
var buf arr[65536, u32] = {}

fn main() i32 {
    for i u64 = 1; i < 65536; i += 1 {
        buf[i] = buf[i - 1] + 1
    }
    return 0
}
FAS
  run_perf "$WORK/cmp/x/cmp.fas" --compare "$WORK/cmp/a.json"
  expect_exit 0 "compare facts only"
  grep -Fq 'compared with a.json  (facts only: one side is static)' "$WORK/stderr" ||
    fail "compare facts only header: $(head -1 "$WORK/stderr")"
  expect_empty_temp "compare facts only"
else
  echo "perf_command: facts-only compare skipped: perf stat true failed" >&2
fi

run_perf "$WORK/cmp/a/cmp.fas" --static --compare "$WORK/cmp/missing.json"
expect_exit 1 "compare missing file"
grep -Fxq -- "--compare: $WORK/cmp/missing.json: No such file or directory" "$WORK/stderr" ||
  fail "compare missing file message: $(cat "$WORK/stderr")"
expect_empty_temp "compare missing file"

printf '%s\n' '{bad' >"$WORK/cmp/bad.json"
run_perf "$WORK/cmp/a/cmp.fas" --static --compare "$WORK/cmp/bad.json"
expect_exit 1 "compare invalid json"
grep -Fxq -- "--compare: $WORK/cmp/bad.json is not valid JSON: expected '\"' at byte 1" "$WORK/stderr" ||
  fail "compare invalid json message: $(cat "$WORK/stderr")"
expect_empty_temp "compare invalid json"

printf '%s\n' '{"format":"other","version":1,"rows":[]}' >"$WORK/cmp/wrong.json"
run_perf "$WORK/cmp/a/cmp.fas" --static --compare "$WORK/cmp/wrong.json"
expect_exit 1 "compare wrong format"
grep -Fxq -- "--compare: $WORK/cmp/wrong.json is not fas perf JSON version 1" "$WORK/stderr" ||
  fail "compare wrong format message: $(cat "$WORK/stderr")"
expect_empty_temp "compare wrong format"

run_perf "$WORK/cmp/a/cmp.fas" --static --compare
expect_exit 2 "compare without a file"
grep -q -- '--compare requires a file' "$WORK/stderr" || fail "compare without a file message"
expect_empty_temp "compare without a file"

expect_empty_temp "usage errors"
