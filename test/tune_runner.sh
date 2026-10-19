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
  echo "tune_runner: $*" >&2
  exit 1
}

CC=$(resolve_tool_path "$CC")
LLVM_OPT=$(resolve_tool_path "$LLVM_OPT")
LLVM_LLC=$(resolve_tool_path "$LLVM_LLC")
OCAML_FAS=$(resolve_tool_path "$OCAML_FAS")
export FAS_CC="$CC" FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC"

RUNNER_TMP=$(mktemp -d)
WORK=$RUNNER_TMP/work
TMPDIR=$RUNNER_TMP/tmp
mkdir -p "$WORK" "$TMPDIR"
export TMPDIR
trap 'rm -rf "$RUNNER_TMP"' EXIT HUP INT TERM
cd "$WORK"

echo 'fn main() i32 { return 0 }' >empty.fas
FAS_TUNE_TEST_TOOLS="$WORK" "$OCAML_FAS" perf --tune empty.fas || fail "tools did not build"
[ -x runner ] && [ -f arena.so ] || fail "runner or arena.so missing"
[ ! -e empty.fasprof ] || fail "the tools hook wrote a profile"

line_shape() {
  grep -E '^[0-9]+ ([0-9]+|-) ([0-9]+|-) ([0-9]+|sig:[A-Z0-9]+) [0-9]+$' "$1/result" >/dev/null \
    || fail "result line has the wrong shape: $(cat "$1/result")"
}
field() { awk -v n="$2" '{ print $n }' "$1/result"; }

mkdir true.out
./runner -1 - 0 true.out /bin/true -- true || fail "runner failed on /bin/true"
line_shape true.out
[ "$(field true.out 4)" = 0 ] || fail "/bin/true status $(field true.out 4)"
[ "$(field true.out 1)" -gt 0 ] || fail "/bin/true wall time is zero"
[ ! -s true.out/stdout ] && [ ! -s true.out/stderr ] || fail "/bin/true wrote output"

cat >streams.fas <<'FAS'
use "C" <stdio.h>
fn main() i32 {
  if fputs(c"to stdout\n", stdout) < 0 { return 1 }
  if fputs(c"to stderr\n", stderr) < 0 { return 2 }
  return 3
}
FAS
"$OCAML_FAS" streams.fas -o streams || fail "streams.fas did not build"
mkdir streams.out
./runner -1 - 0 streams.out "$WORK/streams" -- argv-zero || fail "runner failed on streams"
line_shape streams.out
[ "$(field streams.out 4)" = 3 ] || fail "streams status $(field streams.out 4)"
[ "$(cat streams.out/stdout)" = "to stdout" ] || fail "stdout file: $(cat streams.out/stdout)"
[ "$(cat streams.out/stderr)" = "to stderr" ] || fail "stderr file: $(cat streams.out/stderr)"

cat >argv.c <<'C'
#include <stdio.h>
int main(int argc, char **argv) {
  int c = getchar();
  printf("%s %d %s %d\n", argv[0], argc, argc > 1 ? argv[1] : "", c);
  return 0;
}
C
"$CC" -O2 argv.c -o argv-print || fail "argv.c did not build"
mkdir argv.out
./runner -1 - 0 argv.out "$WORK/argv-print" -- shown "two words" </dev/zero || fail "runner failed on argv"
[ "$(cat argv.out/stdout)" = "shown 2 two words -1" ] || fail "argv and stdin: $(cat argv.out/stdout)"

cat >abort.c <<'C'
#include <stdlib.h>
int main(void) { abort(); }
C
"$CC" -O2 abort.c -o abort-now || fail "abort.c did not build"
mkdir abort.out
./runner -1 - 0 abort.out "$WORK/abort-now" -- abort-now || fail "runner failed on abort"
line_shape abort.out
[ "$(field abort.out 4)" = sig:SIGABRT ] || fail "abort status $(field abort.out 4)"

mkdir missing.out
./runner -1 - 0 missing.out "$WORK/no-such-program" -- x || fail "runner failed on a missing program"
[ "$(field missing.out 4)" = 127 ] || fail "missing program status $(field missing.out 4)"
grep -F "runner: cannot run $WORK/no-such-program" missing.out/stderr >/dev/null \
  || fail "missing program message: $(cat missing.out/stderr)"

if ./runner 2>usage.err; then fail "runner without arguments succeeded"; fi
grep -F 'usage: runner CPU ARENA OFFSET OUTDIR PATH -- ARGV0 [ARGS...]' usage.err >/dev/null \
  || fail "usage text: $(cat usage.err)"

cpu=$(awk '/^Cpus_allowed_list:/ { print $2 }' /proc/self/status | cut -d, -f1 | cut -d- -f1)
mkdir pin.out unpinned.out
./runner "$cpu" - 0 pin.out /bin/sh -- sh -c 'grep Cpus_allowed_list /proc/self/status' \
  || fail "runner failed pinning"
[ "$(awk '{ print $2 }' pin.out/stdout)" = "$cpu" ] || fail "pinned affinity: $(cat pin.out/stdout)"
./runner -1 - 0 unpinned.out /bin/sh -- sh -c 'grep Cpus_allowed_list /proc/self/status' \
  || fail "runner failed unpinned"
[ "$(awk '{ print $2 }' unpinned.out/stdout)" = "$(awk '/^Cpus_allowed_list:/ { print $2 }' /proc/self/status)" ] \
  || fail "unpinned affinity: $(cat unpinned.out/stdout)"

mkdir env.out noenv.out
./runner -1 "$WORK/arena.so" 65536 env.out /bin/sh -- sh -c 'printf "%s %s\n" "$LD_PRELOAD" "$FAS_ARENA_OFFSET"' \
  || fail "runner failed with the arena"
[ "$(cat env.out/stdout)" = "$WORK/arena.so 65536" ] || fail "arena environment: $(cat env.out/stdout)"
env -u LD_PRELOAD -u FAS_ARENA_OFFSET ./runner -1 - 0 noenv.out /bin/sh -- sh -c 'printf "[%s][%s]\n" "${LD_PRELOAD-}" "${FAS_ARENA_OFFSET-}"' \
  || fail "runner failed without the arena"
[ "$(cat noenv.out/stdout)" = "[][]" ] || fail "environment without the arena: $(cat noenv.out/stdout)"

cat >alloc.c <<'C'
#include <errno.h>
#include <malloc.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define N 10000
static unsigned char *blocks[N];
static size_t sizes[N];
int main(void) {
  unsigned long long sum = 0;
  for (int i = 0; i < N; i++) {
    size_t size = (size_t)(i * 7919 % 5000) + 1;
    size_t align = (size_t)16 << (i % 6);
    unsigned char *p;
    switch (i % 5) {
    case 0: p = malloc(size); align = 16; break;
    case 1: p = aligned_alloc(align, size); break;
    case 2: if (posix_memalign((void **)&p, align, size)) p = 0; break;
    case 3: p = memalign(align, size); break;
    default: p = calloc(size, 1); align = 16;
      for (size_t k = 0; k < size; k++) if (p && p[k]) { puts("calloc not zeroed"); return 1; }
    }
    if (!p) { printf("allocation %d failed\n", i); return 1; }
    if ((uintptr_t)p % align) { printf("allocation %d misaligned\n", i); return 1; }
    if (malloc_usable_size(p) < size) { printf("allocation %d too small\n", i); return 1; }
    memset(p, i & 255, size);
    blocks[i] = p;
    sizes[i] = size;
  }
  for (int i = 0; i < N; i += 2) { free(blocks[i]); blocks[i] = 0; }
  for (int i = 1; i < N; i += 2) {
    for (size_t k = 0; k < sizes[i]; k++) if (blocks[i][k] != (i & 255)) { printf("block %d changed\n", i); return 1; }
    unsigned char *q = realloc(blocks[i], sizes[i] * 3 + 100);
    if (!q) { puts("realloc failed"); return 1; }
    for (size_t k = 0; k < sizes[i]; k++) if (q[k] != (i & 255)) { printf("realloc %d lost content\n", i); return 1; }
    for (size_t k = 0; k < sizes[i]; k++) sum += q[k];
    free(q);
  }
  volatile size_t half = (size_t)-1 / 2, four = 4;
  errno = 0;
  void *volatile big = calloc(half, four);
  if (big != 0) { puts("calloc overflow"); return 1; }
  big = reallocarray(0, half, four);
  if (big != 0) { puts("reallocarray overflow"); return 1; }
  unsigned char *page = valloc(100);
  if (!page || (uintptr_t)page % 4096) { puts("valloc"); return 1; }
  free(page);
  page = pvalloc(5000);
  if (!page || (uintptr_t)page % 4096 || malloc_usable_size(page) < 8192) { puts("pvalloc"); return 1; }
  free(page);
  void *r;
  if (posix_memalign(&r, 24, 8) != EINVAL) { puts("posix_memalign accepted 24"); return 1; }
  printf("ok %llu\n", sum);
  return 0;
}
C
"$CC" -O2 alloc.c -o alloc || fail "alloc.c did not build"
expected=$(./alloc)
case "$expected" in ok\ *) ;; *) fail "alloc without the arena: $expected" ;; esac
for offset in 0 65536; do
  got=$(LD_PRELOAD="$WORK/arena.so" FAS_ARENA_OFFSET=$offset ./alloc) || fail "alloc under the arena at offset $offset: $got"
  [ "$got" = "$expected" ] || fail "alloc under the arena at offset $offset: $got"
  mkdir "alloc-$offset.out"
  ./runner -1 "$WORK/arena.so" "$offset" "alloc-$offset.out" "$WORK/alloc" -- alloc || fail "runner failed on alloc"
  [ "$(field "alloc-$offset.out" 4)" = 0 ] && [ "$(cat "alloc-$offset.out/stdout")" = "$expected" ] \
    || fail "alloc through the runner at offset $offset: $(cat "alloc-$offset.out/stdout")"
done

cat >placement.c <<'C'
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
int main(void) {
  void *p = malloc(32);
  printf("%lu\n", (unsigned long)((uintptr_t)p & ((2u << 20) - 1)));
  return 0;
}
C
"$CC" -O2 placement.c -o placement || fail "placement.c did not build"
at0=$(LD_PRELOAD="$WORK/arena.so" FAS_ARENA_OFFSET=0 ./placement)
at64k=$(LD_PRELOAD="$WORK/arena.so" FAS_ARENA_OFFSET=65536 ./placement)
again=$(LD_PRELOAD="$WORK/arena.so" FAS_ARENA_OFFSET=65536 ./placement)
[ "$at64k" = "$again" ] || fail "arena placement is not repeatable: $at64k vs $again"
[ $((at64k - at0)) -eq 65536 ] || fail "arena offset moved the first block by $((at64k - at0)), not 65536"

cat >threads.c <<'C'
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
static void *work(void *arg) {
  unsigned long long seed = (unsigned long long)(size_t)arg, sum = 0;
  unsigned char *live[64] = {0};
  for (int i = 0; i < 200000; i++) {
    seed = seed * 6364136223846793005ull + 1442695040888963407ull;
    int slot = (int)(seed >> 58);
    if (live[slot]) { sum += live[slot][0]; free(live[slot]); }
    size_t size = (size_t)(seed >> 40) % 3000 + 1;
    live[slot] = malloc(size);
    if (!live[slot]) return (void *)1;
    memset(live[slot], slot, size);
  }
  for (int s = 0; s < 64; s++) if (live[s]) { sum += live[s][0]; free(live[s]); }
  printf("%llu\n", sum);
  return 0;
}
int main(void) {
  pthread_t a, b;
  void *ra, *rb;
  pthread_create(&a, 0, work, (void *)1);
  pthread_create(&b, 0, work, (void *)2);
  pthread_join(a, &ra);
  pthread_join(b, &rb);
  return ra || rb;
}
C
"$CC" -O2 -pthread threads.c -o threads || fail "threads.c did not build"
plain=$(./threads | sort)
shimmed=$(LD_PRELOAD="$WORK/arena.so" FAS_ARENA_OFFSET=0 ./threads | sort) || fail "two-thread stress under the arena failed"
[ "$plain" = "$shimmed" ] || fail "two-thread sums differ: $plain vs $shimmed"

echo "tune_runner: passed"
