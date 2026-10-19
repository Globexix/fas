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
  echo "profile_build: $*" >&2
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

cat >main.fas <<'FAS'
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
  return total % 251
}
FAS

cat >block.c <<'C'
int prof_block_bump(int value) {
  return value * 3 + 1;
}
C

cat >prof_link.c <<'C'
int prof_link_bump(int value) {
  return value * 5 - 2;
}
C

mkdir stale
cat >stale/main.fas <<'FAS'
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
    if i == 7 {
      total = total + 2
    }
    i = i + 1
  }
  return total % 251
}
FAS

cat >stale/prof_link.c <<'C'
int prof_link_bump(int value) {
  if (value > 100000) {
    return 0;
  }
  return value * 5 - 2;
}
C

printf 'fas profile 3\nfas 0.5.2\ncounts\n' >wrong.fasprof
: >empty.fasprof
printf 'fas profile 1\ncounts\n' >early.fasprof
printf 'fas profile 1\nfas 0.5.2\nllvm x\ncpu y\nnot counts\n' >nocounts.fasprof
printf 'fas profile 1\nfas 0.5.2\nllvm x\ncpu y\ncounts\ngarbage here\n' >bad.fasprof

cat >cc-log.sh <<SH
#!/bin/sh
printf '%s\n' "\$*" >>"$WORK/cc.log"
exec "$CC" "\$@"
SH
chmod +x cc-log.sh

cat >llc-log.sh <<SH
#!/bin/sh
printf '%s\n' "\$*" >>"$WORK/llc.log"
exec "$LLVM_LLC" "\$@"
SH
chmod +x llc-log.sh

no_profile_temps() {
  for leftover in "$TMPDIR"/fas-profile-*; do
    if [ -e "$leftover" ]; then
      fail "profile temporary left behind: $leftover"
    fi
  done
}

fas_build() {
  name=$1
  shift
  status=0
  "$OCAML_FAS" "$@" -o "$name" >"$name.out" 2>"$name.err" || status=$?
  if [ "$status" -ne 0 ]; then
    cat "$name.err" >&2
    fail "build $name failed with status $status"
  fi
  no_profile_temps
}

expect_error() {
  name=$1
  expected=$2
  shift 2
  status=0
  "$OCAML_FAS" "$@" -o "$name.bin" >"$name.out" 2>"$name.err" || status=$?
  [ "$status" -eq 1 ] || fail "$name: exit status $status, expected 1"
  actual=$(cat "$name.err")
  [ "$actual" = "$expected" ] || fail "$name: stderr is '$actual', expected '$expected'"
  [ ! -e "$name.bin" ] || fail "$name: an output file was written"
  no_profile_temps
}

expect_invalid_counts() {
  name=$1
  shift
  status=0
  "$OCAML_FAS" "$@" -o "$name.bin" >"$name.out" 2>"$name.err" || status=$?
  [ "$status" -eq 1 ] || fail "$name: exit status $status, expected 1"
  [ "$(wc -l <"$name.err")" -eq 1 ] || fail "$name: stderr is not a single line: $(cat "$name.err")"
  case "$(cat "$name.err")" in
    'error: profile `bad.fasprof` has invalid counts: warning: '*': truncated profile data') ;;
    *) fail "$name: unexpected stderr: $(cat "$name.err")" ;;
  esac
  [ ! -e "$name.bin" ] || fail "$name: an output file was written"
  no_profile_temps
}

run_program() {
  status=0
  "./$1" >"$1.stdout" 2>"$1.stderr" || status=$?
  [ "$status" -eq "$2" ] || fail "$1 exited $status, expected $2"
}

"$OCAML_FAS" --emit-llvm main.fas >plain.ll
"$LLVM_OPT" -mcpu=x86-64 -passes='default<O2>' --pgo-kind=pgo-instr-gen-pipeline plain.ll -o gen.bc
"$LLVM_LLC" -O2 -mcpu=x86-64 -relocation-model=pic -filetype=obj gen.bc -o gen.o
"$CC" -O2 -fprofile-generate -c block.c -o block.o
"$CC" -O2 -fprofile-generate -c prof_link.c -o link.o
"$CC" -fprofile-generate gen.o block.o link.o -o gen -lm
status=0
LLVM_PROFILE_FILE="$WORK/main.profraw" ./gen >/dev/null || status=$?
[ "$status" -eq 250 ] || fail "instrumented program exited $status, expected 250"
"$PROFDATA" merge --text -o main.proftext main.profraw
"$PROFDATA" merge -o main.profdata main.proftext
FAS_VERSION=$("$OCAML_FAS" --version | sed -n '1p' | awk '{ print $2 }')
OPT_LINE=$("$LLVM_OPT" --version | sed -n 's/^[[:space:]]*//;/LLVM version/{p;q;}')
HOST_CPU=$("$LLVM_LLC" --version | sed -n 's/^[[:space:]]*Host CPU: *//p')
{
  echo 'fas profile 1'
  echo "fas $FAS_VERSION"
  echo "llvm $OPT_LINE"
  echo "cpu $HOST_CPU"
  echo counts
  cat main.proftext
} >main.fasprof

"$OCAML_FAS" --profile main.fasprof --emit-llvm=optimized main.fas >with-profile.ll
"$OCAML_FAS" --emit-llvm=optimized main.fas >without-profile.ll
grep -q '!"function_entry_count", i64 1}' with-profile.ll || fail "optimized IR lacks the profile entry count"
grep -q '!"branch_weights"' with-profile.ll || fail "optimized IR lacks profile branch weights"
grep -q 'branch_weights", i32 64, i32 4032}' with-profile.ll || grep -q 'branch_weights", i32 4032, i32 64}' with-profile.ll || fail "optimized IR branch weights do not match the run"
if grep -q -e '!"function_entry_count"' -e '!"branch_weights"' without-profile.ll; then
  fail "a build without --profile carries profile metadata"
fi

"$CC" -O2 -fprofile-use="$WORK/main.profdata" -Wno-profile-instr-out-of-date -Wno-profile-instr-unprofiled -Wno-profile-instr-missing -S -emit-llvm -o link-profiled.ll prof_link.c
grep -q 'function_entry_count", i64 4032}' link-profiled.ll || fail "the C link input function gets no profile counts"

: >cc.log
FAS_CC="$WORK/cc-log.sh" "$OCAML_FAS" --profile main.fasprof main.fas prof_link.c -o cc-profile.bin
grep 'prof_link.c' cc.log >link-compile.txt
for flag in '-fprofile-use=' '-Wno-profile-instr-out-of-date' '-Wno-profile-instr-unprofiled' '-Wno-profile-instr-missing'; do
  grep -q -F -e "$flag" link-compile.txt || fail "the C link input compile lacks $flag"
done
: >cc.log
FAS_CC="$WORK/cc-log.sh" "$OCAML_FAS" main.fas prof_link.c -o cc-plain.bin
if grep -q -F -e '-fprofile-use=' cc.log; then
  fail "a build without --profile passes -fprofile-use"
fi

fas_build plain-O0.bin -O0 main.fas prof_link.c
fas_build profile-O0.bin -O0 --profile main.fasprof main.fas prof_link.c
cmp plain-O0.bin profile-O0.bin || fail "the O0 executable changed with a profile"
fas_build plain-O2.bin main.fas prof_link.c
fas_build profile-O2.bin --profile main.fasprof main.fas prof_link.c
run_program plain-O0.bin 250
run_program profile-O0.bin 250
run_program plain-O2.bin 250
run_program profile-O2.bin 250
cmp plain-O2.bin.stdout profile-O2.bin.stdout || fail "the O2 program output changed with a profile"

fas_build det-a.bin --profile main.fasprof main.fas prof_link.c
fas_build det-b.bin --profile main.fasprof main.fas prof_link.c
cmp det-a.bin det-b.bin || fail "two profile builds produced different executables"
fas_build det-a.o -c --profile main.fasprof main.fas
fas_build det-b.o -c --profile main.fasprof main.fas
cmp det-a.o det-b.o || fail "two profile builds produced different objects"

expect_error unreadable 'error: cannot read profile `nope.fasprof`' --profile nope.fasprof main.fas prof_link.c
expect_error unreadable-O0 'error: cannot read profile `nope.fasprof`' -O0 --profile nope.fasprof main.fas prof_link.c
expect_error wrong-version 'error: `wrong.fasprof` is not a fas profile (expected `fas profile 1` or `fas profile 2` on line 1)' --profile wrong.fasprof main.fas prof_link.c
expect_error empty-profile 'error: `empty.fasprof` is not a fas profile (expected `fas profile 1` or `fas profile 2` on line 1)' --profile empty.fasprof main.fas prof_link.c
expect_error early-counts 'error: profile `early.fasprof` has no `counts` section' --profile early.fasprof main.fas prof_link.c
expect_error no-counts 'error: profile `nocounts.fasprof` has no `counts` section' --profile nocounts.fasprof main.fas prof_link.c
expect_invalid_counts invalid-counts --profile bad.fasprof main.fas prof_link.c
expect_invalid_counts invalid-counts-O0 -O0 --profile bad.fasprof main.fas prof_link.c

: >cc.log
status=0
FAS_CC="$WORK/cc-log.sh" "$OCAML_FAS" --profile bad.fasprof main.fas prof_link.c -o bad-cc.bin 2>bad-cc.err || status=$?
[ "$status" -eq 1 ] || fail "invalid counts with a Clang wrapper: exit status $status, expected 1"
[ ! -s cc.log ] || fail "a C tool ran after the profile error"

cd stale
"$OCAML_FAS" --profile ../main.fasprof main.fas prof_link.c -o stale.bin 2>stale.err || fail "stale profile build failed: $(cat stale.err)"
[ ! -s stale.err ] || fail "stale profile build printed: $(cat stale.err)"
cd "$WORK"
no_profile_temps

profile2() {
  {
    echo 'fas profile 2'
    echo "fas $FAS_VERSION"
    echo "llvm $OPT_LINE"
    echo "cpu $2"
    echo "$3"
    echo "$4"
    echo "$5"
    echo counts
    cat main.proftext
  } >"$1"
}
MEASURED='measured 1.0430 band 0.0120 rounds 7 placements 9 metric cycles'
profile2 pgo2.fasprof "$HOST_CPU" 'choice pgo' 'align-loops def' "$MEASURED"
profile2 default2.fasprof "$HOST_CPU" 'choice default' 'align-loops def' "$MEASURED"
profile2 align64.fasprof "$HOST_CPU" 'choice default' 'align-loops 64' "$MEASURED"
profile2 align32.fasprof "$HOST_CPU" 'choice pgo' 'align-loops 32' "$MEASURED"
profile2 native2.fasprof "$HOST_CPU" 'choice native' 'align-loops def' "$MEASURED"
profile2 nativepgo2.fasprof "$HOST_CPU" 'choice native-pgo' 'align-loops def' "$MEASURED"
profile2 othercpu.fasprof othercpu 'choice native' 'align-loops def' "$MEASURED"
profile2 othercpu-pgo.fasprof othercpu 'choice native-pgo' 'align-loops def' "$MEASURED"
profile2 othercpu-portable.fasprof othercpu 'choice pgo' 'align-loops def' "$MEASURED"
profile2 badchoice.fasprof "$HOST_CPU" 'choice fast' 'align-loops def' "$MEASURED"
profile2 badalign.fasprof "$HOST_CPU" 'choice pgo' 'align-loops 16' "$MEASURED"
profile2 nochoice.fasprof "$HOST_CPU" 'align-loops def' 'choice pgo' "$MEASURED"
profile2 noalign.fasprof "$HOST_CPU" 'choice pgo' "$MEASURED" 'align-loops def'
profile2 nomeasured.fasprof "$HOST_CPU" 'choice pgo' 'align-loops def' 'counts'
printf 'fas profile 2\nfas 0.5.2\nllvm x\ncpu y\nchoice pgo\nalign-loops def\nmeasured 1\nnot counts\n' >nocounts2.fasprof

"$OCAML_FAS" --profile pgo2.fasprof --emit-llvm=optimized main.fas >pgo2.ll
cmp with-profile.ll pgo2.ll || fail "a format 2 pgo profile differs from the format 1 profile"
fas_build pgo2-O2.bin --profile pgo2.fasprof main.fas prof_link.c
cmp profile-O2.bin pgo2-O2.bin || fail "a format 2 pgo executable differs from the format 1 executable"

"$OCAML_FAS" --profile default2.fasprof --emit-llvm=optimized main.fas >default2.ll
cmp without-profile.ll default2.ll || fail "choice default changed the optimized IR"
fas_build default2.o -c --profile default2.fasprof main.fas
fas_build plain.o -c main.fas
cmp plain.o default2.o || fail "choice default changed the object"
fas_build default2-O2.bin --profile default2.fasprof main.fas prof_link.c
cmp plain-O2.bin default2-O2.bin || fail "choice default changed the executable"
: >cc.log
FAS_CC="$WORK/cc-log.sh" "$OCAML_FAS" --profile default2.fasprof main.fas prof_link.c -o cc-default2.bin
if grep -q -F -e '-fprofile-use=' cc.log; then
  fail "choice default passes -fprofile-use"
fi

: >llc.log
FAS_LLC="$WORK/llc-log.sh" "$OCAML_FAS" -c main.fas -o llc-plain.o
FAS_LLC="$WORK/llc-log.sh" "$OCAML_FAS" -c --profile default2.fasprof main.fas -o llc-default2.o
FAS_LLC="$WORK/llc-log.sh" "$OCAML_FAS" -c --profile pgo2.fasprof main.fas -o llc-pgo2.o
if grep -q -F -e '-align-loops' llc.log; then
  fail "llc got -align-loops without an align-loops profile line"
fi
: >llc.log
FAS_LLC="$WORK/llc-log.sh" "$OCAML_FAS" -c --profile align64.fasprof main.fas -o llc-align64.o
grep -q -F -e '-align-loops=64' llc.log || fail "align-loops 64 did not reach llc: $(cat llc.log)"
: >llc.log
FAS_LLC="$WORK/llc-log.sh" "$OCAML_FAS" -c --profile align32.fasprof main.fas -o llc-align32.o
grep -q -F -e '-align-loops=32' llc.log || fail "align-loops 32 did not reach llc: $(cat llc.log)"
cmp -s llc-plain.o llc-align64.o && fail "align-loops 64 left the object unchanged"
fas_build align64.bin --profile align64.fasprof main.fas prof_link.c
run_program align64.bin 250

"$OCAML_FAS" --target=force-native --emit-llvm=optimized main.fas >native-plain.ll
"$OCAML_FAS" --profile native2.fasprof --emit-llvm=optimized main.fas >native2.ll
cmp native-plain.ll native2.ll || fail "choice native differs from --target=force-native"
"$OCAML_FAS" --target=force-native --profile main.fasprof --emit-llvm=optimized main.fas >native-v1.ll
"$OCAML_FAS" --profile nativepgo2.fasprof --emit-llvm=optimized main.fas >nativepgo2.ll
cmp native-v1.ll nativepgo2.ll || fail "choice native-pgo differs from --target=force-native with the counts"
grep -q '!"branch_weights"' nativepgo2.ll || fail "choice native-pgo lost the counts"
fas_build native2.bin --profile native2.fasprof main.fas prof_link.c
fas_build nativepgo2.bin --profile nativepgo2.fasprof main.fas prof_link.c
run_program native2.bin 250
run_program nativepgo2.bin 250

expect_error other-cpu "error: profile \`othercpu.fasprof\` was tuned for the CPU \`othercpu\`; this machine is \`$HOST_CPU\` (a native choice runs only on the CPU it was measured on)" --profile othercpu.fasprof main.fas prof_link.c
expect_error other-cpu-pgo "error: profile \`othercpu-pgo.fasprof\` was tuned for the CPU \`othercpu\`; this machine is \`$HOST_CPU\` (a native choice runs only on the CPU it was measured on)" -O0 --profile othercpu-pgo.fasprof main.fas prof_link.c
fas_build other-cpu-portable.bin --profile othercpu-portable.fasprof main.fas prof_link.c
expect_error bad-choice 'error: profile `badchoice.fasprof` has an unknown choice `fast`; expected default, pgo, native or native-pgo' --profile badchoice.fasprof main.fas prof_link.c
expect_error bad-align 'error: profile `badalign.fasprof` has an unknown align-loops value `16`; expected def, 32 or 64' --profile badalign.fasprof main.fas prof_link.c
expect_error no-choice 'error: profile `nochoice.fasprof` line 5 must be `choice default|pgo|native|native-pgo`' --profile nochoice.fasprof main.fas prof_link.c
expect_error no-align 'error: profile `noalign.fasprof` line 6 must be `align-loops def|32|64`' --profile noalign.fasprof main.fas prof_link.c
expect_error no-measured 'error: profile `nomeasured.fasprof` line 7 must be `measured RATIO band BAND rounds R placements K metric M`' --profile nomeasured.fasprof main.fas prof_link.c
expect_error no-counts2 'error: profile `nocounts2.fasprof` has no `counts` section' --profile nocounts2.fasprof main.fas prof_link.c

"$OCAML_FAS" --profile main.fasprof --keep main.fas prof_link.c -o keep.bin >keep.bin.out 2>keep.bin.err || fail "--keep profile build failed: $(cat keep.bin.err)"
kept=$(grep '^fas: kept intermediates:' keep.bin.err) || fail "--keep printed no intermediates line"
case "$kept" in
  *fas-profile-*.proftext*) ;;
  *) fail "--keep does not list the text profile: $kept" ;;
esac
case "$kept" in
  *fas-profile-*.profdata*) ;;
  *) fail "--keep does not list the indexed profile: $kept" ;;
esac
grep -q -F -e "FAS_PROFDATA=$PROFDATA;" keep.bin.err || fail "--keep does not report FAS_PROFDATA=$PROFDATA"
for kept_temp in "$TMPDIR"/fas-profile-*.proftext "$TMPDIR"/fas-profile-*.profdata; do
  [ -f "$kept_temp" ] || fail "--keep did not keep $kept_temp"
done

echo "profile_build: passed"
