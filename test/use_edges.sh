#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
USE_EDGES_TMP=$(mktemp -d)
trap 'rm -rf "$USE_EDGES_TMP"' EXIT HUP INT TERM

fail() {
  echo "use_edges: $*" >&2
  exit 1
}

expect_failure() {
  status=0
  "$@" >"$USE_EDGES_TMP/stdout" 2>"$USE_EDGES_TMP/stderr" || status=$?
  [ "$status" -ne 0 ] || fail "command unexpectedly succeeded: $*"
  [ ! -s "$USE_EDGES_TMP/stdout" ] || fail "failure wrote to stdout: $*"
}

mkdir -p "$USE_EDGES_TMP/project/deps" "$USE_EDGES_TMP/canonical/nested"
cat >"$USE_EDGES_TMP/project/a-main.fas" <<'FAS'
use "deps/left.fas"
use "deps/right.fas"
fn main() i32 { return left() + right() - 46 }
FAS
cat >"$USE_EDGES_TMP/project/a-main-reversed.fas" <<'FAS'
use "deps/right.fas"
use "deps/left.fas"
fn main() i32 { return left() + right() - 46 }
FAS
cat >"$USE_EDGES_TMP/project/deps/left.fas" <<'FAS'
use "leaf.fas"
fn left() i32 { return leaf() + 3 }
FAS
cat >"$USE_EDGES_TMP/project/deps/right.fas" <<'FAS'
use "leaf.fas"
fn right() i32 { return leaf() + 5 }
FAS
cat >"$USE_EDGES_TMP/project/deps/leaf.fas" <<'FAS'
fn leaf() i32 { return 19 }
FAS
for level in 0 2; do
  "$OCAML_FAS" -O"$level" --emit-llvm "$USE_EDGES_TMP/project/a-main.fas" \
    >"$USE_EDGES_TMP/project-$level.ll"
  "$LLVM_OPT" -passes=verify "$USE_EDGES_TMP/project-$level.ll" -disable-output
  "$LLVM_OPT" -passes="default<O$level>" -verify-each \
    "$USE_EDGES_TMP/project-$level.ll" -S -o "$USE_EDGES_TMP/project-$level-opt.ll"
  "$LLVM_OPT" -passes=verify "$USE_EDGES_TMP/project-$level-opt.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$USE_EDGES_TMP/project/a-main.fas" \
    -o "$USE_EDGES_TMP/project-$level"
  "$USE_EDGES_TMP/project-$level" || fail "transitive diamond failed at O$level"
done

"$OCAML_FAS" --emit-llvm "$USE_EDGES_TMP/project/a-main.fas" \
  >"$USE_EDGES_TMP/use-forward.ll"
"$OCAML_FAS" --emit-llvm "$USE_EDGES_TMP/project/a-main-reversed.fas" \
  >"$USE_EDGES_TMP/use-reverse.ll"
cmp "$USE_EDGES_TMP/use-forward.ll" "$USE_EDGES_TMP/use-reverse.ll" \
  >/dev/null || fail "LLVM output changed with use declaration order"

cat >"$USE_EDGES_TMP/canonical/root.fas" <<'FAS'
use "real.fas"
use "alias.fas"
use "nested/../real.fas"
fn main() i32 { return unique() - 4 }
FAS
cat >"$USE_EDGES_TMP/canonical/real.fas" <<'FAS'
fn unique() i32 { return 4 }
FAS
ln -s real.fas "$USE_EDGES_TMP/canonical/alias.fas"
"$OCAML_FAS" --emit-llvm "$USE_EDGES_TMP/canonical/root.fas" \
  >"$USE_EDGES_TMP/canonical.ll"
"$LLVM_OPT" -passes=verify "$USE_EDGES_TMP/canonical.ll" -disable-output
"$OCAML_FAS" "$USE_EDGES_TMP/canonical/root.fas" -o "$USE_EDGES_TMP/canonical-run"
"$USE_EDGES_TMP/canonical-run" || fail "symlink and parent path were not deduplicated"

mkdir -p "$USE_EDGES_TMP/cycles/two" "$USE_EDGES_TMP/cycles/three"
cat >"$USE_EDGES_TMP/cycles/two/a.fas" <<'FAS'
use "b.fas"
fn a() i32 { return b() }
FAS
cat >"$USE_EDGES_TMP/cycles/two/b.fas" <<'FAS'
use "a.fas"
fn b() i32 { return 1 }
FAS
cat >"$USE_EDGES_TMP/cycles/two/main.fas" <<'FAS'
use "a.fas"
fn main() i32 { return a() - 1 }
FAS
cat >"$USE_EDGES_TMP/cycles/three/a.fas" <<'FAS'
use "b.fas"
fn a() i32 { return b() }
FAS
cat >"$USE_EDGES_TMP/cycles/three/b.fas" <<'FAS'
use "c.fas"
fn b() i32 { return c() }
FAS
cat >"$USE_EDGES_TMP/cycles/three/c.fas" <<'FAS'
use "a.fas"
fn c() i32 { return 1 }
FAS
cat >"$USE_EDGES_TMP/cycles/three/main.fas" <<'FAS'
use "a.fas"
fn main() i32 { return a() - 1 }
FAS
cat >"$USE_EDGES_TMP/cycles/self.fas" <<'FAS'
use "self.fas"
fn main() i32 { return 0 }
FAS
for source in "$USE_EDGES_TMP/cycles/two/main.fas" \
  "$USE_EDGES_TMP/cycles/three/main.fas" "$USE_EDGES_TMP/cycles/self.fas"; do
  "$OCAML_FAS" --emit-llvm "$source" >"$USE_EDGES_TMP/cycle.ll"
  "$LLVM_OPT" -passes=verify "$USE_EDGES_TMP/cycle.ll" -disable-output
done
for level in 0 2; do
  "$OCAML_FAS" -O"$level" "$USE_EDGES_TMP/cycles/two/main.fas" \
    -o "$USE_EDGES_TMP/cycle-two-$level"
  "$USE_EDGES_TMP/cycle-two-$level" || fail "2-file cycle failed at O$level"
  "$OCAML_FAS" -O"$level" "$USE_EDGES_TMP/cycles/three/main.fas" \
    -o "$USE_EDGES_TMP/cycle-three-$level"
  "$USE_EDGES_TMP/cycle-three-$level" || fail "3-file cycle failed at O$level"
done

mkdir -p "$USE_EDGES_TMP/missing/sub"
cat >"$USE_EDGES_TMP/missing/a.fas" <<'FAS'
use "sub/b.fas"
FAS
cat >"$USE_EDGES_TMP/missing/sub/b.fas" <<'FAS'
use "gone.fas"
FAS
expect_failure "$OCAML_FAS" "$USE_EDGES_TMP/missing/a.fas"
grep -F "include chain: $USE_EDGES_TMP/missing/a.fas -> $USE_EDGES_TMP/missing/sub/b.fas -> $USE_EDGES_TMP/missing/sub/gone.fas" \
  "$USE_EDGES_TMP/stderr" >/dev/null || fail "missing dependency omitted include chain"

mkdir -p "$USE_EDGES_TMP/duplicates"
cat >"$USE_EDGES_TMP/duplicates/root.fas" <<'FAS'
use "one.fas"
use "two.fas"
FAS
cat >"$USE_EDGES_TMP/duplicates/one.fas" <<'FAS'
fn shared() i32 { return 1 }
FAS
cat >"$USE_EDGES_TMP/duplicates/two.fas" <<'FAS'
fn shared() i64 { return 2 }
FAS
expect_failure "$OCAML_FAS" "$USE_EDGES_TMP/duplicates/root.fas"
grep -F "first definition is at $USE_EDGES_TMP/duplicates/one.fas:1:1" \
  "$USE_EDGES_TMP/stderr" >/dev/null || fail "duplicate omitted the first definition site"
grep -F "$USE_EDGES_TMP/duplicates/two.fas:1:1: error: duplicate function \`shared\`" \
  "$USE_EDGES_TMP/stderr" >/dev/null || fail "duplicate omitted the second definition site"
grep -F "include chain: $USE_EDGES_TMP/duplicates/root.fas -> $USE_EDGES_TMP/duplicates/one.fas" \
  "$USE_EDGES_TMP/stderr" >/dev/null || fail "duplicate omitted first include chain"
grep -F "include chain: $USE_EDGES_TMP/duplicates/root.fas -> $USE_EDGES_TMP/duplicates/two.fas" \
  "$USE_EDGES_TMP/stderr" >/dev/null || fail "duplicate omitted second include chain"

mkdir -p "$USE_EDGES_TMP/paths/directory.fas"
cat >"$USE_EDGES_TMP/paths/absolute.fas" <<FAS
use "$USE_EDGES_TMP/project/deps/leaf.fas"
FAS
cat >"$USE_EDGES_TMP/paths/extension.fas" <<'FAS'
use "library.FAS"
FAS
cat >"$USE_EDGES_TMP/paths/header.fas" <<'FAS'
use "library.h"
FAS
cat >"$USE_EDGES_TMP/paths/directory.fas/root.fas" <<'FAS'
use "../directory.fas"
FAS
expect_failure "$OCAML_FAS" "$USE_EDGES_TMP/paths/absolute.fas"
grep -F "absolute Fas dependency paths are not supported; use a path relative to this file" \
  "$USE_EDGES_TMP/stderr" >/dev/null || fail "absolute dependency diagnostic changed"
expect_failure "$OCAML_FAS" "$USE_EDGES_TMP/paths/extension.fas"
grep -F 'Fas dependency paths must end in lowercase `.fas`; C headers use `use "C"` in v0.2' \
  "$USE_EDGES_TMP/stderr" >/dev/null || fail "extension diagnostic changed"
expect_failure "$OCAML_FAS" "$USE_EDGES_TMP/paths/header.fas"
grep -F 'Fas dependency paths must end in lowercase `.fas`; C headers use `use "C"` in v0.2' \
  "$USE_EDGES_TMP/stderr" >/dev/null || fail "C path guidance is missing"
expect_failure "$OCAML_FAS" "$USE_EDGES_TMP/paths/directory.fas/root.fas"
grep -F "Fas dependency is a directory:" "$USE_EDGES_TMP/stderr" >/dev/null \
  || fail "directory dependency diagnostic changed"
cat >"$USE_EDGES_TMP/paths/c-import.fas" <<'FAS'
use "C" "library.h"
FAS
expect_failure "$OCAML_FAS" "$USE_EDGES_TMP/paths/c-import.fas"
grep -Fx "$USE_EDGES_TMP/paths/c-import.fas:1:1: error: use \"C\" is not implemented until v0.2" \
  "$USE_EDGES_TMP/stderr" >/dev/null || fail "use C rejection diagnostic changed"

mkdir -p "$USE_EDGES_TMP/library"
cat >"$USE_EDGES_TMP/library/api.fas" <<'FAS'
use "impl.fas"
extern "C" {
  fn fas_library_api() i32 { return library_impl() }
}
FAS
cat >"$USE_EDGES_TMP/library/impl.fas" <<'FAS'
fn library_impl() i32 { return 37 }
FAS
cat >"$USE_EDGES_TMP/library/consumer.c" <<'C'
extern int fas_library_api(void);
int main(void) { return fas_library_api() == 37 ? 0 : 1; }
C
for level in 0 2; do
  "$OCAML_FAS" -O"$level" -c "$USE_EDGES_TMP/library/api.fas" \
    -o "$USE_EDGES_TMP/library-$level.o"
  "$CC" -Werror -std=c17 -O"$level" "$USE_EDGES_TMP/library/consumer.c" \
    "$USE_EDGES_TMP/library-$level.o" -o "$USE_EDGES_TMP/library-$level"
  "$USE_EDGES_TMP/library-$level" || fail "no-main Fas closure failed at O$level"
done

echo "use_edges: passed"
