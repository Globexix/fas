#!/bin/sh
set -eu

ROOT=${FAS_ROOT:-/home/glonex/search/fas}
PROJECT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-${FAS_OCAML:-$PROJECT/_build/default/bin/main.exe}}
RUST_FAS=${RUST_FAS:-$ROOT/compiler/target/debug/fas}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
RUNS=${RUNS:-3}
WORK=${WORK:-$(mktemp -d)}
trap 'rm -rf "$WORK"' EXIT

for tool in "$RUST_FAS" "$OCAML_FAS" "$LLVM_OPT" "$LLVM_LLC" "$CC"; do
  if ! command -v "$tool" >/dev/null 2>&1 && [ ! -x "$tool" ]; then
    echo "finder gate: missing $tool" >&2
    exit 2
  fi
done
[ -f "$ROOT/finder/finder.fas" ] || {
  echo "finder gate: missing finder source" >&2
  exit 2
}

compile_one() {
  compiler=$1
  tag=$2
  FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
    "$compiler" "$ROOT/finder/finder.fas" -o "$WORK/$tag" \
    ${FAS_TARGET_FLAGS:-}
  FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
    "$compiler" --emit-llvm "$ROOT/finder/finder.fas" \
    ${FAS_TARGET_FLAGS:-} >"$WORK/$tag.ll"
  FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
    "$compiler" --emit-asm "$ROOT/finder/finder.fas" -o "$WORK/$tag.emit" \
    ${FAS_TARGET_FLAGS:-} >"$WORK/$tag.s"
  if [ ! -s "$WORK/$tag.s" ] && [ -s "$WORK/$tag.emit.s" ]; then
    cp "$WORK/$tag.emit.s" "$WORK/$tag.s"
  fi
  "$LLVM_OPT" '-passes=default<O2>' -verify-each "$WORK/$tag.ll" -S -o "$WORK/$tag.opt.ll"
  "$LLVM_LLC" -O2 "$WORK/$tag.opt.ll" -o "$WORK/$tag.opt.s"
}

compile_one "$RUST_FAS" rust
compile_one "$OCAML_FAS" ocaml
for artifact in "$WORK/rust.ll" "$WORK/ocaml.ll" "$WORK/rust.s" "$WORK/ocaml.s"; do
  [ -s "$artifact" ] || {
    echo "finder gate: empty generated artifact $artifact" >&2
    exit 1
  }
done
grep -q '^k8:' "$WORK/ocaml.s" || {
  echo "finder gate: raw asm function k8 was not retained" >&2
  exit 1
}

for tag in rust ocaml; do
  FB_CW=1 "$WORK/$tag" -s 1 -p "$ROOT/finder/test_pattern.txt" -w 600 -h 600 >"$WORK/$tag.out"
done
cmp "$WORK/rust.out" "$WORK/ocaml.out"
FB_CW=1 "$WORK/ocaml" -s 1 -p "$ROOT/finder/test_pattern.txt" -w 600 -h 600 >"$WORK/ocaml.repeat.out"
cmp "$WORK/ocaml.out" "$WORK/ocaml.repeat.out"

if [ -x "$ROOT/bench/bench.sh" ]; then
  RUNS="$RUNS" "$ROOT/bench/bench.sh" "$WORK/rust" "$WORK/ocaml" 256
else
  echo "finder gate: canonical bench harness is unavailable" >&2
  exit 2
fi

echo "finder gate: correctness, LLVM verification, assembly retention, and benchmark completed"
