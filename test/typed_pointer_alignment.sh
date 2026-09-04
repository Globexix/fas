#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ALIGNMENT_TMP=$(mktemp -d)
trap 'rm -rf "$ALIGNMENT_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/typed_pointer_alignment.fas" \
  >"$ALIGNMENT_TMP/alignment.ll"
"$LLVM_OPT" -passes=verify "$ALIGNMENT_TMP/alignment.ll" -disable-output

sed -n '/define internal <8 x i64> @read_lanes/,/^}/p' \
  "$ALIGNMENT_TMP/alignment.ll" | grep -E 'load <8 x i64>, ptr .*, align 64' >/dev/null
sed -n '/define internal void @write_lanes/,/^}/p' \
  "$ALIGNMENT_TMP/alignment.ll" | grep -E 'store <8 x i64> .*, ptr .*, align 64' >/dev/null

for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$ALIGNMENT_TMP/alignment.ll" \
    -o "$ALIGNMENT_TMP/alignment-$level.ll"
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$ALIGNMENT_TMP/alignment-$level.ll" "$ROOT/test/typed_pointer_alignment.c" \
    -o "$ALIGNMENT_TMP/alignment-$level"
  "$ALIGNMENT_TMP/alignment-$level"
done

echo "typed pointer alignment: ok"
