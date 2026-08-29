#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt}
LLVM_LLC=${LLVM_LLC:-llc}
CC=${CC:-clang}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ASSEMBLY_TMP=$(mktemp -d)
trap 'rm -rf "$ASSEMBLY_TMP"' EXIT HUP INT TERM

for level in 0 2; do
    FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
        "$OCAML_FAS" -O"$level" "$ROOT/test/assembly_linkage.fas" \
        -o "$ASSEMBLY_TMP/linkage-$level"
    "$ASSEMBLY_TMP/linkage-$level"
done

echo "assembly linkage: ok"
