#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt}
LLVM_LLC=${LLVM_LLC:-llc}
CC=${CC:-clang}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ASSEMBLY_TMP=$(mktemp -d)
trap 'rm -rf "$ASSEMBLY_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/assembly_linkage.fas" >"$ASSEMBLY_TMP/linkage.ll"
"$LLVM_OPT" -passes=verify "$ASSEMBLY_TMP/linkage.ll" -disable-output
"$LLVM_OPT" -passes='default<O2>' -verify-each "$ASSEMBLY_TMP/linkage.ll" -S -o "$ASSEMBLY_TMP/linkage-o2.ll"
"$LLVM_OPT" -passes=verify "$ASSEMBLY_TMP/linkage-o2.ll" -disable-output
"$CC" -Werror -Wno-override-module -std=c17 -S -emit-llvm -O0 "$ROOT/test/assembly_linkage.c" -o "$ASSEMBLY_TMP/clang.ll"
python3 "$ROOT/test/assembly_abi_parity.py" "$ASSEMBLY_TMP/linkage.ll" "$ASSEMBLY_TMP/clang.ll"

for level in 0 2; do
    FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
        "$OCAML_FAS" -O"$level" -c "$ROOT/test/assembly_linkage.fas" \
        -o "$ASSEMBLY_TMP/linkage-$level.o"
    "$CC" -Werror -std=c17 -O"$level" "$ASSEMBLY_TMP/linkage-$level.o" \
        "$ROOT/test/assembly_linkage.c" -o "$ASSEMBLY_TMP/linkage-$level"
    "$ASSEMBLY_TMP/linkage-$level"
done

echo "assembly linkage: ok"
