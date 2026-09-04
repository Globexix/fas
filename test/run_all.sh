#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CONTAINER=${FAS_OCAML_CONTAINER:-fas-ocaml-stage3}
DOCKER=${DOCKER:-docker}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}

cd "$ROOT"
for tool in "$DOCKER" "$LLVM_OPT" "$LLVM_LLC" "$CC"; do
  if ! command -v "$tool" >/dev/null 2>&1 && [ ! -x "$tool" ]; then
    echo "validation: required tool is missing: $tool" >&2
    exit 2
  fi
done
"$DOCKER" inspect "$CONTAINER" >/dev/null 2>&1 || {
  echo "validation: persistent container $CONTAINER is unavailable" >&2
  exit 2
}
"$DOCKER" exec "$CONTAINER" sh -lc \
  'cd /work && eval $(opam env) && dune build --display=short @all && dune runtest --force --display=short'

"$OCAML_FAS" --emit-llvm test/ir_simple.fas >test/.stage3.ll
"$LLVM_OPT" -passes=verify test/.stage3.ll -disable-output
"$LLVM_LLC" test/.stage3.ll -o test/.stage3.s
rm -f test/.stage3.ll test/.stage3.s

LLVM_OPT="$LLVM_OPT" OCAML_FAS="$OCAML_FAS" "$ROOT/test/issue33_const_generic_struct.sh"
CC="$CC" LLVM_OPT="$LLVM_OPT" LLVM_LLC="$LLVM_LLC" OCAML_FAS="$OCAML_FAS" \
  "$ROOT/test/lexical_scopes.sh"
CC="$CC" LLVM_OPT="$LLVM_OPT" LLVM_LLC="$LLVM_LLC" OCAML_FAS="$OCAML_FAS" \
  "$ROOT/test/integer_vector_bitcasts.sh"
CC="$CC" LLVM_OPT="$LLVM_OPT" LLVM_LLC="$LLVM_LLC" OCAML_FAS="$OCAML_FAS" \
  "$ROOT/test/integer_vector_comparisons.sh"
CC="$CC" LLVM_OPT="$LLVM_OPT" LLVM_LLC="$LLVM_LLC" OCAML_FAS="$OCAML_FAS" \
  "$ROOT/test/integer_vector_shift_counts.sh"
CC="$CC" LLVM_OPT="$LLVM_OPT" LLVM_LLC="$LLVM_LLC" OCAML_FAS="$OCAML_FAS" \
  "$ROOT/test/defer_unwinding.sh"
CC="$CC" LLVM_OPT="$LLVM_OPT" LLVM_LLC="$LLVM_LLC" OCAML_FAS="$OCAML_FAS" \
  "$ROOT/test/opaque_types.sh"

LLVM_OPT="$LLVM_OPT" OCAML_FAS="$OCAML_FAS" "$ROOT/test/layout_oracle.sh"
CC="$CC" LLVM_OPT="$LLVM_OPT" OCAML_FAS="$OCAML_FAS" \
  "$ROOT/test/typed_pointer_alignment.sh"
CC="$CC" LLVM_OPT="$LLVM_OPT" OCAML_FAS="$OCAML_FAS" "$ROOT/test/abi_boundary.sh"
CC="$CC" LLVM_OPT="$LLVM_OPT" LLVM_LLC="$LLVM_LLC" OCAML_FAS="$OCAML_FAS" \
  "$ROOT/test/assembly_linkage.sh"
echo "validation: all mandatory gates passed"
