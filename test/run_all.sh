#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
START_TIME=$(date +%s)

cd "$ROOT"
for tool in "$LLVM_OPT" "$LLVM_LLC" "$CC"; do
  if ! command -v "$tool" >/dev/null 2>&1 && [ ! -x "$tool" ]; then
    echo "validation: required tool is missing: $tool" >&2
    exit 2
  fi
done

if [ "${FAS_OCAML_CONTAINER+x}" = x ]; then
  CONTAINER=$FAS_OCAML_CONTAINER
  DOCKER=${DOCKER:-docker}
  if ! command -v "$DOCKER" >/dev/null 2>&1 && [ ! -x "$DOCKER" ]; then
    echo "validation: required tool is missing: $DOCKER" >&2
    exit 2
  fi
  "$DOCKER" inspect "$CONTAINER" >/dev/null 2>&1 || {
    echo "validation: persistent container $CONTAINER is unavailable" >&2
    exit 2
  }
  "$DOCKER" exec "$CONTAINER" sh -lc \
    'cd /work && eval $(opam env) && dune build --display=short @all && dune build --display=short @fmt && dune runtest --force --display=short'
else
  dune build @all
  dune build @fmt
  dune runtest --force
fi

"$OCAML_FAS" --emit-llvm test/ir_simple.fas >test/.stage3.ll
"$LLVM_OPT" -passes=verify test/.stage3.ll -disable-output
"$LLVM_LLC" test/.stage3.ll -o test/.stage3.s
rm -f test/.stage3.ll test/.stage3.s

COMPONENTS=0
for component in "$ROOT"/test/*.sh; do
  [ -f "$component" ] || continue
  [ "$component" = "$ROOT/test/run_all.sh" ] && continue
  COMPONENTS=$((COMPONENTS + 1))
  echo "validation: running ${component#"$ROOT"/}"
  CC="$CC" LLVM_OPT="$LLVM_OPT" LLVM_LLC="$LLVM_LLC" OCAML_FAS="$OCAML_FAS" \
    sh "$component"
done

END_TIME=$(date +%s)
echo "validation: all $COMPONENTS component scripts passed in $((END_TIME - START_TIME)) seconds"
