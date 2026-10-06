#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ulimit -v 4194304
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

for kind in functions structs enums; do
  for count in 1250 5000; do
    CASE="$TMP/$kind-$count"
    mkdir -p "$CASE"
    python3 - "$CASE" "$kind" "$count" <<'PY'
from pathlib import Path
import sys

directory = Path(sys.argv[1])
kind = sys.argv[2]
count = int(sys.argv[3])
if kind == "functions":
    declarations = [f"int f{i}(int);" for i in range(count)]
elif kind == "structs":
    declarations = [f"struct s{i} {{ int a; char b; }};" for i in range(count)]
else:
    declarations = [f"enum e{i} {{ E{i}a, E{i}b }};" for i in range(count)]
(directory / "h.h").write_text("\n".join(declarations) + "\n")
(directory / "main.fas").write_text('use "C" "h.h"\nfn main() i32 { return 0 }\n')
PY
    best=
    run=0
    while [ "$run" -lt 3 ]; do
      start=$(date +%s%N)
      (
        ulimit -v 4194304
        CC="$CC" LLVM_OPT="$LLVM_OPT" LLVM_LLC="$LLVM_LLC" OCAML_FAS="$OCAML_FAS" \
          "$OCAML_FAS" --emit-llvm -I "$CASE" "$CASE/main.fas" >/dev/null
      )
      end=$(date +%s%N)
      elapsed=$((end - start))
      if [ -z "$best" ] || [ "$elapsed" -lt "$best" ]; then best=$elapsed; fi
      run=$((run + 1))
    done
    printf '%s\n' "$best" > "$TMP/$kind-$count.time"
  done
  small=$(cat "$TMP/$kind-1250.time")
  large=$(cat "$TMP/$kind-5000.time")
  ratio=$(awk -v small="$small" -v large="$large" 'BEGIN { printf "%.3f", large / small }')
  printf 'C import scaling %s: 1250=%s ns 5000=%s ns ratio=%s\n' "$kind" "$small" "$large" "$ratio"
  if awk -v ratio="$ratio" 'BEGIN { exit !(ratio > 8) }'; then
    printf 'C import scaling exceeded 8 for %s\n' "$kind" >&2
    exit 1
  fi
done
