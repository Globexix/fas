#!/bin/sh
set -eu
ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
python3 "$ROOT/test/reproducible_objects.py" "$ROOT" "$OCAML_FAS"
