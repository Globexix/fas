#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/test/abi_matrix.py"
