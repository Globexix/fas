#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/test/layout_diff.py"
