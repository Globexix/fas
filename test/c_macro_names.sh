#!/bin/sh
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

fail() {
  echo "c_macro_names: $*" >&2
  exit 1
}

cat >"$TMP/macros.h" <<'C'
#define SAME_I 7
#define SAME_F 2.5
C

cat >"$TMP/same-int.fas" <<'FAS'
use "C" "macros.h"
const SAME_I i32 = 1
fn main() i32 { return SAME_I }
FAS
cat >"$TMP/same-float.fas" <<'FAS'
use "C" "macros.h"
fn SAME_F() f64 { return 1.0 }
fn main() i32 { return 0 }
FAS
cat >"$TMP/lib.fas" <<'FAS'
use "C" "macros.h"
FAS
cat >"$TMP/cross-int.fas" <<'FAS'
use "lib.fas"
const SAME_I i32 = 1
fn main() i32 { return SAME_I }
FAS
cat >"$TMP/cross-float.fas" <<'FAS'
use "lib.fas"
fn SAME_F() f64 { return 1.0 }
fn main() i32 { return 0 }
FAS

for source in same-int same-float cross-int cross-float; do
  if "$OCAML_FAS" --emit-llvm "$TMP/$source.fas" -o "$TMP/$source.ll" \
    >"$TMP/$source.out" 2>&1; then
    fail "$source macro conflict was accepted"
  fi
  grep -F 'duplicate declaration' "$TMP/$source.out" >/dev/null \
    || fail "$source did not report a duplicate declaration"
  test ! -e "$TMP/$source.ll" || fail "$source created output after rejection"
done

cat >"$TMP/accepted.fas" <<'FAS'
use "C" "macros.h"
fn main() i32 {
  integer i32 = SAME_I
  floating f64 = SAME_F
  return integer - 7
}
FAS
"$OCAML_FAS" -O0 "$TMP/accepted.fas" -o "$TMP/accepted"
timeout 30 "$TMP/accepted" || fail "accepted integer and float macros changed values"

cat >"$TMP/no-header.fas" <<'FAS'
fn main() i32 { return 0 }
FAS
cat >"$TMP/cc-wrap" <<'SH'
#!/bin/sh
printf '%s\n' "$*" >>"$TOOL_LOG"
exec "$REAL_CC" "$@"
SH
chmod +x "$TMP/cc-wrap"
: >"$TMP/cc.log"
TOOL_LOG="$TMP/cc.log" REAL_CC="$CC" CC="$TMP/cc-wrap" \
  "$OCAML_FAS" --emit-llvm "$TMP/no-header.fas" >"$TMP/no-header.ll"
test ! -s "$TMP/cc.log" || fail "no-header program ran extra C preprocessing"

echo 'c_macro_names: declaration conflicts, accepted macros, and no-header imports: ok'
