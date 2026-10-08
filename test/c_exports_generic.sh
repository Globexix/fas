#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
GENERIC_EXPORT_TMP=$(mktemp -d)
trap 'rm -rf "$GENERIC_EXPORT_TMP"' EXIT HUP INT TERM

cat >"$GENERIC_EXPORT_TMP/non_generic.fas" <<'FAS'
struct B4 { value arr[4,u8] }
extern "C" {
  var item B4 = {{1,2,3,4}}
  fn read_item() i32 { return item.value[0] }
}
FAS

"$OCAML_FAS" --emit-header -o "$GENERIC_EXPORT_TMP/non_generic.h" \
  "$GENERIC_EXPORT_TMP/non_generic.fas"
printf '#include "non_generic.h"\nint main(void) { return item.value[0] != 1; }\n' \
  >"$GENERIC_EXPORT_TMP/check.c"
"$CC" -std=c11 -Wall -Wextra -Werror -pedantic \
  -I"$GENERIC_EXPORT_TMP" -fsyntax-only "$GENERIC_EXPORT_TMP/check.c"

echo 'c_exports_generic: non-generic twin header passes strict C11: ok'
