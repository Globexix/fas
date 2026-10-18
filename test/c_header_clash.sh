#!/bin/sh
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
case $OCAML_FAS in
  /*) ;;
  *) OCAML_FAS=$(pwd)/$OCAML_FAS ;;
esac
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

fail() {
  echo "c_header_clash: $*" >&2
  exit 1
}

expect_clash() {
  name=$1
  source=$2
  output=$TMP/$name.out
  if "$OCAML_FAS" check "$source" >"$output" 2>&1; then
    fail "$name clash with a C import was accepted"
  fi
  first=$(head -n 1 "$output")
  [ "$first" = "$source:3:1: error: duplicate declaration \`$name\`" ] \
    || fail "$name: unexpected first line: $first"
  note=$(grep -F "note: \`$name\` is also declared by \`<stdio.h>\` at " "$output") \
    || fail "$name: missing note naming <stdio.h>"
  printf '%s\n' "$note" | grep -Eq ' at /[^ ]+:[0-9]+$' \
    || fail "$name: note has no header file and line: $note"
}

cd "$TMP"

cat >clash-function.fas <<'FAS'
use "C" <stdio.h>

fn remove(x u64) u64 {
    return x + 1
}

fn main(argc i32, argv addr) i32 {
    return 0
}
FAS
cat >clash-global.fas <<'FAS'
use "C" <stdio.h>

var stdin i32 = 1

fn main(argc i32, argv addr) i32 {
    return stdin
}
FAS
cat >clash-struct-tag.fas <<'FAS'
use "C" <stdio.h>

struct FILE {
    x i32
}

fn main(argc i32, argv addr) i32 {
    return 0
}
FAS
cat >clash-macro-const.fas <<'FAS'
use "C" <stdio.h>

const EOF i32 = 1

fn main(argc i32, argv addr) i32 {
    return EOF
}
FAS

expect_clash remove "$ROOT/test/c_header_clash.fas"
expect_clash remove clash-function.fas
expect_clash stdin clash-global.fas
expect_clash FILE clash-struct-tag.fas
expect_clash EOF clash-macro-const.fas

cat >clash-block.fas <<'FAS'
use "C" <<C
int remove_it(int x);
C

fn remove_it(x i32) i32 { return x }
fn main() i32 { return 0 }
FAS
if "$OCAML_FAS" check clash-block.fas >clash-block.out 2>&1; then
  fail "clash with a C block was accepted"
fi
grep -Fx 'clash-block.fas:5:1: error: duplicate declaration `remove_it`' clash-block.out >/dev/null \
  || fail "C block clash lost its message: $(cat clash-block.out)"
grep -Fx 'note: `remove_it` is also declared by the C block at clash-block.fas:1:1' clash-block.out >/dev/null \
  || fail "C block clash lacks the block note: $(cat clash-block.out)"

cat >same-file.fas <<'FAS'
fn twice(x i32) i32 { return x }
fn twice(x i32) i32 { return x }
fn main() i32 { return 0 }
FAS
if "$OCAML_FAS" check same-file.fas >same-file.out 2>&1; then
  fail "same-file duplicate was accepted"
fi
grep -F 'duplicate function `twice`' same-file.out >/dev/null \
  || fail "same-file duplicate lost its message"
grep -F 'also declared by' same-file.out >/dev/null \
  && fail "same-file duplicate gained a C header note"

echo 'c_header_clash: function, global, struct tag, macro and C block clashes name their source: ok'
