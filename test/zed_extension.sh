#!/bin/sh
set -eu
export DEBUGINFOD_URLS=

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
ZED_DIR="$ROOT/editors/zed"
GRAMMAR_DIR="$ROOT/editors/tree-sitter-fas"
GATE_TMP=$(mktemp -d)
trap 'rm -rf "$GATE_TMP"' EXIT HUP INT TERM

fail() {
  echo "zed extension: $*" >&2
  exit 1
}

command -v cargo >/dev/null 2>&1 || {
  echo "zed extension: skipped, cargo not found" >&2
  exit 0
}
if command -v rustup >/dev/null 2>&1 && ! rustup target list --installed 2>/dev/null | grep -qx wasm32-wasip2; then
  echo "zed extension: skipped, wasm32-wasip2 target not installed" >&2
  exit 0
fi

python3 -I - "$ZED_DIR" > "$GATE_TMP/pin" <<'PY' || fail "manifest checks failed"
import re
import sys
import tomllib

zed = sys.argv[1]


def load(name):
    with open(f"{zed}/{name}", "rb") as handle:
        return tomllib.load(handle)


def fail(message):
    print(f"zed extension: {message}", file=sys.stderr)
    sys.exit(1)


ext = load("extension.toml")
cargo = load("Cargo.toml")
lock = load("Cargo.lock")
cfg = load("languages/fas/config.toml")

if ext.get("id") != "fas" or ext.get("version") != "0.1.0":
    fail("extension id and version must be fas 0.1.0")
if ext.get("schema_version") != 1:
    fail("schema_version must be 1")
if ext.get("lib", {}).get("kind") != "Rust":
    fail("[lib] kind must be Rust")

api = cargo["dependencies"]["zed_extension_api"]
if ext["lib"].get("version") != api:
    fail(f"[lib] version must match zed_extension_api {api}")
locked = [pkg["version"] for pkg in lock["package"] if pkg["name"] == "zed_extension_api"]
if locked != [api]:
    fail(f"Cargo.lock must pin zed_extension_api {api}")

grammar = ext["grammars"]["fas"]
rev = grammar.get("rev", "")
if not re.fullmatch(r"[0-9a-f]{40}", rev):
    fail("grammar rev must be a full commit hash")
if not grammar.get("repository", "").startswith(("https://", "file://")):
    fail("grammar repository must be https or file")
if grammar.get("path") != "editors/tree-sitter-fas":
    fail("grammar path must be editors/tree-sitter-fas")

server = ext["language_servers"]["fas"]
if server.get("languages") != ["Fas"] or cfg.get("name") != "Fas":
    fail("language server must target the Fas language")
if cfg.get("grammar") != "fas" or cfg.get("path_suffixes") != ["fas"]:
    fail("languages/fas/config.toml must use grammar fas and suffix fas")
if cfg.get("line_comments") != ["// "]:
    fail("line comment must be '// '")

print(rev)
print(grammar["path"])
PY

REV=$(sed -n 1p "$GATE_TMP/pin")
GRAMMAR_PATH=$(sed -n 2p "$GATE_TMP/pin")

git -C "$ROOT" cat-file -e "$REV^{commit}" 2>/dev/null || fail "grammar rev $REV is not a commit"
git -C "$ROOT" cat-file -e "$REV:$GRAMMAR_PATH/grammar.js" 2>/dev/null ||
  fail "grammar rev $REV has no $GRAMMAR_PATH/grammar.js"

git init -q "$GATE_TMP/checkout"
git -C "$GATE_TMP/checkout" fetch -q --depth 1 "file://$ROOT" "$REV" ||
  fail "file:// fetch of grammar rev $REV failed"
git -C "$GATE_TMP/checkout" checkout -q FETCH_HEAD
[ -f "$GATE_TMP/checkout/$GRAMMAR_PATH/grammar.js" ] ||
  fail "file:// checkout lacks $GRAMMAR_PATH/grammar.js"

CC_TOOL=${CC:-clang-22}
for unit in parser scanner; do
  "$CC_TOOL" -O2 -fPIC -c -I "$GATE_TMP/checkout/$GRAMMAR_PATH/src" \
    -o "$GATE_TMP/$unit.o" "$GATE_TMP/checkout/$GRAMMAR_PATH/src/$unit.c" ||
    fail "grammar $unit.c at $REV does not compile"
  nm -u "$GATE_TMP/$unit.o" | awk '{print $NF}' >> "$GATE_TMP/imports"
  nm --defined-only "$GATE_TMP/$unit.o" | awk '{print $NF}' >> "$GATE_TMP/defined"
done
LC_ALL=C sort -u "$GATE_TMP/defined" > "$GATE_TMP/defined.sorted"
LC_ALL=C sort -u "$GATE_TMP/imports" | LC_ALL=C comm -23 - "$GATE_TMP/defined.sorted" | grep -vxE 'malloc|calloc|realloc|free|memchr|memcmp|memcpy|memmove|memset|strcmp|strlen|strncat|strncmp|strncpy|towlower|towupper|isw(alnum|alpha|blank|digit|lower|space|upper|xdigit)|abort|_GLOBAL_OFFSET_TABLE_' \
  > "$GATE_TMP/bad_imports" || true
[ ! -s "$GATE_TMP/bad_imports" ] ||
  fail "grammar at $REV imports symbols Zed's wasm runtime lacks: $(tr '\n' ' ' < "$GATE_TMP/bad_imports")"

for name in brackets highlights indents outline; do
  dest="$ZED_DIR/languages/fas/$name.scm"
  src="$GRAMMAR_DIR/queries/$name.scm"
  [ ! -L "$dest" ] || fail "$dest is a symlink"
  [ -f "$dest" ] || fail "$dest is missing"
  cmp -s "$src" "$dest" || fail "$dest differs from $src"
done

find "$ZED_DIR" \( -type l -o -name target -o -name node_modules -o -name '*.wasm' \) -print > "$GATE_TMP/forbidden"
[ ! -s "$GATE_TMP/forbidden" ] || {
  cat "$GATE_TMP/forbidden" >&2
  fail "forbidden entries in editors/zed"
}

CARGO_TARGET_DIR="$GATE_TMP/target" cargo build --manifest-path "$ZED_DIR/Cargo.toml" \
  --target wasm32-wasip2 --release --locked --quiet || fail "wasm build failed"
WASM="$GATE_TMP/target/wasm32-wasip2/release/zed_fas.wasm"
[ -f "$WASM" ] || fail "wasm build produced no zed_fas.wasm"
magic=$(head -c 4 "$WASM" | od -An -tx1 | tr -d ' \n')
[ "$magic" = 0061736d ] || fail "wasm magic is $magic, expected 0061736d"

grep -q 'cmd = { "fas", "lsp" }' "$ROOT/editors/neovim/fas.lua" ||
  fail "editors/neovim/fas.lua does not start fas lsp"
if command -v luac >/dev/null 2>&1; then
  luac -p "$ROOT/editors/neovim/fas.lua" || fail "editors/neovim/fas.lua has a syntax error"
else
  echo "zed extension: luac not found, skipped Lua syntax check" >&2
fi

echo "zed extension: grammar $REV, wasm $(wc -c < "$WASM" | tr -d ' ') bytes, 4 query copies identical, file:// checkout ok"
