#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
IR_JSON_TMP=$(mktemp -d)
trap 'rm -rf "$IR_JSON_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-ir-json "$ROOT/test/ir_json.fas" >"$IR_JSON_TMP/out.json" 2>"$IR_JSON_TMP/stderr"
[ ! -s "$IR_JSON_TMP/stderr" ]
"$OCAML_FAS" --emit-ir-json "$ROOT/test/ir_json.fas" -o "$IR_JSON_TMP/file.json"
cmp "$IR_JSON_TMP/out.json" "$IR_JSON_TMP/file.json"
"$OCAML_FAS" --emit-ir "$ROOT/test/ir_json.fas" >"$IR_JSON_TMP/out.ir"

python3 - "$IR_JSON_TMP/out.json" "$IR_JSON_TMP/out.ir" <<'PY'
import json, re, sys
m = json.load(open(sys.argv[1]))
dump = open(sys.argv[2]).read()
assert [s["name"] for s in m["structs"]] == ["Pair"], m["structs"]
assert m["structs"][0]["fields"] == ["i32", "i32"]
globs = {g["name"]: g for g in m["globals"]}
assert len(globs) == 2
assert any(g["kind"] == "array" and g["elems"] == [7, -2, 9] and g["elem"] == "i32" for g in globs.values()) or \
    any(g["kind"] == "storage" and g["size"] == 12 for g in globs.values())
names = [f["name"] for f in m["funcs"]]
assert names == re.findall(r'Function \{ name = "([^"]+)"', dump), names
for f in m["funcs"]:
    want = re.search(r'Function \{ name = "%s".*?\n    \] \}' % re.escape(f["name"]), dump, re.S).group(0)
    lines = [l.strip() for l in want.splitlines()]
    instrs = [l for l in lines if l.startswith("%") or l.startswith("store ") or l.startswith("call ")]
    terms = [l for l in lines if l.startswith("terminator = ")]
    assert sum(len(b["instrs"]) for b in f["blocks"]) == len(instrs), f["name"]
    assert len(f["blocks"]) == len(terms), f["name"]
pick = next(f for f in m["funcs"] if f["name"] == "pick")
ops = [i[0] for b in pick["blocks"] for i in b["instrs"]]
kinds = [b["term"][0] for b in pick["blocks"]]
assert "switch" in kinds and "condbr" in kinds and "ret" in kinds, kinds
sw = next(b["term"] for b in pick["blocks"] if b["term"][0] == "switch")
assert sw[1] == "i32" and [c[0] for c in sw[3]] == [1], sw
bins = [i[2] for b in pick["blocks"] for i in b["instrs"] if i[0] == "bin"]
assert "srem" in bins and "add" in bins, bins
cmps = [i[2] for b in pick["blocks"] for i in b["instrs"] if i[0] == "cmp"]
assert "slt" in cmps and "ne" in cmps, cmps
assert "alloca" in ops and "load" in ops and "store" in ops, ops
main = next(f for f in m["funcs"] if f["name"] == "main")
calls = [i for b in main["blocks"] for i in b["instrs"] if i[0] == "call"]
assert calls and calls[0][4] == "pick" and [a[2] for a in calls[0][5]] == [["const", "i32", 3], ["const", "i32", 5]], calls
PY

echo "IR JSON: matches the custom IR dump: ok"
