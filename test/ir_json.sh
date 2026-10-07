#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
IR_JSON_TMP=$(mktemp -d)
trap 'rm -rf "$IR_JSON_TMP"' EXIT HUP INT TERM
mkdir "$IR_JSON_TMP/pairs"

emit_one() {
  mode=$1
  source=$2
  output=$3
  error=$4
  case "$source" in
    "$ROOT/test/assembly_linkage.fas")
      "$OCAML_FAS" -DASSEMBLY_TABLE_VALUE=42 "$mode" "$source" >"$output" 2>"$error"
      ;;
    "$ROOT/test/export_nested_headers.fas")
      "$OCAML_FAS" -I "$ROOT/test" "$mode" "$source" >"$output" 2>"$error"
      ;;
    *) "$OCAML_FAS" "$mode" "$source" >"$output" 2>"$error" ;;
  esac
}

emit_one --emit-ir-json "$ROOT/test/ir_json.fas" "$IR_JSON_TMP/out.json" \
  "$IR_JSON_TMP/stderr"
[ ! -s "$IR_JSON_TMP/stderr" ] || { cat "$IR_JSON_TMP/stderr" >&2; exit 1; }
emit_one --emit-ir-json "$ROOT/test/ir_json.fas" "$IR_JSON_TMP/repeat.json" \
  "$IR_JSON_TMP/stderr"
cmp "$IR_JSON_TMP/out.json" "$IR_JSON_TMP/repeat.json"
emit_one --emit-ir "$ROOT/test/ir_json.fas" "$IR_JSON_TMP/out.ir" "$IR_JSON_TMP/stderr"
"$OCAML_FAS" --emit-ir-json "$ROOT/test/ir_json.fas" -o "$IR_JSON_TMP/file.json"
cmp "$IR_JSON_TMP/out.json" "$IR_JSON_TMP/file.json"
"$OCAML_FAS" -g --no-inline pick --emit-ir-json "$ROOT/test/ir_json.fas" \
  >"$IR_JSON_TMP/noinline.json"

index=0
skipped=0
for source in "$ROOT"/test/*.fas; do
  dump="$IR_JSON_TMP/current.ir"
  json="$IR_JSON_TMP/current.json"
  if emit_one --emit-ir "$source" "$dump" "$IR_JSON_TMP/stderr"; then
    if ! emit_one --emit-ir-json "$source" "$json" "$IR_JSON_TMP/stderr"; then
      cat "$IR_JSON_TMP/stderr" >&2
      echo "IR JSON: rejected source accepted by --emit-ir: $source" >&2
      exit 1
    fi
    cp "$dump" "$IR_JSON_TMP/pairs/$index.ir"
    cp "$json" "$IR_JSON_TMP/pairs/$index.json"
    basename "$source" .fas >"$IR_JSON_TMP/pairs/$index.name"
    index=$((index + 1))
  else
    skipped=$((skipped + 1))
  fi
done

python3 - "$IR_JSON_TMP" "$index" "$skipped" <<'PY'
import json
import os
import re
import sys

root, count, skipped = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])

def read_json(path):
    with open(path, encoding="utf-8") as stream:
        return json.load(stream)

source_indexes = {}
for index in range(count):
    with open(os.path.join(root, "pairs", str(index) + ".name"), encoding="utf-8") as stream:
        source_indexes[stream.read().strip()] = index

def names_in_dump(text):
    result = []
    for line in text.splitlines():
        match = re.search(r'Function \{ name = "((?:\\.|[^"\\])*)";', line)
        if match:
            result.append(bytes(match.group(1), "utf-8").decode("unicode_escape"))
    return result

def ir_tag(line):
    line = line.strip()
    if line == "call void @llvm.trap()":
        return "trap"
    rhs = line.split(" = ", 1)[1] if " = " in line else line
    if rhs.startswith("getelementptr "):
        if "@.str." in rhs:
            return "string_ptr"
        if re.search(r", ptr @[^, ]+", rhs):
            return "global_ptr"
        return "gep"
    if rhs.startswith("load volatile "):
        return "load_volatile"
    if rhs.startswith("load "):
        return "load"
    if rhs.startswith("store volatile "):
        return "store_volatile"
    if rhs.startswith("store "):
        return "store"
    if rhs.startswith("icmp "):
        return "cmp"
    if rhs.startswith("alloca "):
        return "alloca"
    if rhs.startswith("call "):
        return "call"
    if rhs.startswith("phi "):
        return "phi"
    if rhs.startswith("select "):
        return "select"
    if rhs.startswith("extractelement "):
        return "extract"
    if rhs.startswith("insertelement "):
        return "insert"
    if rhs.startswith("shufflevector "):
        return "shuffle_zero" if rhs.endswith("zeroinitializer") else "shufflevector"
    if rhs.startswith(("zext ", "sext ", "trunc ", "bitcast ", "ptrtoint ",
                       "inttoptr ", "sitofp ", "uitofp ", "fptosi ", "fptoui ")):
        return "cast"
    if rhs.startswith(("add ", "sub ", "mul ", "sdiv ", "srem ", "udiv ",
                       "urem ", "and ", "or ", "xor ", "shl ", "lshr ", "ashr ")):
        return "bin"
    raise AssertionError("unrecognized --emit-ir instruction: " + line)

def term_tag(line):
    value = line.split("terminator = ", 1)[1].strip()
    if value.startswith("ret ") or value == "ret void":
        return "ret"
    if value.startswith("br i1 "):
        return "condbr"
    if value.startswith("br label "):
        return "br"
    if value.startswith("switch "):
        return "switch"
    if value == "unreachable":
        return "unreachable"
    raise AssertionError("unrecognized --emit-ir terminator: " + value)

def parse_dump(text):
    functions = []
    current_function = None
    current_block = None
    in_instrs = False
    for line in text.splitlines():
        match = re.search(r'Function \{ name = "((?:\\.|[^"\\])*)";', line)
        if match:
            current_function = {
                "name": bytes(match.group(1), "utf-8").decode("unicode_escape"),
                "blocks": [],
            }
            functions.append(current_function)
            current_block = None
            continue
        match = re.search(r"Block \{ id = ([0-9]+);", line)
        if match:
            current_block = {"id": int(match.group(1)), "instrs": [], "term": None}
            current_function["blocks"].append(current_block)
            in_instrs = "instrs = [" in line
            continue
        if current_block is None:
            continue
        if "instrs = [" in line:
            in_instrs = True
            continue
        if in_instrs:
            if line.strip() == "];":
                in_instrs = False
            else:
                current_block["instrs"].append(ir_tag(line))
            continue
        if "terminator = " in line:
            current_block["term"] = term_tag(line)
    return functions

def target_names(module):
    functions = {function["name"] for function in module["funcs"]}
    globals_ = {global_["name"] for global_ in module["globals"]}
    for function in module["funcs"]:
        for block in function["blocks"]:
            for instruction in block["instrs"]:
                if instruction[0] == "call":
                    assert instruction[4] in functions or instruction[4] in globals_, (
                        function["name"], instruction[4]
                    )

expected = {
    "value": {"const", "vconst", "null", "undef", "zero", "local", "param", "global"},
    "instr": {"bin", "cmp", "alloca", "load", "load_volatile", "store",
              "store_volatile", "gep", "cast", "call", "phi", "select", "extract",
              "insert", "shuffle_zero", "shufflevector", "string_ptr", "global_ptr", "trap"},
    "terminator": {"ret", "br", "condbr", "switch", "unreachable"},
    "global": {"string", "array", "storage"},
    "ty": {"i1", "i8", "i16", "i32", "i64", "i128", "ptr", "vector", "struct", "array", "void"},
}
observed = {key: set() for key in expected}
value_tags, instruction_tags, terminator_tags = expected["value"], expected["instr"], expected["terminator"]

def visit(value):
    if isinstance(value, list):
        if value and isinstance(value[0], str):
            tag = value[0]
            if tag in value_tags:
                observed["value"].add(tag)
            if tag in instruction_tags:
                observed["instr"].add(tag)
            if tag in terminator_tags:
                observed["terminator"].add(tag)
        for item in value:
            visit(item)
    elif isinstance(value, dict):
        if value.get("kind") in expected["global"]:
            observed["global"].add(value["kind"])
        for item in value.values():
            visit(item)
    elif isinstance(value, str):
        if value in {"i1", "i8", "i16", "i32", "i64", "i128", "ptr", "void"}:
            observed["ty"].add(value)
        elif value.startswith("<") and " x " in value:
            observed["ty"].add("vector")
        elif value.startswith("[") and " x " in value:
            observed["ty"].add("array")
        elif value.startswith("%"):
            observed["ty"].add("struct")

for index in range(count):
    pair = os.path.join(root, "pairs", str(index))
    source_name = open(pair + ".name", encoding="utf-8").read().strip()
    module = read_json(pair + ".json")
    dump = open(pair + ".ir", encoding="utf-8").read()
    parsed = parse_dump(dump)
    dump_names = names_in_dump(dump)
    assert len(module["funcs"]) == len(dump_names), source_name
    assert len(module["funcs"]) == len(parsed), source_name
    for json_function, ir_function in zip(module["funcs"], parsed):
        assert json_function["name"] == ir_function["name"] or json_function["name"].startswith(
            "__fas_c_adapter_"
        ), (source_name, json_function["name"], ir_function["name"])
        assert [block["id"] for block in json_function["blocks"]] == [
            block["id"] for block in ir_function["blocks"]
        ], json_function["name"]
        assert len(json_function["blocks"]) == len(ir_function["blocks"]), json_function["name"]
        for json_block, ir_block in zip(json_function["blocks"], ir_function["blocks"]):
            assert [instruction[0] for instruction in json_block["instrs"]] == ir_block["instrs"], (
                source_name, json_function["name"], json_block["id"],
                [instruction[0] for instruction in json_block["instrs"]], ir_block["instrs"]
            )
            assert json_block["term"][0] == ir_block["term"], (
                source_name, json_function["name"], json_block["id"],
                json_block["term"][0], ir_block["term"]
            )
    target_names(module)
    visit(module)

for key, tags in expected.items():
    missing = tags - observed[key]
    assert not missing, f"missing {key} forms from IR constructors in src/ir.ml: {sorted(missing)}"

fixture = read_json(os.path.join(root, "out.json"))
assert list(fixture) == ["structs", "globals", "funcs", "format", "version",
                         "target_triple", "data_layout", "no_inline"]
assert fixture["format"] == "fas-ir-json" and fixture["version"] == 1
assert fixture["target_triple"] == "x86_64-unknown-linux-gnu"
assert fixture["data_layout"] == "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
assert fixture["no_inline"] is None
assert [list(item) for item in fixture["structs"]] == [["name", "fields", "tail_padding"]]
assert [item["name"] for item in fixture["structs"]] == ["Pair"]
assert fixture["structs"][0]["fields"] == ["i32", "i32"]
global_kinds = {item["kind"] for item in fixture["globals"]}
assert len(fixture["globals"]) == 2
assert global_kinds <= {"array", "storage", "string"}, global_kinds
assert any(
    item["kind"] == "array" and item["elems"] == [7, -2, 9] and item["elem"] == "i32"
    for item in fixture["globals"]
) or any(item["kind"] == "storage" and item["size"] == 12 for item in fixture["globals"])
for array in (item for item in fixture["globals"] if item["kind"] == "array"):
    assert list(array) == ["kind", "name", "elem", "elems", "align"]
for storage in (item for item in fixture["globals"] if item["kind"] == "storage"):
    assert list(storage) == [
        "kind", "name", "ty", "size", "bytes", "pointers", "readonly", "align", "linkage"
    ]
names = [function["name"] for function in fixture["funcs"]]
assert names == names_in_dump(open(os.path.join(root, "out.ir"), encoding="utf-8").read())
for function in fixture["funcs"]:
    assert list(function) == ["name", "params", "ret", "linkage", "variadic", "blocks", "ret_extension"]
    for parameter in function["params"]:
        assert list(parameter) == ["name", "ty", "extension"]
pick = next(function for function in fixture["funcs"] if function["name"] == "pick")
assert pick["ret_extension"] == "none"
assert all(parameter["extension"] == "none" for parameter in pick["params"])
ops = [instruction[0] for block in pick["blocks"] for instruction in block["instrs"]]
kinds = [block["term"][0] for block in pick["blocks"]]
assert "switch" in kinds and "condbr" in kinds and "ret" in kinds, kinds
switch = next(block["term"] for block in pick["blocks"] if block["term"][0] == "switch")
assert switch[1] == "i32" and [case[0] for case in switch[3]] == [1], switch
bins = [instruction[2] for block in pick["blocks"] for instruction in block["instrs"] if instruction[0] == "bin"]
assert "srem" in bins and "add" in bins, bins
cmps = [instruction[2] for block in pick["blocks"] for instruction in block["instrs"] if instruction[0] == "cmp"]
assert "slt" in cmps and "ne" in cmps, cmps
assert "alloca" in ops and "load" in ops and "store" in ops, ops
main = next(function for function in fixture["funcs"] if function["name"] == "main")
calls = [instruction for block in main["blocks"] for instruction in block["instrs"] if instruction[0] == "call"]
assert calls and calls[0][4] == "pick" and [arg[2] for arg in calls[0][5]] == [
    ["const", "i32", 3], ["const", "i32", 5]
], calls

noinline = read_json(os.path.join(root, "noinline.json"))
assert noinline["no_inline"] == "pick"
abi = read_json(os.path.join(root, "pairs", str(source_indexes["abi_exports"]) + ".json"))
abi_functions = {function["name"]: function for function in abi["funcs"]}
for name, extension in (("fas_bool", "zext"), ("fas_i8", "sext"), ("fas_u8", "zext")):
    function = abi_functions[name]
    assert function["params"][0]["extension"] == extension, function
    assert function["ret_extension"] == extension, function

globals_module = read_json(os.path.join(
    root, "pairs", str(source_indexes["globals"]) + ".json"
))
globals_by_name = {global_["name"]: global_ for global_ in globals_module["globals"]}
assert globals_by_name["ZeroScalar"]["linkage"] == "internal"
assert globals_by_name["Exported"]["linkage"] == "export"
assert globals_by_name["Imported"]["linkage"] == "import"

adapter_module = read_json(os.path.join(
    root, "pairs", str(source_indexes["c_interop"]) + ".json"
))
adapter_functions = {
    function["name"]: function for function in adapter_module["funcs"]
    if function["name"].startswith("__fas_c_adapter_")
}
adapter_calls = {
    instruction[4]
    for function in adapter_module["funcs"]
    for block in function["blocks"]
    for instruction in block["instrs"]
    if instruction[0] == "call" and instruction[4].startswith("__fas_c_adapter_")
}
assert adapter_calls, "C adapter call was not serialized by adapter symbol"
assert adapter_calls <= set(adapter_functions), adapter_calls - set(adapter_functions)
for name in adapter_calls:
    assert adapter_functions[name]["linkage"] == "external"
    assert adapter_functions[name]["blocks"] == []

print(f"IR JSON: {count} compilable source files matched --emit-ir; {skipped} were rejected")
PY

echo "IR JSON: format, constructors, determinism and symbol closure passed"
