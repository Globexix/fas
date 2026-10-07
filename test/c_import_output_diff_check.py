import json
import re
import sys


def llvm_symbol(line, prefix):
    match = re.search(prefix + r'("(?:[^"\\]|\\.)+"|[-A-Za-z$._0-9]+)', line)
    return match.group(1) if match else None


def llvm_referenced(lines, prefix, symbol):
    if symbol.startswith('"'):
        pattern = re.compile(prefix + re.escape(symbol))
    else:
        pattern = re.compile(
            r"(?<![-A-Za-z$._0-9])" + re.escape(prefix + symbol)
            + r"(?![-A-Za-z$._0-9])"
        )
    return any(pattern.search(line) for line in lines)


def check_llvm(old_text, new_text):
    old_lines = old_text.splitlines()
    new_lines = new_text.splitlines()
    new_index = 0
    removed = []
    for line in old_lines:
        if new_index < len(new_lines) and line == new_lines[new_index]:
            new_index += 1
        else:
            removed.append(line)
    if new_index != len(new_lines):
        raise ValueError("new LLVM contains added or reordered lines")
    for line in removed:
        if line.startswith("declare "):
            symbol = llvm_symbol(line, "@")
            if symbol and not symbol.startswith('"llvm.') and not llvm_referenced(
                new_lines, "@", symbol
            ):
                continue
        if re.match(r'^%(?:"(?:[^"\\]|\\.)+"|[-A-Za-z$._0-9]+)\s*=\s*type\b', line):
            symbol = llvm_symbol(line, "%")
            if symbol and not llvm_referenced(new_lines, "%", symbol):
                continue
        if re.match(r'^@(?:"(?:[^"\\]|\\.)+"|[-A-Za-z$._0-9]+)\s*=\s*external\b', line):
            symbol = llvm_symbol(line, "@")
            if symbol and not llvm_referenced(new_lines, "@", symbol):
                continue
        raise ValueError("removed LLVM line is not an unreferenced declaration or type")
    return len(removed)


def ir_references(value, collection, name):
    if isinstance(value, dict):
        if collection == "funcs" and value.get("func") == name:
            return True
        return any(
            key not in {"name", "symbol"}
            and ir_references(item, collection, name)
            for key, item in value.items()
        )
    if isinstance(value, list):
        if collection == "funcs" and len(value) > 4 and value[0] == "call":
            if value[4] == name:
                return True
        if collection == "globals":
            if len(value) > 2 and value[0] == "global" and value[2] == name:
                return True
            if len(value) > 2 and value[0] == "global_ptr" and value[2] == name:
                return True
            if len(value) == 3 and isinstance(value[0], int) and value[1] == name:
                return True
        return any(ir_references(item, collection, name) for item in value)
    return False


def type_references(value, name):
    pattern = re.compile(r"%struct\." + re.escape(name) + r"(?![-A-Za-z$._0-9])")
    if isinstance(value, dict):
        return any(key != "name" and type_references(item, name) for key, item in value.items())
    if isinstance(value, list):
        return any(type_references(item, name) for item in value)
    return isinstance(value, str) and pattern.search(value) is not None


def check_removed_entry(document, collection, entry):
    name = entry.get("name") if isinstance(entry, dict) else None
    if not name:
        raise ValueError("removed imported entry has no name")
    if collection == "funcs":
        if entry.get("blocks") != [] or entry.get("linkage") != "external":
            raise ValueError("removed function is a definition")
        if any(adapter.get("name") == name for adapter in document.get("c_adapters", [])):
            raise ValueError("removed function is still named by a C adapter")
    elif collection == "globals":
        if entry.get("kind") != "storage" or entry.get("linkage") not in {
            "import",
            "import_const",
        }:
            raise ValueError("removed global is not an imported declaration")
    elif collection != "structs":
        raise ValueError("removed entry is not an imported declaration or type")
    if collection in {"funcs", "globals"}:
        if any(ir_references(item, collection, name) for key, item in document.items() if key != collection):
            raise ValueError("removed imported entry is still referenced")
        declarations = document.get(collection, [])
        if any(
            ir_references({key: value for key, value in declaration.items() if key != "name"}, collection, name)
            for declaration in declarations
        ):
            raise ValueError("removed imported entry is still referenced")
    else:
        declarations = [item for item in document.get("structs", []) if item.get("name") != name]
        others = {key: value for key, value in document.items() if key != "structs"}
        if type_references(declarations, name) or type_references(others, name):
            raise ValueError("removed imported type is still referenced")


def compare_json(old, new, document, path=()):
    if isinstance(old, dict) and isinstance(new, dict):
        if old.keys() != new.keys():
            raise ValueError("JSON object keys changed at " + ".".join(path))
        return sum(
            compare_json(old[key], new[key], document, path + (key,))
            for key in old
        )
    if isinstance(old, list) and isinstance(new, list):
        if len(path) == 1 and path[0] in {"funcs", "globals", "structs"}:
            old_index = 0
            removed = []
            for entry in new:
                while old_index < len(old) and old[old_index] != entry:
                    removed.append(old[old_index])
                    old_index += 1
                if old_index == len(old):
                    raise ValueError("JSON array has an added or changed entry at " + path[0])
                old_index += 1
            removed.extend(old[old_index:])
            for entry in removed:
                check_removed_entry(document, path[0], entry)
            return len(removed)
        if len(old) != len(new):
            raise ValueError("JSON array length changed at " + ".".join(path))
        return sum(
            compare_json(left, right, document, path + (str(index),))
            for index, (left, right) in enumerate(zip(old, new))
        )
    if old != new:
        raise ValueError("JSON value changed at " + ".".join(path))
    return 0


def main():
    mode, old_path, new_path = sys.argv[1:]
    old_text = open(old_path, encoding="utf-8").read()
    new_text = open(new_path, encoding="utf-8").read()
    if mode == "llvm":
        count = check_llvm(old_text, new_text)
    elif mode == "json":
        old = json.loads(old_text)
        new = json.loads(new_text)
        count = compare_json(old, new, new)
    else:
        raise ValueError("mode must be llvm or json")
    print(count)


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, json.JSONDecodeError) as error:
        print("c import output difference: " + str(error), file=sys.stderr)
        sys.exit(1)
