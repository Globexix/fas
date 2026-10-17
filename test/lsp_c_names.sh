#!/bin/sh
set -eu
export DEBUGINFOD_URLS=

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
START_DIR=$(pwd)
CC=$(printenv CC || printf 'clang-22')
LLVM_OPT=$(printenv LLVM_OPT || printf 'opt-22')
LLVM_LLC=$(printenv LLVM_LLC || printf 'llc-22')
OCAML_FAS=$(printenv OCAML_FAS || printf '%s/_build/default/bin/main.exe' "$ROOT")

resolve_tool_path() {
  case "$1" in
    /*) printf '%s\n' "$1" ;;
    */*) printf '%s/%s\n' "$START_DIR" "$1" ;;
    *)
      resolved=$(command -v "$1" 2>/dev/null || true)
      case "$resolved" in
        /*) printf '%s\n' "$resolved" ;;
        */*) printf '%s/%s\n' "$START_DIR" "$resolved" ;;
        *) printf '%s\n' "$1" ;;
      esac
      ;;
  esac
}

CC=$(resolve_tool_path "$CC")
LLVM_OPT=$(resolve_tool_path "$LLVM_OPT")
LLVM_LLC=$(resolve_tool_path "$LLVM_LLC")
OCAML_FAS=$(resolve_tool_path "$OCAML_FAS")
REAL_CC=$(command -v "$CC")
REAL_OPT=$(command -v "$LLVM_OPT")
REAL_LLC=$(command -v "$LLVM_LLC")

LSP_TMP=$(mktemp -d)
TMPDIR=$LSP_TMP/tmp
mkdir -p "$LSP_TMP/work" "$TMPDIR"
WORK=$(cd "$LSP_TMP/work" && pwd -P)
export TMPDIR
trap 'rm -rf "$LSP_TMP"' EXIT HUP INT TERM

cat >"$WORK/opt-wrap" <<'SH'
#!/bin/sh
exec "$REAL_OPT" "$@"
SH
cat >"$WORK/llc-wrap" <<'SH'
#!/bin/sh
exec "$REAL_LLC" "$@"
SH
cat >"$WORK/cc-wrap" <<'SH'
#!/bin/sh
printf '%s\n' "$*" >>"$CLANG_LOG"
exec "$REAL_CC" "$@"
SH
chmod +x "$WORK/opt-wrap" "$WORK/llc-wrap" "$WORK/cc-wrap"
: >"$WORK/clang.log"

CLANG_LOG=$WORK/clang.log
FAS_OPT=$WORK/opt-wrap
FAS_LLC=$WORK/llc-wrap
FAS_CC=$WORK/cc-wrap
export CLANG_LOG REAL_OPT REAL_LLC REAL_CC FAS_OPT FAS_LLC FAS_CC

python3 -I - "$OCAML_FAS" "$WORK" <<'PY'
import json
import os
import re
import select
import subprocess
import sys
import urllib.parse

exe = sys.argv[1]
work = sys.argv[2]
real_cc = os.environ["REAL_CC"]
TIMEOUT = 60
servers = []

GOOD_HEADER = """#include <stdint.h>

#define CN_LIMIT 42

struct CnPoint {
  int32_t x;
  int32_t y;
};

typedef struct CnPoint CnPoint;

int cn_add(int a, int b);
"""

GOOD_MAIN = "\n".join(
    [
        'use "C" <string.h>',
        'use "C" <stdlib.h>',
        'use "C" "cn.h"',
        "fn main() i32 {",
        '  count usize = strlen(c"abc")',
        "  raw addr = malloc(8)",
        '  copied addr = memcpy(raw, c"abc", 4)',
        "  point CnPoint = {.x = 1, .y = 2}",
        "  limit i32 = CN_LIMIT",
        "  total i32 = cn_add(point.x, point.y)",
        "  return 0",
        "}",
    ]
) + "\n"

BAD_HEADER = """#include <stdint.h>

int cn_ok(int value);

long double cn_wide(long double value);
"""

BAD_BROKEN = "\n".join(
    [
        'use "C" "bad.h"',
        "fn main() i32 {",
        "  ok i32 = cn_ok(1)",
        "  wide i32 = cn_wide(1)",
        "  return 0",
        "}",
    ]
) + "\n"

BAD_FIXED = BAD_BROKEN.replace("wide i32 = cn_wide(1)", "wide i32 = cn_ok(2)")

BAD_SHIFTED = "\n" + BAD_BROKEN

OTHER = "\n".join(
    [
        "fn main() i32 {",
        "  wide i32 = cn_wide(1)",
        "  return 0",
        "}",
    ]
) + "\n"

def fail(message):
    for server in servers:
        server.proc.kill()
    sys.stderr.write("lsp_c_names: %s\n" % message)
    sys.exit(1)


def expect(condition, message):
    if not condition:
        fail(message)


def join_lines(lines):
    return "\n".join(lines) + "\n"


def raw_frame(body):
    return b"Content-Length: %d\r\n\r\n" % len(body) + body


def frame_of(message):
    return raw_frame(json.dumps(message).encode("utf-8"))


def uri_of(path):
    return "file://" + urllib.parse.quote(path, safe="/")


def write_file(path, text):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8", newline="") as handle:
        handle.write(text)


def read_text(path):
    with open(path, encoding="utf-8", newline="") as handle:
        return handle.read()


def utf16_units(text):
    return len(text.encode("utf-16-le")) // 2


def position_of(text, anchor, offset):
    count = text.count(anchor)
    expect(count == 1, "anchor %r occurs %d times" % (anchor, count))
    prefix = text[: text.index(anchor) + offset]
    return prefix.count("\n"), utf16_units(prefix.rsplit("\n", 1)[-1])


def rng(start_line, start_char, end_line, end_char):
    return {
        "start": {"line": start_line, "character": start_char},
        "end": {"line": end_line, "character": end_char},
    }


def loc(path, line, start, end):
    return {"uri": uri_of(path), "range": rng(line, start, line, end)}


def at(path, source, anchor, offset=0):
    line, character = position_of(source, anchor, offset)
    return {
        "textDocument": {"uri": uri_of(path)},
        "position": {"line": line, "character": character},
    }


def published(publishes, path):
    found = None
    for uri, diagnostics in publishes:
        if uri == uri_of(path):
            found = diagnostics
    expect(found is not None, "no publishDiagnostics for %s" % path)
    return found


class Server:
    def __init__(self, name):
        self.name = name
        self.stderr = open(os.path.join(work, name + ".err"), "wb")
        self.proc = subprocess.Popen(
            [exe, "lsp"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=self.stderr,
        )
        self.buffer = b""
        self.next_id = 100
        self.replies = []
        self.publishes = []
        servers.append(self)

    def write(self, data):
        self.proc.stdin.write(data)
        self.proc.stdin.flush()

    def read_message(self):
        while True:
            end = self.buffer.find(b"\r\n\r\n")
            if end >= 0:
                length = None
                for line in self.buffer[:end].split(b"\r\n"):
                    key, _, value = line.partition(b":")
                    if key.strip().lower() == b"content-length":
                        length = int(value.strip())
                expect(length is not None, "%s: output without Content-Length" % self.name)
                start = end + 4
                if len(self.buffer) >= start + length:
                    body = self.buffer[start : start + length]
                    self.buffer = self.buffer[start + length :]
                    return json.loads(body.decode("utf-8"))
            ready, _, _ = select.select([self.proc.stdout], [], [], TIMEOUT)
            expect(ready, "%s: no output within %d seconds" % (self.name, TIMEOUT))
            chunk = os.read(self.proc.stdout.fileno(), 65536)
            expect(chunk, "%s: server closed stdout" % self.name)
            self.buffer += chunk

    def pump(self):
        message = self.read_message()
        if "method" in message:
            if message["method"] == "textDocument/publishDiagnostics":
                params = message["params"]
                self.publishes.append((params["uri"], params["diagnostics"]))
        else:
            self.replies.append(message)

    def wait_reply(self, ident):
        while True:
            for index, reply in enumerate(self.replies):
                if reply.get("id") == ident:
                    return self.replies.pop(index)
            self.pump()

    def request(self, method, params=None):
        self.next_id += 1
        ident = self.next_id
        message = {"jsonrpc": "2.0", "id": ident, "method": method}
        if params is not None:
            message["params"] = params
        self.write(frame_of(message))
        return self.wait_reply(ident)

    def result(self, method, params=None):
        reply = self.request(method, params)
        expect("result" in reply, "%s: %s failed: %r" % (self.name, method, reply))
        return reply["result"]

    def notify(self, method, params=None):
        message = {"jsonrpc": "2.0", "method": method}
        if params is not None:
            message["params"] = params
        self.write(frame_of(message))

    def settle(self):
        reply = self.request("fas/settle")
        expect(
            reply.get("error", {}).get("code") == -32601,
            "%s: settle got %r" % (self.name, reply),
        )
        expect(not self.replies, "%s: unexpected replies %r" % (self.name, self.replies))
        publishes = self.publishes
        self.publishes = []
        return publishes

    def finish(self, code):
        self.notify("exit")
        status = self.proc.wait(timeout=TIMEOUT)
        expect(status == code, "%s: exit status %d, expected %d" % (self.name, status, code))


def initialize(server):
    reply = server.request(
        "initialize", {"processId": None, "rootUri": None, "capabilities": {}}
    )
    expect("result" in reply, "%s: initialize failed: %r" % (server.name, reply))
    result = reply["result"]
    expect(result["capabilities"]["textDocumentSync"] == 1, "%s: textDocumentSync" % server.name)
    expect(result["serverInfo"]["name"] == "fas", "%s: serverInfo name" % server.name)
    server.notify("initialized", {})
    return result


def shutdown(server):
    reply = server.request("shutdown")
    expect("result" in reply and reply["result"] is None, "%s: shutdown %r" % (server.name, reply))


def did_open(server, path, text, version):
    server.notify(
        "textDocument/didOpen",
        {"textDocument": {"uri": uri_of(path), "languageId": "fas", "version": version, "text": text}},
    )


def did_change(server, path, text, version):
    server.notify(
        "textDocument/didChange",
        {"textDocument": {"uri": uri_of(path), "version": version}, "contentChanges": [{"text": text}]},
    )


def did_close(server, path):
    server.notify("textDocument/didClose", {"textDocument": {"uri": uri_of(path)}})


def valid_range(value):
    expect(isinstance(value, dict) and set(value) == {"start", "end"}, "range shape: %r" % (value,))
    for point in (value["start"], value["end"]):
        expect(isinstance(point, dict) and set(point) == {"line", "character"}, "position shape: %r" % (value,))
        expect(type(point["line"]) is int and type(point["character"]) is int, "position type: %r" % (value,))
    start = (value["start"]["line"], value["start"]["character"])
    end = (value["end"]["line"], value["end"]["character"])
    expect(start <= end, "range order: %r" % (value,))


def hover_text(result):
    if result is None:
        return None
    expect(set(result) == {"contents", "range"}, "hover keys: %r" % (result,))
    valid_range(result["range"])
    expect(set(result["contents"]) == {"kind", "value"}, "hover contents keys: %r" % (result,))
    expect(result["contents"]["kind"] == "markdown", "hover kind: %r" % (result,))
    return result["contents"]["value"]


def hover_at(s, path, source, anchor, offset=0):
    result = s.result("textDocument/hover", at(path, source, anchor, offset))
    return result, hover_text(result)


def expect_value(s, path, source, anchor, offset, expected, label):
    _, value = hover_at(s, path, source, anchor, offset)
    expect(value == expected, "%s: hover at %r+%d: got %r, expected %r" % (label, anchor, offset, value, expected))


def clang_paths(name):
    probe = os.path.join(work, "probe.c")
    write_file(probe, "#include <%s>\n" % name)
    output = subprocess.run(
        [real_cc, "-H", "-fsyntax-only", probe],
        capture_output=True,
        text=True,
        timeout=TIMEOUT,
        check=True,
    ).stderr
    paths = set()
    for line in output.splitlines():
        match = re.match(r"^\.+ (/.+)$", line)
        if match:
            paths.add(match.group(1))
    expect(paths, "clang -H lists no headers for %s" % name)
    return paths


def find_line(path, pattern):
    matches = [
        number
        for number, line in enumerate(read_text(path).split("\n"), 1)
        if re.match(pattern, line)
    ]
    expect(len(matches) == 1, "%s: pattern %r matches lines %r" % (path, pattern, matches))
    return matches[0]


def c_declaration(text, line):
    lines = text.split("\n")
    start = sum(len(item) + 1 for item in lines[: line - 1])
    depth_paren = 0
    depth_brace = 0
    raw = []
    index = start
    while index < len(text):
        if text.startswith("/*", index):
            index = text.index("*/", index + 2) + 2
            continue
        if text.startswith("//", index):
            index = text.index("\n", index)
            continue
        char = text[index]
        if char in "\"'":
            end = index + 1
            while text[end] != char:
                end += 2 if text[end] == "\\" else 1
            raw.append(text[index : end + 1])
            index = end + 1
            continue
        raw.append(char)
        if char == "(":
            depth_paren += 1
        elif char == ")":
            depth_paren -= 1
        elif char == "{":
            depth_brace += 1
        elif char == "}":
            depth_brace -= 1
            if depth_brace == 0:
                break
        elif char == ";" and depth_paren == 0 and depth_brace == 0:
            break
        index += 1
    joined = "".join(raw)
    keep_lines = "{" in joined
    collapsed = re.sub(
        r"\s+",
        lambda match: "\n" if keep_lines and "\n" in match.group(0) else " ",
        joined,
    )
    return collapsed.strip()


def split_location(value):
    path, _, number = value.rpartition("\n")[2].rpartition(":")
    return path, int(number)


def expect_c_hover(s, path, source, anchor, offset, fas, facts, pattern, allowed, label):
    result, value = hover_at(s, path, source, anchor, offset)
    expect(value is not None, "%s: no hover at %r+%d" % (label, anchor, offset))
    header, number = split_location(value)
    expect(header in allowed, "%s: header %r not in %r" % (label, header, sorted(allowed)))
    line = find_line(header, pattern)
    expect(number == line, "%s: header line %d, grep found %d" % (label, number, line))
    declaration = c_declaration(read_text(header), line)
    expected = "\n\n".join(
        ["```fas\n%s\n```" % fas]
        + facts
        + ["```c\n%s\n```" % declaration, "%s:%d" % (header, line)]
    )
    expect(value == expected, "%s: got %r, expected %r" % (label, value, expected))
    return result


UNSUPPORTED_REASON = "floating-point types are not supported"


def check_names_reason(bad):
    output = subprocess.run(
        [exe, "check", bad],
        capture_output=True,
        text=True,
        timeout=TIMEOUT,
    )
    text = output.stdout + output.stderr
    expect(
        "C declaration `cn_wide` is not supported: %s" % UNSUPPORTED_REASON in text,
        "fas check does not report the cn_wide reason: %r" % (text,),
    )


def expect_unsupported(s, path, source, anchor, offset, reason, header_path, pattern, label):
    result, value = hover_at(s, path, source, anchor, offset)
    expect(value is not None, "%s: no hover at %r+%d" % (label, anchor, offset))
    header, number = split_location(value)
    expect(header == header_path, "%s: header %r, expected %r" % (label, header, header_path))
    line = find_line(header_path, pattern)
    expect(number == line, "%s: header line %d, grep found %d" % (label, number, line))
    expected = "C declaration `cn_wide` is not supported: %s\n\n%s:%d" % (reason, header_path, line)
    expect(value == expected, "%s: got %r, expected %r" % (label, value, expected))
    position_line, character = position_of(source, anchor, offset)
    expect(
        result["range"] == rng(position_line, character, position_line, character + len("cn_wide")),
        "%s: range %r" % (label, result["range"]),
    )


def pins_good(s, main, cn_h):
    system_string = clang_paths("string.h")
    system_stdlib = clang_paths("stdlib.h")
    local = {cn_h}
    expect_c_hover(
        s, main, GOOD_MAIN, "strlen(c", 1,
        "extern fn strlen(arg0 addr) usize",
        ["nonnull: arg0; C string: arg0"],
        r"^extern .*[ *]strlen \(", system_string, "strlen",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "malloc(8)", 0,
        "extern fn malloc(arg0 usize) addr",
        ["alloc_size: arg0"],
        r"^extern .*[ *]malloc \(", system_stdlib, "malloc",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "memcpy(raw", 0,
        "extern fn memcpy(arg0 addr, arg1 addr, arg2 usize) addr",
        ["nonnull: arg0, arg1"],
        r"^extern .*[ *]memcpy \(", system_string, "memcpy",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "CnPoint =", 0,
        "struct CnPoint @align(4)",
        ["x i32 at offset 0", "y i32 at offset 4"],
        r"^struct CnPoint \{", local, "struct CnPoint",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "point.x", 6,
        "x i32 at offset 0",
        [],
        r"^\s+int32_t x;", local, "field x",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "cn_add(point", 0,
        "extern fn cn_add(arg0 i32, arg1 i32) i32",
        [],
        r"^int cn_add\(", local, "cn_add",
    )
    expect_value(
        s, main, GOOD_MAIN, "CN_LIMIT", 0,
        "```fas\nconst CN_LIMIT i32 = 42\n```",
        "macro CN_LIMIT",
    )


def run_c_names():
    s = Server("c_names")
    result = initialize(s)
    expect(result["capabilities"].get("hoverProvider") is True, "capability hoverProvider is not true")
    expect(result["capabilities"].get("definitionProvider") is True, "capability definitionProvider is not true")

    folder = os.path.join(work, "good")
    main = os.path.join(folder, "main.fas")
    cn_h = os.path.realpath(os.path.join(folder, "cn.h"))
    write_file(cn_h, GOOD_HEADER)
    write_file(main, GOOD_MAIN)
    did_open(s, main, GOOD_MAIN, 1)
    expect(published(s.settle(), main) == [], "open good main has diagnostics")

    pins_good(s, main, cn_h)

    expect(
        s.result("textDocument/definition", at(main, GOOD_MAIN, "CnPoint =", 0)) == loc(cn_h, 4, 0, 0),
        "definition of CnPoint",
    )
    expect(
        s.result("textDocument/definition", at(main, GOOD_MAIN, "point.x", 6)) == loc(cn_h, 5, 0, 0),
        "definition of field x",
    )

    bad_folder = os.path.join(work, "bad")
    bad_h = os.path.realpath(os.path.join(bad_folder, "bad.h"))
    bad = os.path.join(bad_folder, "bad.fas")
    other = os.path.join(bad_folder, "other.fas")
    write_file(bad_h, BAD_HEADER)
    write_file(bad, BAD_BROKEN)
    check_names_reason(bad)
    write_file(other, OTHER)

    did_open(s, other, OTHER, 1)
    expect(published(s.settle(), other) != [], "other.fas should fail check")
    expect_value(s, other, OTHER, "cn_wide(1", 0, None, "outside the closure")

    did_open(s, bad, BAD_BROKEN, 1)
    expect(published(s.settle(), bad) != [], "broken bad.fas has no diagnostics")
    expect_unsupported(
        s, bad, BAD_BROKEN, "cn_wide(1", 0, UNSUPPORTED_REASON, bad_h,
        r"^long double cn_wide\(", "unsupported cn_wide",
    )

    did_change(s, bad, BAD_FIXED, 2)
    expect(published(s.settle(), bad) == [], "fixed bad.fas has diagnostics")
    expect_c_hover(
        s, bad, BAD_FIXED, "cn_ok(1", 0,
        "extern fn cn_ok(arg0 i32) i32",
        [],
        r"^int cn_ok\(", {bad_h}, "cn_ok",
    )

    did_change(s, bad, BAD_BROKEN, 3)
    expect(published(s.settle(), bad) != [], "restored cn_wide line has no diagnostics")
    expect_unsupported(
        s, bad, BAD_BROKEN, "cn_wide(1", 0, UNSUPPORTED_REASON, bad_h,
        r"^long double cn_wide\(", "unsupported cn_wide in changed region",
    )
    expect_c_hover(
        s, bad, BAD_BROKEN, "cn_ok(1", 0,
        "extern fn cn_ok(arg0 i32) i32",
        [],
        r"^int cn_ok\(", {bad_h}, "cn_ok from snapshot",
    )

    did_change(s, bad, BAD_SHIFTED, 4)
    expect(published(s.settle(), bad) != [], "shifted bad.fas has no diagnostics")
    expect_unsupported(
        s, bad, BAD_SHIFTED, "cn_wide(1", 0, UNSUPPORTED_REASON, bad_h,
        r"^long double cn_wide\(", "unsupported cn_wide after line insert",
    )
    expect_value(s, other, OTHER, "cn_wide(1", 0, None, "outside the closure after bad root")

    pins_good(s, main, cn_h)

    did_close(s, other)
    did_close(s, bad)
    did_close(s, main)
    s.settle()
    shutdown(s)
    s.finish(0)


run_c_names()
PY

echo "lsp_c_names: imported C name hovers, unsupported names, and definitions: ok"
