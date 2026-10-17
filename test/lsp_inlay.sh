#!/bin/sh
set -eu
export DEBUGINFOD_URLS=

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
START_DIR=$(pwd)
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

OCAML_FAS=$(resolve_tool_path "$OCAML_FAS")

LSP_TMP=$(mktemp -d)
TMPDIR=$LSP_TMP/tmp
mkdir -p "$LSP_TMP/work" "$TMPDIR"
WORK=$(cd "$LSP_TMP/work" && pwd -P)
export TMPDIR
trap 'rm -rf "$LSP_TMP"' EXIT HUP INT TERM

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
TIMEOUT = 60
servers = []

MAIN = [
    'use "lib.fas"',
    "",
    "fn identity[T](value T) T {",
    "    return value",
    "}",
    "",
    "fn fill[T, N const usize](values vec[N,T]) T {",
    "    return reduce_sum(values)",
    "}",
    "",
    "fn relay[T](value T) T {",
    "    held T = identity(value)",
    "    return held",
    "}",
    "",
    "fn main() i32 {",
    "    seven u8 = 7",
    "    tiny u8 = identity(seven)",
    "    values vec[4,u8] = {1, 2, 3, 4}",
    "    total u8 = fill(values)",
    "    seen u8 = identity[u8](7)",
    "    four u32 = 4",
    "    wide u32 = relay(four)",
    "    narrow u8 = relay[u8](7)",
    "    if wide != 4 { return 1 }",
    "    return 0",
    "}",
]

LIB = [
    "fn wrap[T](value T) T {",
    "    return value",
    "}",
    "",
    "fn lib_seed() u8 {",
    "    seed u8 = 3",
    "    kept u8 = wrap(seed)",
    "    return kept",
    "}",
]

FULL = ((0, 0), (1000, 0))

MAIN_HINTS = [
    (11, 21, "[u32] | [u8]"),
    (17, 22, "[u8]"),
    (19, 19, "[u8, 4]"),
    (22, 20, "[u32]"),
]

def fail(message):
    for server in servers:
        server.proc.kill()
    sys.stderr.write("lsp_inlay: %s\n" % message)
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


def rng(start_line, start_char, end_line, end_char):
    return {
        "start": {"line": start_line, "character": start_char},
        "end": {"line": end_line, "character": end_char},
    }


def hint(line, character, label):
    return {
        "position": {"line": line, "character": character},
        "label": label,
        "kind": 1,
        "paddingLeft": False,
    }


def hints_of(lines):
    return [hint(line, character, label) for line, character, label in lines]


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

    def expect_invalid(self, method, params, label):
        reply = self.request(method, params)
        expect(
            reply.get("error", {}).get("code") == -32602,
            "%s: got %r" % (label, reply),
        )
        message = reply["error"].get("message")
        expect(isinstance(message, str) and message != "", "%s: no message in %r" % (label, reply))

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
    expect(
        re.match(r"^\d+\.\d+\.\d+", result["serverInfo"]["version"]),
        "%s: serverInfo version" % server.name,
    )
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


def expect_published(publishes, path, expected, label):
    found = published(publishes, path)
    expect(found == expected, "%s: got %r, expected %r" % (label, found, expected))


def expect_hints(s, path, first, last, expected, label):
    got = s.result(
        "textDocument/inlayHint",
        {
            "textDocument": {"uri": uri_of(path)},
            "range": rng(first[0], first[1], last[0], last[1]),
        },
    )
    expect(isinstance(got, list), "%s: inlay hints are not a list: %r" % (label, got))
    for item in got:
        expect(set(item) == {"position", "label", "kind", "paddingLeft"}, "%s: hint keys %r" % (label, item))
        position = item["position"]
        expect(
            isinstance(position, dict) and set(position) == {"line", "character"},
            "%s: position shape %r" % (label, item),
        )
        expect(
            type(position["line"]) is int and type(position["character"]) is int,
            "%s: position type %r" % (label, item),
        )
        expect(type(item["label"]) is str, "%s: label type %r" % (label, item))
        expect(type(item["kind"]) is int, "%s: kind type %r" % (label, item))
        expect(type(item["paddingLeft"]) is bool, "%s: paddingLeft type %r" % (label, item))
    expect(got == expected, "%s: got %r, expected %r" % (label, got, expected))


def expect_malformed(s, path, label):
    uri = {"uri": uri_of(path)}
    s.expect_invalid("textDocument/inlayHint", {"textDocument": uri}, label + ": missing range")
    s.expect_invalid(
        "textDocument/inlayHint",
        {"textDocument": uri, "range": rng(17, 0, 12, 0)},
        label + ": inverted range",
    )
    s.expect_invalid(
        "textDocument/inlayHint",
        {"textDocument": uri, "range": rng(-1, 0, 2, 0)},
        label + ": negative line",
    )


def run_inlay():
    s = Server("inlay")
    result = initialize(s)
    expect(result["capabilities"].get("inlayHintProvider") is True, "capability inlayHintProvider is not true")
    folder = os.path.join(work, "inlay")
    main = os.path.join(folder, "main.fas")
    lib = os.path.join(folder, "lib.fas")
    stray = os.path.join(folder, "stray.fas")
    main_text = join_lines(MAIN)
    lib_text = join_lines(LIB)
    write_file(main, main_text)
    write_file(lib, lib_text)
    write_file(stray, "fn stray_only() i32 {\n    return 0\n}\n")
    did_open(s, main, main_text, 1)
    did_open(s, lib, lib_text, 1)
    publishes = s.settle()
    expect_published(publishes, main, [], "open main")
    expect_published(publishes, lib, [], "open lib")

    expect_hints(s, main, FULL[0], FULL[1], hints_of(MAIN_HINTS), "open main")
    expect_hints(s, main, (12, 0), (21, 99), hints_of([(17, 22, "[u8]"), (19, 19, "[u8, 4]")]), "range excludes outer calls")
    expect_hints(s, main, (17, 0), (17, 22), hints_of([(17, 22, "[u8]")]), "range end is inclusive")
    expect_hints(s, main, (17, 0), (17, 21), [], "range ends before hint")
    expect_hints(s, lib, FULL[0], FULL[1], hints_of([(6, 18, "[u8]")]), "use file")
    expect_hints(s, stray, FULL[0], FULL[1], [], "file without snapshot")
    expect_malformed(s, main, "open main")

    broken_text = join_lines(["broken one ("] + MAIN)
    did_change(s, main, broken_text, 2)
    expect(published(s.settle(), main) != [], "inserted broken line has no diagnostics")
    expect_hints(
        s,
        main,
        FULL[0],
        FULL[1],
        hints_of([(12, 21, "[u32] | [u8]"), (18, 22, "[u8]"), (20, 19, "[u8, 4]"), (23, 20, "[u32]")]),
        "inserted broken line",
    )

    row = next(index for index, line in enumerate(MAIN) if line.startswith("    tiny u8"))
    edited = list(MAIN)
    edited[row] = "    tiny u8 = identity(seven"
    edited_text = join_lines(edited)
    did_change(s, main, edited_text, 3)
    expect(published(s.settle(), main) != [], "edited hint line has no diagnostics")
    expect_hints(
        s,
        main,
        FULL[0],
        FULL[1],
        hints_of([(11, 21, "[u32] | [u8]"), (19, 19, "[u8, 4]"), (22, 20, "[u32]")]),
        "edited hint line",
    )

    did_change(s, main, main_text, 4)
    expect_published(s.settle(), main, [], "fixed main")
    expect_hints(s, main, FULL[0], FULL[1], hints_of(MAIN_HINTS), "fixed main")

    did_close(s, main)
    did_close(s, lib)
    s.settle()
    shutdown(s)
    s.finish(0)


run_inlay()
PY

echo "lsp_inlay: inferred inlay hints, ranges, use files, and broken-edit mapping: ok"
