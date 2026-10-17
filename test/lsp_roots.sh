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
import select
import subprocess
import sys
import time
import urllib.parse

exe = sys.argv[1]
work = sys.argv[2]
TIMEOUT = 60
REGISTRATION = "fas-roots-watch"
servers = []

GAME = [
    'use "engine/core.fas"',
    'use "engine/sdl.fas"',
    "",
    "fn game_tick() i32 {",
    "    return 3",
    "}",
    "",
    "fn main() i32 {",
    "    return game_init() + sdl_ready()",
    "}",
]
CORE = [
    "fn core_name() i32 {",
    "    return 2",
    "}",
    "",
    "fn game_init() i32 {",
    "    return core_name()",
    "}",
]
SDL = [
    'use "core.fas"',
    "",
    "fn sdl_ready() i32 {",
    "    return game_init() + game_tick() + core_name()",
    "}",
]
ZZ_MAIN = [
    'use "lib/shared.fas"',
    "",
    "fn helper_main() i32 {",
    "    return 4",
    "}",
    "",
    "fn main() i32 {",
    "    return shared_value()",
    "}",
]
A_ROOT = [
    'use "lib/shared.fas"',
    "",
    "fn other() i32 {",
    "    return shared_value()",
    "}",
]
SHARED = [
    "fn shared_value() i32 {",
    "    return helper_main()",
    "}",
]
S_ROOT = [
    'use "lib/leaf.fas"',
    "",
    "fn only_in_s() i32 {",
    "    return 1",
    "}",
]
LONGER_ROOT = [
    'use "lib/leaf.fas"',
    "",
    "fn unrelated() i32 {",
    "    return 2",
    "}",
]
LEAF = [
    "fn leaf() i32 {",
    "    return only_in_s()",
    "}",
]
TIE_A = [
    'use "lib/tie_leaf.fas"',
    "",
    "fn only_a() i32 {",
    "    return 1",
    "}",
]
TIE_B = [
    'use "lib/tie_leaf.fas"',
    "",
    "fn only_b() i32 {",
    "    return 2",
    "}",
]
TIE_LEAF = [
    "fn tie_leaf() i32 {",
    "    return only_a()",
    "}",
]
LONELY = [
    "fn lonely() i32 {",
    "    return missing_name",
    "}",
]
TARGET_LIB = [
    "fn target_lib() i32 {",
    "    return hidden_only()",
    "}",
]
SKIPPED_MAIN = [
    'use "../target_lib.fas"',
    "",
    "fn hidden_only() i32 {",
    "    return 5",
    "}",
    "",
    "fn main() i32 {",
    "    return target_lib()",
    "}",
]
CAP_LEAF = [
    "fn leaf() i32 {",
    "    return only_root()",
    "}",
]
CAP_ROOT = [
    'use "a_leaf.fas"',
    "",
    "fn only_root() i32 {",
    "    return 1",
    "}",
    "",
    "fn main() i32 {",
    "    return leaf()",
    "}",
]
CHANGE_PART = [
    "fn part_value() i32 {",
    "    return root_fn()",
    "}",
]
CHANGE_APP_BEFORE = [
    "fn root_fn() i32 {",
    "    return 1",
    "}",
    "",
    "fn main() i32 {",
    "    return part_value()",
    "}",
]
CHANGE_APP_AFTER = ['use "lib/part.fas"', ""] + CHANGE_APP_BEFORE
WATCH_LIB = [
    "fn lib_fn() i32 {",
    "    return root_only()",
    "}",
]
WATCH_APP = [
    'use "lib.fas"',
    "",
    "fn root_only() i32 {",
    "    return 3",
    "}",
    "",
    "fn main() i32 {",
    "    return lib_fn()",
    "}",
]


def fail(message):
    for server in servers:
        server.proc.kill()
    sys.stderr.write("lsp_roots: %s\n" % message)
    sys.exit(1)


def expect(condition, message):
    if not condition:
        fail(message)


def pin(label, got, expected):
    expect(got == expected, "%s: got %r, expected %r" % (label, got, expected))


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


def position_of(text, anchor, offset):
    count = text.count(anchor)
    expect(count == 1, "anchor %r occurs %d times" % (anchor, count))
    prefix = text[: text.index(anchor) + offset]
    return prefix.count("\n"), len(prefix.rsplit("\n", 1)[-1])


def at(path, text, anchor, offset=0):
    line, character = position_of(text, anchor, offset)
    return {
        "textDocument": {"uri": uri_of(path)},
        "position": {"line": line, "character": character},
    }


def error_code(reply):
    return reply.get("error", {}).get("code")


def hover_of(signature):
    return "```fas\n%s\n```" % signature


def error_at(line, start, end, message):
    return {
        "range": {
            "start": {"line": line, "character": start},
            "end": {"line": line, "character": end},
        },
        "severity": 1,
        "source": "fas",
        "message": message,
    }


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
        self.latest = {}
        self.requests = []
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
                self.latest[params["uri"]] = params["diagnostics"]
            else:
                self.requests.append(message)
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
        expect(error_code(reply) == -32601, "%s: settle got %r" % (self.name, reply))
        expect(not self.replies, "%s: unexpected replies %r" % (self.name, self.replies))

    def diagnostics(self, path):
        uri = uri_of(path)
        expect(uri in self.latest, "%s: no diagnostics published for %s" % (self.name, path))
        return self.latest[uri]

    def hover(self, path, text, anchor, offset=0):
        result = self.result("textDocument/hover", at(path, text, anchor, offset))
        if result is None:
            return None
        expect(set(result) == {"contents", "range"}, "hover keys: %r" % (result,))
        expect(
            set(result["contents"]) == {"kind", "value"} and result["contents"]["kind"] == "markdown",
            "hover contents: %r" % (result["contents"],),
        )
        return result["contents"]["value"]

    def finish(self, code):
        reply = self.request("shutdown")
        expect("result" in reply and reply["result"] is None, "%s: shutdown %r" % (self.name, reply))
        self.notify("exit")
        status = self.proc.wait(timeout=TIMEOUT)
        expect(status == code, "%s: exit status %d, expected %d" % (self.name, status, code))


def start(name, folder, watch=False):
    server = Server(name)
    capabilities = {}
    if watch:
        capabilities = {"workspace": {"didChangeWatchedFiles": {"dynamicRegistration": True}}}
    reply = server.request(
        "initialize",
        {"processId": None, "rootUri": uri_of(folder), "capabilities": capabilities},
    )
    expect("result" in reply, "%s: initialize failed: %r" % (name, reply))
    expect(reply["result"]["capabilities"]["textDocumentSync"] == 1, "%s: textDocumentSync" % name)
    server.notify("initialized", {})
    return server


def did_open(server, path, text, version=1):
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


def watched(server, path, kind):
    server.notify(
        "workspace/didChangeWatchedFiles",
        {"changes": [{"uri": uri_of(path), "type": kind}]},
    )


def fasge_session(label, opens):
    folder = os.path.join(work, "fasge-" + label)
    files = {
        "game": (os.path.join(folder, "game.fas"), join_lines(GAME)),
        "core": (os.path.join(folder, "engine", "core.fas"), join_lines(CORE)),
        "sdl": (os.path.join(folder, "engine", "sdl.fas"), join_lines(SDL)),
    }
    for path, text in files.values():
        write_file(path, text)
    s = start("fasge-" + label, folder)
    for key in opens:
        path, text = files[key]
        did_open(s, path, text)
        s.settle()
    expect(s.requests == [], "fasge %s: unexpected server requests %r" % (label, s.requests))
    for key in opens:
        pin("fasge %s diagnostics of %s" % (label, key), s.diagnostics(files[key][0]), [])
    sdl_path, sdl_text = files["sdl"]
    values = {
        "core_name in sdl": s.hover(sdl_path, sdl_text, "core_name()", 0),
        "game_init in sdl": s.hover(sdl_path, sdl_text, "game_init()", 0),
        "game_tick in sdl": s.hover(sdl_path, sdl_text, "game_tick()", 0),
    }
    if "core" in opens:
        core_path, core_text = files["core"]
        values["game_init in core"] = s.hover(core_path, core_text, "fn game_init", 3)
        values["core_name in core"] = s.hover(core_path, core_text, "fn core_name", 3)
    s.finish(0)
    return values


def run_roots():
    sdl_only = fasge_session("sdl-only", ["sdl"])
    pin(
        "fasge sdl-only hovers",
        sdl_only,
        {
            "core_name in sdl": hover_of("fn core_name() i32"),
            "game_init in sdl": hover_of("fn game_init() i32"),
            "game_tick in sdl": hover_of("fn game_tick() i32"),
        },
    )
    sdl_first = fasge_session("sdl-first", ["sdl", "core"])
    core_first = fasge_session("core-first", ["core", "sdl"])
    pin("fasge open order", sdl_first, core_first)
    pin(
        "fasge both open hovers",
        sdl_first,
        {
            "core_name in sdl": hover_of("fn core_name() i32"),
            "game_init in sdl": hover_of("fn game_init() i32"),
            "game_tick in sdl": hover_of("fn game_tick() i32"),
            "game_init in core": hover_of("fn game_init() i32"),
            "core_name in core": hover_of("fn core_name() i32"),
        },
    )

    shared = os.path.join(work, "shared")
    zz_main = os.path.join(shared, "zz_main.fas")
    a_root = os.path.join(shared, "a.fas")
    lib_shared = os.path.join(shared, "lib", "shared.fas")
    write_file(zz_main, join_lines(ZZ_MAIN))
    write_file(a_root, join_lines(A_ROOT))
    write_file(lib_shared, join_lines(SHARED))
    s = start("shared", shared)
    did_open(s, lib_shared, join_lines(SHARED))
    s.settle()
    pin("shared diagnostics with fn main root", s.diagnostics(lib_shared), [])
    pin(
        "shared hover with fn main root",
        s.hover(lib_shared, join_lines(SHARED), "fn shared_value", 3),
        hover_of("fn shared_value() i32"),
    )
    did_open(s, a_root, join_lines(A_ROOT))
    s.settle()
    pin("shared diagnostics after opening the shorter root", s.diagnostics(lib_shared), [])
    s.finish(0)

    pair = os.path.join(work, "pair")
    s_root = os.path.join(pair, "s.fas")
    longer = os.path.join(pair, "longer_root.fas")
    leaf = os.path.join(pair, "lib", "leaf.fas")
    write_file(s_root, join_lines(S_ROOT))
    write_file(longer, join_lines(LONGER_ROOT))
    write_file(leaf, join_lines(LEAF))
    s = start("pair", pair)
    did_open(s, leaf, join_lines(LEAF))
    s.settle()
    pin("pair leaf diagnostics", s.diagnostics(leaf), [])
    pin(
        "pair hover through shortest path root",
        s.hover(leaf, join_lines(LEAF), "only_in_s()", 0),
        hover_of("fn only_in_s() i32"),
    )
    s.finish(0)

    tie = os.path.join(work, "tie")
    tie_a = os.path.join(tie, "a.fas")
    tie_b = os.path.join(tie, "b.fas")
    tie_leaf = os.path.join(tie, "lib", "tie_leaf.fas")
    write_file(tie_a, join_lines(TIE_A))
    write_file(tie_b, join_lines(TIE_B))
    write_file(tie_leaf, join_lines(TIE_LEAF))
    s = start("tie", tie)
    did_open(s, tie_leaf, join_lines(TIE_LEAF))
    s.settle()
    pin("tie leaf diagnostics", s.diagnostics(tie_leaf), [])
    pin(
        "tie hover through alphabetical root",
        s.hover(tie_leaf, join_lines(TIE_LEAF), "only_a()", 0),
        hover_of("fn only_a() i32"),
    )
    s.finish(0)

    lonely_dir = os.path.join(work, "lonely")
    lonely = os.path.join(lonely_dir, "lonely.fas")
    write_file(lonely, join_lines(LONELY))
    s = start("lonely", lonely_dir)
    did_open(s, lonely, join_lines(LONELY))
    s.settle()
    pin(
        "lonely own root diagnostics",
        s.diagnostics(lonely),
        [error_at(1, 11, 23, "unknown name `missing_name`")],
    )
    did_close(s, lonely)
    s.settle()
    pin("lonely cleared after close", s.diagnostics(lonely), [])
    s.finish(0)

    skip = os.path.join(work, "skip")
    target = os.path.join(skip, "target_lib.fas")
    for directory in ("build", ".hidden", "target", "node_modules", ".git"):
        write_file(os.path.join(skip, directory, "main.fas"), join_lines(SKIPPED_MAIN))
    write_file(target, join_lines(TARGET_LIB))
    s = start("skip", skip)
    did_open(s, target, join_lines(TARGET_LIB))
    s.settle()
    pin(
        "skipped directories diagnostics",
        s.diagnostics(target),
        [error_at(1, 11, 24, "unknown function `hidden_only`")],
    )
    s.finish(0)

    cap = os.path.join(work, "cap")
    cap_leaf = os.path.join(cap, "a_leaf.fas")
    cap_root = os.path.join(cap, "zz_root.fas")
    write_file(cap_leaf, join_lines(CAP_LEAF))
    write_file(cap_root, join_lines(CAP_ROOT))
    for number in range(1999):
        write_file(
            os.path.join(cap, "c%04d.fas" % number),
            join_lines(["fn c%d() i32 {" % number, "    return %d" % number, "}"]),
        )
    started = time.time()
    s = start("cap", cap)
    did_open(s, cap_leaf, join_lines(CAP_LEAF))
    s.settle()
    pin(
        "cap leaf past the limit diagnostics",
        s.diagnostics(cap_leaf),
        [error_at(1, 11, 22, "unknown function `only_root`")],
    )
    tiny = os.path.join(cap, "c0500.fas")
    tiny_text = join_lines(["fn c500() i32 {", "    return 500", "}"])
    did_open(s, tiny, tiny_text)
    s.settle()
    pin("cap tiny file hover", s.hover(tiny, tiny_text, "fn c500", 3), hover_of("fn c500() i32"))
    removed = os.path.join(cap, "c1998.fas")
    os.remove(removed)
    watched(s, removed, 3)
    s.settle()
    pin("cap leaf within the limit diagnostics", s.diagnostics(cap_leaf), [])
    elapsed = time.time() - started
    expect(elapsed < 30, "cap scenario took %.1f seconds" % elapsed)
    s.finish(0)

    change = os.path.join(work, "change")
    part = os.path.join(change, "lib", "part.fas")
    app = os.path.join(change, "zz_app.fas")
    write_file(part, join_lines(CHANGE_PART))
    write_file(app, join_lines(CHANGE_APP_BEFORE))
    s = start("change", change)
    did_open(s, part, join_lines(CHANGE_PART))
    s.settle()
    pin(
        "change part before the use line",
        s.diagnostics(part),
        [error_at(1, 11, 20, "unknown function `root_fn`")],
    )
    did_open(s, app, join_lines(CHANGE_APP_BEFORE))
    s.settle()
    pin(
        "change app before the use line",
        s.diagnostics(app),
        [error_at(5, 11, 23, "unknown function `part_value`")],
    )
    did_change(s, app, join_lines(CHANGE_APP_AFTER), 2)
    s.settle()
    pin("change app after the use line", s.diagnostics(app), [])
    pin("change part after the use line", s.diagnostics(part), [])
    pin(
        "change part hover after the use line",
        s.hover(part, join_lines(CHANGE_PART), "fn part_value", 3),
        hover_of("fn part_value() i32"),
    )
    s.finish(0)

    watch = os.path.join(work, "watch")
    lib = os.path.join(watch, "lib.fas")
    app = os.path.join(watch, "zz_app.fas")
    write_file(lib, join_lines(WATCH_LIB))
    s = start("watch", watch, watch=True)
    did_open(s, lib, join_lines(WATCH_LIB))
    s.settle()
    pin(
        "watch registration request",
        s.requests,
        [
            {
                "jsonrpc": "2.0",
                "id": REGISTRATION,
                "method": "client/registerCapability",
                "params": {
                    "registrations": [
                        {
                            "id": REGISTRATION,
                            "method": "workspace/didChangeWatchedFiles",
                            "registerOptions": {"watchers": [{"globPattern": "**/*.fas"}]},
                        }
                    ]
                },
            }
        ],
    )
    s.write(frame_of({"jsonrpc": "2.0", "id": REGISTRATION, "result": None}))
    s.requests = []
    s.settle()
    pin(
        "watch own root diagnostics",
        s.diagnostics(lib),
        [error_at(1, 11, 22, "unknown function `root_only`")],
    )
    write_file(app, join_lines(WATCH_APP))
    watched(s, app, 1)
    s.settle()
    pin("watch open file switches root", s.diagnostics(lib), [])
    pin(
        "watch hover after switch",
        s.hover(lib, join_lines(WATCH_LIB), "fn lib_fn", 3),
        hover_of("fn lib_fn() i32"),
    )
    s.finish(0)


run_roots()
PY

echo "lsp_roots: roots, skipped directories, the file cap, and watched files: ok"
