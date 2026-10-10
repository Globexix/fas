import os
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile
import time

root = pathlib.Path(sys.argv[1]).resolve()
compiler = pathlib.Path(sys.argv[2]).resolve()
started = time.monotonic()
with tempfile.TemporaryDirectory(prefix="fas-reproducible-") as scratch:
    work = pathlib.Path(scratch)
    directories = [work / "first", work / "second"]
    for directory in directories:
        directory.mkdir()
        (directory / "tmp").mkdir()
    sources = sorted(p for p in (root / "test").glob("*.fas") if re.search(r'use\s+"C"', p.read_text()))
    accepted = rejected = artifacts = manifests = 0
    for source in sources:
        statuses = []
        for option, suffix in [("--emit-obj", "o"), ("--emit-llvm", "ll"), ("--emit-header", "h")]:
            results = []
            outputs = []
            bindings = []
            for directory in directories:
                output = directory / ("result." + suffix)
                output.unlink(missing_ok=True)
                env = dict(os.environ, TMPDIR=str(directory / "tmp"))
                command = [str(compiler), "-O2", "--keep", option, str(source), "-o", str(output)]
                result = subprocess.run(command, cwd=directory, env=env, capture_output=True, timeout=300)
                results.append(result)
                outputs.append(output.read_bytes() if output.exists() else None)
                match = re.search(rb"^fas: kept C bindings: (.+)$", result.stderr, re.M)
                bindings.append(pathlib.Path(os.fsdecode(match[1])).read_bytes() if match else None)
            if results[0].returncode != results[1].returncode or outputs[0] != outputs[1] or bindings[0] != bindings[1]:
                raise RuntimeError(f"different {suffix} output for {source}")
            if results[0].returncode == 0:
                if outputs[0] is None:
                    raise RuntimeError(f"missing {suffix} output for {source}")
                artifacts += 1
            if bindings[0] is not None:
                manifests += 1
            if suffix == "o":
                statuses.append(results[0].returncode)
            for directory in directories:
                for path in (directory / "tmp").iterdir():
                    if path.is_dir():
                        shutil.rmtree(path)
                    else:
                        path.unlink()
        if statuses[0] == 0:
            accepted += 1
        else:
            rejected += 1
    if accepted == 0:
        raise RuntimeError("no accepted C importing source was checked")
    print(f"reproducible objects: sources={len(sources)} accepted={accepted} rejected={rejected} artifacts={artifacts} manifests={manifests} seconds={time.monotonic()-started:.3f}")
