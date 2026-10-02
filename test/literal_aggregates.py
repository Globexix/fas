import os
import pathlib
import subprocess
import tempfile
import time

compiler = os.environ['OCAML_FAS']
opt = os.environ.get('LLVM_OPT', 'opt-22')
with tempfile.TemporaryDirectory() as directory:
    root = pathlib.Path(directory)
    for size in (16384, 65536):
        source = root / 'large.fas'
        entries = ','.join(f'{index} + 1' for index in range(size))
        source.write_text(f'fn main() i32 {{ a arr[{size},u32] = {{{entries}}}\n'
                          'i usize = 0\n while i < sizeof[a] / 4 { '
                          'if a[i] != trunc[u32](i + 1) { return 1 }\n i += 1 }\n return 0 }'.replace('sizeof[a]', f'{size * 4}'))
        ir = root / 'large.ll'
        ir.write_bytes(subprocess.check_output([compiler, '--emit-llvm', str(source)], timeout=5))
        subprocess.run([opt, '-passes=verify', str(ir), '-disable-output'], check=True)
        for level in (0, 2):
            optimized = root / 'optimized.ll'
            subprocess.run([opt, '-S', f'-passes=default<O{level}>', str(ir), '-o', str(optimized)], check=True, timeout=5)
            subprocess.run([opt, '-passes=verify', str(optimized), '-disable-output'], check=True)
            start = time.monotonic()
            subprocess.run([compiler, f'-O{level}', str(source), '-o', str(root / 'large')], check=True, timeout=5)
            elapsed = time.monotonic() - start
            print(f'literal_aggregates: {size} entries O{level}: {elapsed:.3f}s', flush=True)
            assert elapsed < 1, elapsed
            subprocess.run([str(root / 'large')], check=True)
        assert b'@.literal.' in ir.read_bytes()
