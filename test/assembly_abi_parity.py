import sys
from pathlib import Path

from abi_matrix import function_signature


fas_text = Path(sys.argv[1]).read_text()
clang_text = Path(sys.argv[2]).read_text()
names = [
    "asm_echo_bool", "asm_echo_u8", "asm_echo_i8", "asm_echo_u16", "asm_echo_i16",
    "asm_echo_u32", "asm_echo_i32", "asm_echo_u64", "asm_echo_i64", "asm_echo_usize",
    "asm_echo_isize", "asm_echo_addr", "asm_echo_handle", "asm_noop", "asm_seven",
    "asm_write", "fas_verify_assembly",
]

for name in names:
    fas_sig = function_signature(fas_text, name)
    clang_sig = function_signature(clang_text, name)
    fas_ret = (fas_sig[0], tuple(attr for attr in fas_sig[1] if attr != "noundef"))
    clang_ret = (clang_sig[0], tuple(attr for attr in clang_sig[1] if attr != "noundef"))
    fas_args = tuple((ty, tuple(attr for attr in attrs if attr != "noundef")) for ty, attrs in fas_sig[2])
    clang_args = tuple((ty, tuple(attr for attr in attrs if attr != "noundef")) for ty, attrs in clang_sig[2])
    if fas_ret != clang_ret or fas_args != clang_args or fas_sig[3] != clang_sig[3]:
        raise SystemExit(f"asm ABI parity mismatch for {name}: Fas={fas_sig} Clang={clang_sig}")

print(f"Assembly ABI parity: {len(names)} signatures match Clang")
