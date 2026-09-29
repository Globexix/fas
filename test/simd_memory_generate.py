import pathlib
import sys


out = pathlib.Path(sys.argv[1])
types = [
    ("u8", 1, False, False),
    ("i16", 2, True, False),
    ("u32", 4, False, False),
    ("u64", 8, False, False),
    ("bool", 1, False, True),
]
shapes = [1, 3, 4, 8, 16]
forms = [
    ("ml", "masked_load", 0),
    ("ms", "masked_store", 1),
    ("ga", "gather", 2),
    ("sc", "scatter", 3),
    ("gb", "gather_bytes", 4),
    ("sb", "scatter_bytes", 5),
]


def value(raw, width, signed, boolean):
    raw &= (1 << (width * 8)) - 1
    if boolean:
        return "true" if raw else "false"
    if signed and raw >= (1 << (width * 8 - 1)):
        raw -= 1 << (width * 8)
    return str(raw)


def vector(ty, values):
    return "{" + ", ".join(values) + "}"


def hash_source(name, lanes, ty, boolean):
    vals = []
    for lane in range(lanes):
        item = f"r[{lane}]"
        vals.append(f"zext[u64]({item})" if boolean or ty != "u64" else item)
    lines = [f"  h u64 = {vals[0]}"]
    lines.extend(f"  h = h * 1315423911 + {item}" for item in vals[1:])
    lines.append("  return h")
    return lines


fas = []
header = [
    "typedef uint64_t (*simd_load_fn)(void *);",
    "typedef void (*simd_store_fn)(void *);",
    "enum { SIMD_MASKED_LOAD, SIMD_MASKED_STORE, SIMD_GATHER, SIMD_SCATTER, SIMD_GATHER_BYTES, SIMD_SCATTER_BYTES };",
    "enum { SIMD_U8, SIMD_I16, SIMD_U32, SIMD_U64, SIMD_BOOL };",
    "struct simd_case { const char *name; int op; int ty; int size; int lanes; simd_load_fn load; simd_store_fn store; uint32_t mask; int64_t indices[16]; uint64_t fallback[16]; uint64_t values[16]; };",
]
cases = []
declarations = []

for ty, width, signed, boolean in types:
    for lanes in shapes:
        mask = [lane % 3 != 1 for lane in range(lanes)]
        mask_bits = sum((1 << lane) for lane, active in enumerate(mask) if active)
        fallback = []
        values = []
        for lane in range(lanes):
            if boolean:
                fallback.append(int(lane % 2 == 0))
                values.append(int(lane % 2 == 1))
            else:
                raw_fallback = 0x35 + lane * 41 + lanes * 7
                raw_value = 0x91 + lane * 53 + lanes * 13
                if signed and lane == 1:
                    raw_fallback |= 1 << (width * 8 - 1)
                    raw_value |= 1 << (width * 8 - 1)
                fallback.append(raw_fallback & ((1 << (width * 8)) - 1))
                values.append(raw_value & ((1 << (width * 8)) - 1))
        index_sets = {
            "masked_load": [0] * lanes,
            "masked_store": [0] * lanes,
            "gather": [lane - 4 for lane in range(lanes)],
            "scatter": [lane - 4 for lane in range(lanes)],
            "gather_bytes": [2 * lane - 3 for lane in range(lanes)],
            "scatter_bytes": [2 * lane - 3 for lane in range(lanes)],
        }
        for short, operation, op_code in forms:
            name = f"fas_{short}_{ty}_{lanes}"
            indices = index_sets[operation]
            index_ty = "vec[%d,i64]" % lanes
            mask_source = vector("bool", ["true" if bit else "false" for bit in mask])
            args = []
            lines = ["extern \"C\" {", f"  fn {name}(p addr) " + ("void" if op_code in (1, 3, 5) else "u64") + " {"]
            if op_code in (2, 3, 4, 5):
                lines.append(f"    idx {index_ty} = {vector('i64', [str(x) for x in indices])}")
            lines.append(f"    mask vec[{lanes},bool] = {mask_source}")
            raw_fallback = [value(x, width, signed, boolean) for x in fallback]
            raw_values = [value(x, width, signed, boolean) for x in values]
            if op_code in (0, 2, 4):
                lines.append(f"    fallback vec[{lanes},{ty}] = {vector(ty, raw_fallback)}")
                if op_code == 0:
                    call = "masked_load"
                    call_args = "p, mask, fallback"
                elif op_code == 2:
                    call = "gather"
                    call_args = "p, idx, mask, fallback"
                else:
                    call = "gather_bytes"
                    call_args = "p, idx, mask, fallback"
                lines.append(f"    r vec[{lanes},{ty}] = {call}[{ty}]({call_args})")
                lines.extend(hash_source(name, lanes, ty, boolean))
            else:
                lines.append(f"    values vec[{lanes},{ty}] = {vector(ty, raw_values)}")
                if op_code == 1:
                    lines.append(f"    masked_store[{ty}](p, mask, values)")
                elif op_code == 3:
                    lines.append(f"    scatter[{ty}](p, idx, mask, values)")
                else:
                    lines.append(f"    scatter_bytes[{ty}](p, idx, mask, values)")
                lines.append("    return")
            lines.extend(["  }", "}"])
            fas.extend(lines)
            load = name if op_code in (0, 2, 4) else "NULL"
            store = name if op_code in (1, 3, 5) else "NULL"
            declarations.append(
                f"extern {'void' if op_code in (1, 3, 5) else 'uint64_t'} {name}(void *);"
            )
            indices_c = ", ".join(str(x) for x in indices) or "0"
            fallback_c = ", ".join(str(x) for x in fallback) or "0"
            values_c = ", ".join(str(x) for x in values) or "0"
            ty_code = {"u8": "SIMD_U8", "i16": "SIMD_I16", "u32": "SIMD_U32", "u64": "SIMD_U64", "bool": "SIMD_BOOL"}[ty]
            cases.append(
                f'{{"{name}", {("SIMD_MASKED_LOAD", "SIMD_MASKED_STORE", "SIMD_GATHER", "SIMD_SCATTER", "SIMD_GATHER_BYTES", "SIMD_SCATTER_BYTES")[op_code]}, {ty_code}, {width}, {lanes}, {load}, {store}, {mask_bits}u, {{{indices_c}}}, {{{fallback_c}}}, {{{values_c}}}}}'
            )

header.extend(
    [
        *declarations,
        "static const struct simd_case simd_cases[] = {",
        ",\n".join(cases),
        "};",
        "static const size_t simd_case_count = sizeof(simd_cases) / sizeof(simd_cases[0]);",
    ]
)
(out / "simd_memory_matrix.fas").write_text("\n".join(fas) + "\n")
(out / "simd_memory_cases.h").write_text("\n".join(header) + "\n")
