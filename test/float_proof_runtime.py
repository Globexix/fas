import pathlib
import re
import os
import sys

edge_tables = {}
for bits, text in re.findall(r"let edges(32|64)\s*=\s*\[\|(.+?)\|\]", pathlib.Path(__file__).with_name("float_proof.ml").read_text(), re.S):
    edge_tables[int(bits)] = [1 << 63 if item.strip() == "Int64.min_int" else int(item.strip().removesuffix("L"), 0) for item in text.split(";") if item.strip()]

out = []
for bits, maximum in [(32, 64), (64, 32)]:
    for shape_index in range(0, maximum + 1):
        lanes = max(1, shape_index)
        group = (0 if bits == 32 else 4) + (lanes - 1) // 16
        if len(sys.argv) > 2 and group != int(sys.argv[2]):
            continue
        if os.getenv("FULL") != "1" and lanes not in [1, 2, 3, 4, 8, 16, 32, 64]:
            continue
        scalar = shape_index == 0

        def shape(t):
            return t if scalar else f"vec[{lanes},{t}]"

        float_type = shape(f"f{bits}")
        uint_type = shape(f"u{bits}")
        name = f"proof{bits}_{shape_index}"
        start = len(out)
        out.append(f'extern "C" {{ fn {name}(ap addr, bp addr, cp addr, output addr) void {{')
        for p in "abc":
            out.append(f"{p} {float_type} = {p}p[{float_type}]")
        results = []
        for expression in ["a + b", "a - b", "a * b", "a / b", "sqrt(a)", "floor(a)", "ceil(a)", "round(a)", "abs(a)", "-a", "fma(a,b,c)"]:
            results.append((float_type, expression, f"u{bits}"))
        for op in ["==", "!=", "<", "<=", ">", ">="]:
            results.append((shape("bool"), f"a {op} b", "bool"))
        if lanes <= 32:
            target = 96 - bits
            results.append((shape(f"f{target}"), f"cvt[{shape(f'f{target}')}](a)", f"u{target}"))
        for width in [8, 16, 32, 64, "size"]:
            width_bits = 64 if width == "size" else width
            if lanes * width_bits > 2048:
                continue
            for prefix in ["u", "i"]:
                target = shape(f"{prefix}{width}")
                results.append((target, f"cvt[{target}](a)", f"u{width}"))
                results.append((float_type, f"cvt[{float_type}](ap[{target}])", f"u{bits}"))
        for i, (target, expression, _) in enumerate(results):
            out.append(f"r{i} {target} = {expression}")
        out.append("lane usize = 0")
        out.append(f"while lane < {lanes} {{")
        for i, (_, _, integer) in enumerate(results):
            lane_value = f"r{i}" if scalar else f"r{i}[lane]"
            if integer == "bool":
                value = f"zext[u64]({lane_value})"
            else:
                value = f"bitcast[{integer}]({lane_value})"
                if integer == "usize":
                    value = f"bitcast[u64]({value})"
                elif integer != "u64":
                    value = f"zext[u64]({value})"
            out.append(f"output[u64, {i * lanes} + lane] = {value}")
        out.append("lane += 1\n}\n} }")
        edges = edge_tables[bits]
        constant = []
        globals = []
        for line in out[start:]:
            line = line.replace(name, f"proof_const{bits}_{shape_index}")
            for index, p in enumerate("abc"):
                if line == f"{p} {float_type} = {p}p[{float_type}]":
                    values = [f"0x{edges[(lane + index * 7) % len(edges)]:x}" for lane in range(lanes)]
                    value = values[0] if scalar else "{" + ",".join(values) + "}"
                    globals.append(f"const {p}_bits {uint_type} = {value}")
                    line = f"const {p} {float_type} = bitcast[{float_type}]({p}_bits)"
            if line.startswith("r"):
                line = "const " + line
            if "ap[" in line:
                for i, (target, expression, _) in enumerate(results):
                    if expression in line:
                        input_type = expression.split("ap[", 1)[1][:-2]
                        kind = re.search(r"([ui])(8|16|32|64|size)", input_type)
                        width = 64 if kind[2] == "size" else int(kind[2])
                        storage = b"".join(edges[lane % len(edges)].to_bytes(bits // 8, "little") for lane in range(lanes))
                        storage += bytes(max(0, lanes * (width // 8) - len(storage)))
                        values = [f"0x{int.from_bytes(storage[lane * (width // 8):(lane + 1) * (width // 8)], 'little'):x}" for lane in range(lanes)]
                        value = values[0] if scalar else "{" + ",".join(values) + "}"
                        input_name = f"r{len(results) + i}"
                        globals.append(f"const {input_name} {shape('u' + kind[2])} = {value}")
                        input_value = input_name if kind[1] == "u" else f"bitcast[{input_type}]({input_name})"
                        line = line.replace(f"ap[{input_type}]", input_value)
                        break
            if line.startswith("const "):
                globals.append(line)
            else:
                constant.append(line)
        for line in globals + constant:
            out.append(re.sub(r"\b([abc](?:_bits)?|r\d+)\b", lambda match: name + "_" + match[1], line))
pathlib.Path(sys.argv[1]).write_text("\n".join(out) + "\n")

