let source text = Source.create ~file:"regression.fas" ~text

let contains text needle =
  let rec search offset =
    offset + String.length needle <= String.length text
    && (String.sub text offset (String.length needle) = needle || search (offset + 1))
  in
  needle = "" || search 0

let expect_ok = function
  | Ok value -> value
  | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)

let semantic_error name fragment text =
  let program = expect_ok (Parser.parse (source text)) in
  match Sema.check program with
  | Ok _ -> failwith (name ^ ": expected semantic rejection")
  | Error diagnostics ->
      let rendered = Diag.render_all ~source:None diagnostics in
      if not (contains rendered fragment) then
        failwith (name ^ ": unexpected diagnostic: " ^ rendered)

let lower_of text =
  let program = expect_ok (Parser.parse (source text)) in
  let hir = expect_ok (Sema.check program) in
  expect_ok (Lower.lower hir)

let llvm_of text = Ir.render (lower_of text)

let () =
  semantic_error "defer-does-not-initialize" "use of uninitialized local `x`"
    "fn f() i64 { x i64\n defer { x = 1 }\n return x }\n";
  semantic_error "nested-defer" "nested defer is not allowed"
    "fn f() void { defer { defer { } } }\n";
  semantic_error "void-value-return" "void function cannot return a value"
    "extern \"C\" { fn sink() void }\nfn f() void { return sink() }\n";
  semantic_error "nonvoid-fallthrough" "may reach the end without returning"
    "fn f() i64 { }\n";
  semantic_error "const-specialization-arity" "wrong number of arguments"
    "fn id[N const usize](x u64) u64 { return x + N }\n\
     fn main() u64 { return id[3](2, 4) }\n";
  semantic_error "fas-008-i64-positive-overflow"
    "integer literal is out of range for i64"
    "const X i64 = 9223372036854775808\nfn f() i64 { return X }\n";
  semantic_error "fas-008-i64-negative-underflow"
    "integer literal is out of range for i64"
    "const X i64 = -9223372036854775809\nfn f() i64 { return X }\n";
  semantic_error "fas-020-positive-narrow-overflow"
    "integer literal is out of range for i32"
    "fn f() i32 { x i32 = 18446744073709551615\n return x }\n";
  semantic_error "fas-020-negative-narrow-overflow"
    "integer literal is out of range for i32"
    "fn f() i32 { x i32 = -18446744073709551615\n return x }\n";
  semantic_error "fas-020-hex-narrow-overflow" "integer literal is out of range for i32"
    "fn f() i32 { x i32 = 0xffffffffffffffff\n return x }\n";
  let u64_max = llvm_of "fn f() u64 { return 18446744073709551615 }\n" in
  if not (contains u64_max "ret i64 -1") then
    failwith "fas-020: valid u64 maximum literal was rejected";
  let signed_const_eval =
    llvm_of
      "const X i8 = -4 / 2\n\
       const R i8 = -5 % 2\n\
       const A i8 = ashr(-4, 1)\n\
       const B bool = -1 < 0\n\
       fn main() i32 { return sext[i32](X) + sext[i32](R) + sext[i32](A) + \
       sext[i32](B) }\n"
  in
  if
    (not (contains signed_const_eval "sext i8 254 to i32"))
    || (not (contains signed_const_eval "sext i8 255 to i32"))
    || not (contains signed_const_eval "zext i1 true to i32")
  then failwith "fas-001: signed constant evaluation used masked bit patterns";
  let nested_align =
    llvm_of
      "struct Inner @align(16) { x u8 y u8 }\n\
       struct Outer { p u8 i Inner }\n\
       fn main() i32 {\n\
       a arr[2,Outer]\n\
       a[0].i.y = 7\n\
       a[1].p = 9\n\
       return zext[i32](a[0].i.y)\n\
       }\n"
  in
  if
    (not (contains nested_align "%struct.Inner = type { i8, i8, [14 x i8] }"))
    || not
         (contains nested_align "%struct.Outer = type { i8, [15 x i8], %struct.Inner }")
  then failwith "fas-003: nested aligned struct layout lost internal padding";
  semantic_error "fas-009-duplicate-opaque" "duplicate type `x`"
    "opaque x\nopaque x\nfn main() i32 { return 0 }\n";
  semantic_error "fas-005-void-ternary" "ternary arms cannot have void type"
    "fn a() void { }\n\
     fn b() void { }\n\
     fn main() i32 {\n\
    \  c bool = true\n\
    \  c ? a() : b()\n\
    \  return 0\n\
     }\n";
  semantic_error "fas-006-noalias-non-pointer" "noalias requires a pointer parameter"
    "fn f(x noalias i32) i32 { return x }\nfn main() i32 { return f(0) }\n";
  semantic_error "fas-007-non-power-of-two-parameter-alignment"
    "alignment must be a positive power of two"
    "fn f(x aligned[3] ptr[u8]) i32 { return 0 }\nfn main() i32 { return f(null) }\n";
  semantic_error "fas-007-zero-parameter-alignment"
    "alignment must be a positive power of two"
    "fn f(x aligned[0] ptr[u8]) i32 { return 0 }\n";
  let generic_attrs =
    llvm_of
      "@inline\n\
       fn id[N const i64](x i64) i64 { return x }\n\
       fn main() i64 { return id[3](4) }\n"
  in
  if not (contains generic_attrs "inlinehint") then
    failwith "fas-019: specialization did not inherit template attributes";
  semantic_error "fas-017-unsupported-target" "unsupported target `wasm32-junk`"
    "@target(\"wasm32-junk\")\nfn f() i64 { return 7 }\nfn main() i64 { return f() }\n";
  let target_attr = llvm_of "@target(\"zen3\")\nfn f() i64 { return 7 }\n" in
  if not (contains target_attr "\"target-cpu\"=\"znver3\"") then
    failwith "fas-017: supported target attribute was not preserved";
  semantic_error "fas-002-switch-default-init-leak" "use of uninitialized local `x`"
    "fn main() i32 {\n\
    \     x i32\n\
    \     switch 0 {\n\
    \       case 0: { }\n\
    \       default: { x = 5 }\n\
    \     }\n\
    \     return x\n\
    \   }\n";
  semantic_error "fas-002-switch-cross-arm-init-leak" "use of uninitialized local `x`"
    "fn main() i32 {\n\
    \     x i32\n\
    \     y i32 = 0\n\
    \     switch 1 {\n\
    \       case 0: { x = 7 }\n\
    \       case 1: { y = x }\n\
    \     }\n\
    \     return y\n\
    \   }\n";
  semantic_error "fas-004-index-void-pointer" "void pointers cannot be indexed"
    "fn f(p ptr[void]) i32 { p[0]\n return 0}";
  semantic_error "fas-004-deref-void-pointer" "cannot dereference a void pointer"
    "fn f(p ptr[void]) i32 { p.*\nreturn 0 }";
  semantic_error "fas-004-assign-index-void-pointer" "void pointers cannot be indexed"
    "fn sink() void { }\nfn f(p ptr[void]) i32 { p[0] = sink()\n return 0}";
  semantic_error "fas-004-assign-deref-void-pointer" "cannot dereference a void pointer"
    "fn sink() void { }\nfn f(p ptr[void]) i32 { p.* = sink()\nreturn 0 }";

  let rem = llvm_of "fn rem(x i8) i8 { return x % -1 }\n" in
  if
    (not (contains rem "srem i8"))
    || (not (contains rem "br i1"))
    || not (contains rem "phi i8")
  then failwith "signed-rem-poison-guard: missing branch/phi guard";

  semantic_error "fas-026-const-array-write" "cannot modify const array"
    "const K arr[2, i64] = {1, 2}\nfn main() i32 { K[0] = 9\n return 0 }\n";
  semantic_error "fas-026-const-array-address" "cannot take address of const array"
    "const K arr[2, i64] = {1, 2}\n\
     fn main() i32 { p ptr[i64] = &K[0]\n\
    \ p[0] = 9\n\
    \ return 0 }\n";
  let const_array_value =
    llvm_of
      "const G arr[2, i64] = {7, 8}\n\
       fn take(p arr[2, i64]) i64 { return p[0] + p[1] }\n\
       fn main() i64 { a arr[2, i64] = G\n\
      \ return take(G) }\n"
  in
  if
    (not (contains const_array_value "load [2 x i64], ptr"))
    || contains const_array_value "store [2 x i64] ptr"
  then failwith "fas-013: const array value was lowered as a pointer";

  let wide_shift =
    llvm_of
      "const C u8 = shl(1, 8)\n\
       fn run(x u8, n u8) u8 { return shl(x, n) }\n\
       fn main() i32 { return zext[i32](C) - zext[i32](run(1, 8)) }\n"
  in
  if not (contains wide_shift "and i8") then
    failwith "fas-010: runtime shift count was not reduced modulo width";

  semantic_error "fas-011-ctz-zero-const"
    "ctz/clz of zero is undefined in constant expression"
    "const C u8 = ctz(0)\nfn main() i32 { return 0 }\n";
  semantic_error "fas-011-clz-zero-const"
    "ctz/clz of zero is undefined in constant expression"
    "const C u8 = clz(0)\nfn main() i32 { return 0 }\n";

  semantic_error "fas-027-const-array-function-collision" "duplicate declaration `F`"
    "const F arr[2, i64] = {1, 2}\n\
     fn F() i64 { return 3 }\n\
     fn main() i64 { return F() }\n";

  semantic_error "fas-021-local-aggregate-limit"
    "aggregate element count exceeds the configured limit"
    "fn main() i32 { v vec[1000001,u8]\n return 0 }\n";
  semantic_error "fas-021-nested-aggregate-limit"
    "aggregate element count exceeds the configured limit"
    "fn main() i32 { a arr[100000,arr[20,u8]]\n return 0 }\n";

  let bool_sext =
    llvm_of "const B i64 = sext[i64](true)\nfn f() i64 { return sext[i64](true) }\n"
  in
  if not (contains bool_sext "zext i1 true to i64") then
    failwith "bool-sext: expected value-preserving zext lowering";

  let vector_rotate =
    llvm_of
      "fn f() i64 { x vec[4,u32] = splat(1)\n\
      \ y vec[4,u32] = rotl(x, 1)\n\
      \ return zext[i64](y[0]) }\n"
  in
  if
    (not (contains vector_rotate "@llvm.fshl.v4i32"))
    || not
         (contains vector_rotate
            "declare <4 x i32> @llvm.fshl.v4i32(<4 x i32>, <4 x i32>, <4 x i32>)")
  then failwith "vector-rotate: missing typed vector intrinsic";

  let hoisted =
    lower_of
      "fn hoist(p i64) i64 {\n\
      \ x i64 = p\n\
      \ if p > 0 { y i64 = x + 1\n\
      \ x = y }\n\
      \ while x < 3 { z i64 = x\n\
      \ x = z + 1 }\n\
      \ defer { cleanup i64 = x\n\
      \ x = cleanup }\n\
      \ return x }\n"
  in
  let hoist_fn =
    List.find (fun (func : Ir.func) -> func.name = "hoist") hoisted.Ir.funcs
  in
  let alloca_blocks =
    List.concat_map
      (fun (block : Ir.block) ->
        List.filter_map
          (function Ir.Alloca _ -> Some block.id | _ -> None)
          block.instrs)
      hoist_fn.blocks
  in
  if List.length alloca_blocks <> 5 || not (List.for_all (( = ) 0) alloca_blocks) then
    failwith "entry-alloca: lexical local storage escaped the entry block";

  let token_limits = { Limits.default with max_tokens = 1 } in
  (match Lexer.lex ~limits:token_limits (source "x /* trailing trivia */ ") with
  | Ok [ { Token.kind = Token.Ident "x"; _ }; { kind = Token.Eof; _ } ] -> ()
  | Ok _ -> failwith "token-limit: exact limit returned unexpected tokens"
  | Error diagnostics ->
      failwith
        ("token-limit: exact limit was rejected: "
        ^ Diag.render_all ~source:None diagnostics));
  (match Lexer.lex ~limits:token_limits (source "x y") with
  | Error diagnostics ->
      if
        not (contains (Diag.render_all ~source:None diagnostics) "token limit exceeded")
      then failwith "token-limit: unexpected diagnostic"
  | Ok _ -> failwith "token-limit: expected rejection");

  semantic_error "for-step-discard" "use of uninitialized local `y`"
    "fn f() i64 { y i64\n for i i32 = 0; i < 0; y = 5 { }\n return y }\n";

  let vec_comp =
    llvm_of
      "fn f() i32 { v vec[4,i32] = splat(1)\n      v[0] += 5\n      return v[0] }\n"
  in
  if
    (not (contains vec_comp "extractelement"))
    || not (contains vec_comp "insertelement")
  then failwith "vec-compound-assign: expected extract/insert on vec lane write";

  let _ =
    lower_of
      "fn f() i64 { b vec[4,bool] = splat(true)\n\
      \                    x bool = b[0]\n\
      \                    return 0 }\n"
  in
  let _ =
    lower_of
      "fn f() i64 { v vec[2,ptr[u8]] = splat(null)\n                    return 0 }\n"
  in
  let _ =
    lower_of
      "fn f(n i32) i32 {\n\
      \  x i32\n\
      \  switch n {\n\
      \    case 0: { x = 1 }\n\
      \    case 1: { x = 2 }\n\
      \    default: { x = 3 }\n\
      \  }\n\
      \  return x\n\
      \  }\n"
  in

  let shadowed_template =
    llvm_of
      "const N i64 = 100\n\
       fn f[N const i64]() i64 { return N }\n\
       fn main() i64 { return f[5]() }\n"
  in
  if not (contains shadowed_template "ret i64 5") then
    failwith "fas-015: global const shadowed template const parameter";

  let nested_shadowed_template =
    llvm_of
      "const N i64 = 100\n\
       fn inner[N const i64]() i64 { return N }\n\
       fn outer[N const i64]() i64 { return inner[N]() }\n\
       fn main() i64 { return outer[5]() }\n"
  in
  if not (contains nested_shadowed_template "ret i64 5") then
    failwith "fas-015: nested specialization used global const over template parameter";

  let signed_narrow_specialization =
    llvm_of
      "fn b8[N const i8](x i8) i32 { return sext[i32](x) + N }\n\
       fn main() i32 { return b8[-5](2) }\n"
  in
  if
    (not (contains signed_narrow_specialization "add i32"))
    || not (contains signed_narrow_specialization ", -5")
  then
    failwith "signed-narrow-specialization: negative i8 constant was not sign-extended";

  semantic_error "bool-vec-arith-rejected"
    "arithmetic requires integer or vector operands"
    "fn f() i64 { b vec[4,bool] = splat(true)\n\
    \               d vec[4,bool] = b & b\n\
    \               return 0 }\n";

  print_endline "regression tests: 41 passed"
