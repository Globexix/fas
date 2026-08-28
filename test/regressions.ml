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

let parse_error name text =
  match Parser.parse (source text) with
  | Ok _ -> failwith (name ^ ": expected parse rejection")
  | Error _ -> ()

let lower_of text =
  let program = expect_ok (Parser.parse (source text)) in
  let hir = expect_ok (Sema.check program) in
  expect_ok (Lower.lower hir)

let llvm_of text = Ir.render (lower_of text)

let () =
  let uninitialized_field_write =
    llvm_of "struct S { x i64 }\nfn main() i64 { p S\n p.x = 1\n return 0 }\n"
  in
  if not (contains uninitialized_field_write "store i64 1") then
    failwith "place-init: field assignment on an uninitialized aggregate failed";
  let uninitialized_element_write =
    llvm_of "fn main() i64 { a arr[2, i64]\n a[0] = 1\n return 0 }\n"
  in
  if not (contains uninitialized_element_write "store i64 1") then
    failwith "place-init: element assignment on an uninitialized aggregate failed";
  ignore (llvm_of "fn main() i64 { v vec[2, i64]\n v[0] = 1\n return 0 }\n");
  let uninitialized_array_field_write =
    llvm_of
      "struct S { a arr[2, i64] }\nfn main() i64 { s S\n s.a[0] = 1\n return 0 }\n"
  in
  if not (contains uninitialized_array_field_write "store i64 1") then
    failwith "place-init: array field indexing read the whole aggregate";
  let uninitialized_array_address =
    llvm_of
      "fn take(p ptr[i64]) void { return }\n\
       fn main() i64 { a arr[2, i64]\n\
       take(&a[0])\n\
       return 0 }\n"
  in
  if not (contains uninitialized_array_address "call void @take(ptr") then
    failwith "place-init: address of an uninitialized array element failed";
  let uninitialized_address =
    llvm_of
      "fn take(p ptr[i64]) void { return }\n\
       fn main() i64 { x i64\n\
      \ take(&x)\n\
      \ return 0 }\n"
  in
  if not (contains uninitialized_address "call void @take(ptr") then
    failwith "place-init: taking the address of an uninitialized local failed";
  semantic_error "place-init-pointer-intermediate" "use of uninitialized local `p`"
    "fn main() i64 { p ptr[i64]\n p.* = 1\n return 0 }\n";
  semantic_error "place-init-pointer-index-write" "use of uninitialized local `p`"
    "fn main() i64 { p ptr[i64]\n p[0] = 1\n return 0 }\n";
  semantic_error "place-init-pointer-index-read" "use of uninitialized local `p`"
    "fn main() i64 { p ptr[i64]\n x i64 = p[0]\n return x }\n";
  semantic_error "place-init-pointer-index-address" "use of uninitialized local `p`"
    "fn take(p ptr[i64]) void { return }\n\
     fn main() i64 { p ptr[i64]\n\
     take(&p[0])\n\
     return 0 }\n";
  semantic_error "place-init-pointer-index-compound" "use of uninitialized local `p`"
    "fn main() i64 { p ptr[i64]\n p[0] += 1\n return 0 }\n";
  semantic_error "place-init-binding-identity" "use of uninitialized local `x`"
    "fn main() i64 { { x i64 = 1 }\n { x i64\n y i64 = x\n }\n return 0 }\n";
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
  parse_error "fas-006-noalias-parameter" "fn f(x noalias ptr[u8]) i32 { return 0 }\n";
  parse_error "fas-007-aligned-parameter"
    "fn f(x aligned[16] ptr[u8]) i32 { return 0 }\n";
  semantic_error "fas-028-implicit-pointer-erasure"
    "type mismatch: expected ptr[u8], got ptr[i64]"
    "fn take(p ptr[u8]) i32 { return 0 }\n\
     fn main() i32 { x i64 = 1\n\
    \ return take(&x) }\n";
  let explicit_pointer_cast =
    llvm_of
      "fn take(p ptr[u8]) i32 { return 0 }\n\
       fn main() i32 { x i64 = 1\n\
      \ return take(bitcast[ptr[u8]](&x)) }\n"
  in
  if not (contains explicit_pointer_cast "call i32 @take(ptr") then
    failwith "fas-028: explicit pointer bitcast was rejected";
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

  let guarded_div = llvm_of "fn div(x i64, y i64) i64 { return x / y }\n" in
  if
    (not (contains guarded_div "icmp eq i64"))
    || (not (contains guarded_div "call void @llvm.trap()"))
    || (not (contains guarded_div "-9223372036854775808"))
    || not (contains guarded_div "unreachable")
  then failwith "integer-div-trap: missing runtime divisor guard";
  let guarded_rem = llvm_of "fn rem(x i8, y i8) i8 { return x % y }\n" in
  if
    (not (contains guarded_rem "icmp eq i8"))
    || (not (contains guarded_rem "srem i8"))
    || not (contains guarded_rem "call void @llvm.trap()")
  then failwith "integer-rem-trap: missing runtime divisor guard";
  let vector_div =
    llvm_of "fn div(x vec[4,u32], y vec[4,u32]) vec[4,u32] { return x / y }\n"
  in
  if
    (not (contains vector_div "icmp eq <4 x i32>"))
    || (not (contains vector_div "@llvm.vector.reduce.or.v4i1"))
    || not (contains vector_div "udiv <4 x i32>")
  then failwith "integer-vector-div-trap: missing vector divisor guard";
  semantic_error "signed-div-constant-overflow"
    "signed division overflow in constant expression"
    "const X i64 = -9223372036854775808 / -1\nfn main() i64 { return X }\n";
  let signed_rem_const =
    llvm_of "const X i64 = -9223372036854775808 % -1\nfn main() i64 { return X }\n"
  in
  if not (contains signed_rem_const "ret i64 0") then
    failwith "signed-rem-constant-overflow: expected zero remainder";

  semantic_error "fas-026-const-array-write" "cannot modify const array"
    "const K arr[2, i64] = {1, 2}\nfn main() i32 { K[0] = 9\n return 0 }\n";
  semantic_error "fas-026-const-array-address" "cannot modify read-only pointer"
    "const K arr[2, i64] = {1, 2}\n\
     fn main() i32 { p ptr[const i64] = &K[0]\n\
    \ p[0] = 9\n\
    \ return 0 }\n";
  semantic_error "fas-029-string-literal-index" "cannot modify read-only pointer"
    "fn main() i32 { \"x\"[0] = 9\n return 0 }\n";
  semantic_error "fas-029-string-literal-deref" "cannot modify read-only pointer"
    "fn main() i32 { p ptr[const u8] = \"x\"\n p.* = 9\n return 0 }\n";
  semantic_error "fas-029-string-literal-mutable-storage"
    "type mismatch: expected ptr[u8], got ptr[const u8]"
    "fn main() i32 { p ptr[u8] = \"x\"\n return 0 }\n";
  semantic_error "fas-029-read-only-address-propagation"
    "type mismatch: expected ptr[i64], got ptr[const i64]"
    "fn main() i32 { x i64 = 1\n\
    \ p ptr[const i64] = &x\n\
    \ q ptr[i64] = &p.*\n\
    \ return 0 }\n";
  let string_literals =
    llvm_of
      "extern \"C\" { fn take(p ptr[const u8]) void }\n\
       fn main() i32 { take(\"x\")\n\
      \ take(c\"x\")\n\
      \ return 0 }\n"
  in
  if
    (not (contains string_literals "[1 x i8] c\"x\""))
    || not (contains string_literals "[2 x i8] c\"x\\00\"")
  then failwith "fas-030-string-literals: incorrect literal storage";
  let string_pointer =
    llvm_of
      "fn read(p ptr[const u8]) i32 { return zext[i32](p[0]) }\n\
       fn main() i32 { return read(\"x\") }\n"
  in
  if not (contains string_pointer "call i32 @read(ptr") then
    failwith "fas-030-string-literals: read-only pointer call was rejected";
  semantic_error "fas-030-string-literal-nul" "string literal cannot contain NUL"
    "fn main() i32 { c\"a\\0b\"[0]\n return 0 }\n";
  let raw_literal_length =
    llvm_of "fn main() i64 { return zext[i64](len(\"abc\")) }\n"
  in
  if not (contains raw_literal_length "ret i64 3") then
    failwith "fas-031-len: raw literal length is incorrect";
  let c_literal_length =
    llvm_of "fn main() i64 { return zext[i64](len(c\"abc\")) }\n"
  in
  if not (contains c_literal_length "ret i64 3") then
    failwith "fas-031-len: C literal payload length is incorrect";
  let array_length =
    llvm_of
      "const K arr[3, u8] = {1, 2, 3}\n\
       const N usize = len(\"abc\")\n\
       fn main() i64 { return zext[i64](len(K)) + zext[i64](N) }\n"
  in
  if (not (contains array_length "ret i64")) || not (contains array_length "i64 3") then
    failwith "fas-031-len: fixed array length is incorrect";
  semantic_error "fas-031-len-pointer" "len requires a fixed array or string literal"
    "fn main() i64 { p ptr[const u8] = \"abc\"\n return zext[i64](len(p)) }\n";
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

  let zero_bit_counts =
    llvm_of
      "const C8 u8 = ctz(0)\n\
       const L8 u8 = clz(0)\n\
       const C16 u16 = ctz(0)\n\
       const L16 u16 = clz(0)\n\
       const C32 u32 = ctz(0)\n\
       const L32 u32 = clz(0)\n\
       const C64 u64 = ctz(0)\n\
       const L64 u64 = clz(0)\n\
       fn main() i64 { return zext[i64](C8) + zext[i64](L8) +zext[i64](C16) + \
       zext[i64](L16) + zext[i64](C32) +zext[i64](L32) + zext[i64](C64) + \
       zext[i64](L64) }\n"
  in
  if
    (not (contains zero_bit_counts "zext i8 8 to i64"))
    || (not (contains zero_bit_counts "zext i16 16 to i64"))
    || (not (contains zero_bit_counts "zext i32 32 to i64"))
    || not (contains zero_bit_counts "add i64 %v11, 64")
  then failwith "fas-011: zero bit counts did not equal the integer width";
  let runtime_bit_counts =
    llvm_of
      "fn ctz32(x u32) u32 { return ctz(x) }\nfn clz32(x u32) u32 { return clz(x) }\n"
  in
  if
    (not (contains runtime_bit_counts "@llvm.cttz.i32"))
    || (not (contains runtime_bit_counts "@llvm.ctlz.i32"))
    || not (contains runtime_bit_counts "i1 false")
  then failwith "fas-011: runtime zero bit counts were not defined";
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

  List.iter
    (fun name ->
      semantic_error ("reserved-builtin-" ^ name)
        (Printf.sprintf "`%s` is a reserved builtin name" name)
        (Printf.sprintf "fn %s(x i64) i64 { return x }\n" name))
    [ "len"; "shl"; "lshr"; "ashr"; "rotl"; "rotr"; "popcount"; "ctz"; "clz" ];

  let spec_count_limits = { Limits.default with max_specializations = 1 } in
  let repeated_spec_at_count_limit =
    expect_ok
      (Sema.check ~limits:spec_count_limits
         (expect_ok
            (Parser.parse
               (source
                  "fn id[N const usize](x u64) u64 { return x + N }\n\
                   fn main() u64 { return id[3](2) + id[3](3) }\n"))))
  in
  if List.length repeated_spec_at_count_limit.Hir.funcs <> 2 then
    failwith "spec-count-limit: repeated specialization was not deduplicated";
  (match
     Sema.check ~limits:spec_count_limits
       (expect_ok
          (Parser.parse
             (source
                "fn id[N const usize](x u64) u64 { return x + N }\n\
                 fn main() u64 { return id[3](2) + id[4](3) }\n")))
   with
  | Ok _ -> failwith "spec-count-limit: expected rejection at the count limit"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "const specialization count limit exceeded")
      then failwith "spec-count-limit: unexpected diagnostic");

  let spec_depth_limits = { Limits.default with max_specialization_depth = 1 } in
  let repeated_spec_at_depth_limit =
    expect_ok
      (Sema.check ~limits:spec_depth_limits
         (expect_ok
            (Parser.parse
               (source
                  "fn loop[N const i64]() i64 { return loop[N]() }\n\
                   fn main() i64 { return loop[5]() }\n"))))
  in
  if List.length repeated_spec_at_depth_limit.Hir.funcs <> 2 then
    failwith "spec-depth-limit: repeated specialization was not deduplicated";
  (match
     Sema.check ~limits:spec_depth_limits
       (expect_ok
          (Parser.parse
             (source
                "fn inner[N const i64]() i64 { return N }\n\
                 fn outer[M const i64]() i64 { return inner[M]() }\n\
                 fn main() i64 { return outer[5]() }\n")))
   with
  | Ok _ -> failwith "spec-depth-limit: expected rejection at the depth limit"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "const specialization recursion depth limit exceeded")
      then failwith "spec-depth-limit: unexpected diagnostic");
  semantic_error "const-param-bool-rejected" "const parameter type must be an integer"
    "fn id[N const bool](x u64) u64 { return x }\n";

  semantic_error "const-param-pointer-rejected"
    "const parameter type must be an integer"
    "fn id[N const ptr[u8]](x u64) u64 { return x }\n";

  semantic_error "const-param-duplicate" "duplicate generic parameter `N`"
    "fn id[N const usize, N const usize](x u64) u64 { return x + N }\n";

  let i8_min_specialization =
    llvm_of
      "fn b[N const i8](x i8) i32 { return sext[i32](x) + N }\n\
       fn main() i32 { return b[-128](1) }\n"
  in
  if
    (not (contains i8_min_specialization "add i32"))
    || not (contains i8_min_specialization ", -128")
  then failwith "const-param-i8-min: i8 minimum constant was not sign-extended";

  let u8_max_specialization =
    llvm_of
      "fn w[N const u8](x u8) i32 { return zext[i32](x) + N }\n\
       fn main() i32 { return w[255](1) }\n"
  in
  if
    (not (contains u8_max_specialization "add i32"))
    || not (contains u8_max_specialization ", 255")
  then failwith "const-param-u8-max: u8 maximum constant was not zero-extended";

  semantic_error "const-param-i8-overflow" "integer literal is out of range for i8"
    "fn b[N const i8](x i8) i32 { return sext[i32](x) + N }\n\
     fn main() i32 { return b[128](1) }\n";

  semantic_error "const-param-u8-overflow" "integer literal is out of range for u8"
    "fn w[N const u8](x u8) i32 { return zext[i32](x) + N }\n\
     fn main() i32 { return w[256](1) }\n";

  (match
     Lower.lower
       {
         Hir.structs =
           [
             {
               Hir.name = "S";
               fields = [ { Hir.name = "x"; ty = Hir.Opaque "X"; offset = 0 } ];
               size = 8;
               align = 8;
             };
           ];
         consts = [];
         const_arrays = [];
         funcs = [];
         strings = [];
       }
   with
  | Ok _ -> failwith "layout-invariant: malformed struct lowered without error"
  | Error diagnostics ->
      if not (contains (Diag.render_all ~source:None diagnostics) "internal error") then
        failwith "layout-invariant: unexpected diagnostic");

  (match
     Lower.lower
       {
         Hir.structs = [];
         consts = [];
         const_arrays = [];
         strings = [];
         funcs =
           [
             {
               Hir.name = "f";
               params = [ ("x", Hir.Opaque "X", false, None) ];
               ret = Hir.Void;
               body = [];
               attrs = [];
               linkage = Hir.Internal;
               variadic = false;
               asm_body = None;
             };
           ];
       }
   with
  | Ok _ -> failwith "layout-invariant: opaque parameter lowered without error"
  | Error diagnostics ->
      if not (contains (Diag.render_all ~source:None diagnostics) "internal error") then
        failwith "layout-invariant: unexpected diagnostic");

  (match
     Lower.lower
       {
         Hir.structs = [];
         consts = [];
         const_arrays =
           [
             { Hir.name = "A"; ty = Hir.Array (2, Hir.Opaque "X"); elems = [ 0L; 0L ] };
           ];
         strings = [];
         funcs = [];
       }
   with
  | Ok _ -> failwith "layout-invariant: malformed const array lowered without error"
  | Error diagnostics ->
      if not (contains (Diag.render_all ~source:None diagnostics) "internal error") then
        failwith "layout-invariant: unexpected diagnostic");

  if
    not
      (try
         ignore
           (Lower.lower
              {
                Hir.structs = [];
                consts = [];
                const_arrays = [];
                strings = [];
                funcs =
                  [
                    {
                      Hir.name = "f";
                      params = [];
                      ret = Hir.Void;
                      body = [ Hir.Let ("x", Hir.Opaque "X", None, Span.synthetic) ];
                      attrs = [];
                      linkage = Hir.Internal;
                      variadic = false;
                      asm_body = None;
                    };
                  ];
              });
         false
       with Failure message -> contains message "internal error")
  then failwith "layout-invariant: opaque local did not fail with an internal error";
  (match
     Sema.check
       (expect_ok
          (Parser.parse
             (source
                "const MIN i64 = -9223372036854775808\n\
                 const X bool = false && (MIN / -1 == 0)\n")))
   with
  | Ok _ -> ()
  | Error diagnostics ->
      failwith
        ("const-logical-short-circuit: unexpected diagnostic: "
        ^ Diag.render_all ~source:None diagnostics));

  let runtime_short_circuit =
    llvm_of
      "const MIN i64 = -9223372036854775808\n\
       fn f() bool { return false && (MIN / -1 == 0) }\n"
  in
  if not (contains runtime_short_circuit "br") then
    failwith "runtime-logical-short-circuit: branch lowering missing";

  semantic_error "const-logical-static-right" "division by zero in constant expression"
    "const Z bool = false && (1 / 0 == 0)\n";

  semantic_error "runtime-logical-static-right"
    "division by zero is not a defined runtime operation"
    "fn f() bool { return false && (1 / 0 == 0) }\n";

  semantic_error "const-logical-reaches-right" "division by zero in constant expression"
    "const Z bool = true && (1 / 0 == 0)\n";

  semantic_error "runtime-logical-reaches-right"
    "division by zero is not a defined runtime operation"
    "fn f() bool { return true && (1 / 0 == 0) }\n";

  (match
     Sema.check (expect_ok (Parser.parse (source "const B bool = 1 && true\n")))
   with
  | Ok _ -> ()
  | Error diagnostics ->
      failwith
        ("const-logical-mixed: unexpected diagnostic: "
        ^ Diag.render_all ~source:None diagnostics));

  let mixed_runtime = llvm_of "fn f() bool { return 1 && true }\n" in
  if not (contains mixed_runtime "phi") then
    failwith "runtime-logical-mixed: short-circuit lowering missing";

  print_endline "regression tests: 85 passed"
