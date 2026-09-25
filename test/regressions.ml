let source text = Source.create ~file:"regression.fas" ~text
let checks_run = ref 0

let contains text needle =
  let rec search offset =
    offset + String.length needle <= String.length text
    && (String.sub text offset (String.length needle) = needle || search (offset + 1))
  in
  needle = "" || search 0

let positions text needle =
  let rec search offset acc =
    if offset + String.length needle > String.length text then List.rev acc
    else if String.sub text offset (String.length needle) = needle then
      search (offset + 1) (offset :: acc)
    else search (offset + 1) acc
  in
  if needle = "" then [] else search 0 []

let expect_ok = function
  | Ok value -> value
  | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)

let parse_file file text = expect_ok (Parser.parse (Source.create ~file ~text))

let check_files files =
  incr checks_run;
  let items =
    files
    |> List.map (fun (file, text) -> (parse_file file text).Ast.items)
    |> List.concat
  in
  Sema.check { Ast.items }

let semantic_diagnostics text =
  incr checks_run;
  let program = expect_ok (Parser.parse (source text)) in
  match Sema.check program with
  | Ok _ -> failwith "expected semantic rejection"
  | Error diagnostics -> diagnostics

let semantic_messages text =
  List.map (fun (diagnostic : Diag.t) -> diagnostic.message) (semantic_diagnostics text)

let semantic_error name fragment text =
  incr checks_run;
  let program = expect_ok (Parser.parse (source text)) in
  match Sema.check program with
  | Ok _ -> failwith (name ^ ": expected semantic rejection")
  | Error diagnostics ->
      let rendered = Diag.render_all ~source:None diagnostics in
      if not (contains rendered fragment) then
        failwith (name ^ ": unexpected diagnostic: " ^ rendered)

let parse_error name text =
  incr checks_run;
  match Parser.parse (source text) with
  | Ok _ -> failwith (name ^ ": expected parse rejection")
  | Error _ -> ()

let parse_messages text =
  incr checks_run;
  match Parser.parse (source text) with
  | Ok _ -> failwith "expected parse rejection"
  | Error diagnostics ->
      List.map (fun (diagnostic : Diag.t) -> diagnostic.message) diagnostics

let parse_error_message name fragment text =
  incr checks_run;
  match Parser.parse (source text) with
  | Ok _ -> failwith (name ^ ": expected parse rejection")
  | Error diagnostics ->
      let rendered = Diag.render_all ~source:None diagnostics in
      if not (contains rendered fragment) then
        failwith (name ^ ": unexpected diagnostic: " ^ rendered)

let cli_error name fragment args =
  incr checks_run;
  match Cli.parse (Array.of_list ("fas" :: args)) with
  | Error message when contains message fragment -> ()
  | Error message -> failwith (name ^ ": unexpected diagnostic: " ^ message)
  | Ok _ -> failwith (name ^ ": expected CLI rejection")

let cli_run args =
  incr checks_run;
  match Cli.parse (Array.of_list ("fas" :: args)) with
  | Ok (Cli.Run config) -> config
  | Ok Cli.Help -> failwith "expected compiler invocation"
  | Error message -> failwith message

let lower_of text =
  incr checks_run;
  let program = expect_ok (Parser.parse (source text)) in
  let hir = expect_ok (Sema.check program) in
  expect_ok (Lower.lower hir)

let llvm_of text = Ir.render (lower_of text)

let lower_struct_error name fragment (struct_def : Hir.struct_def) =
  incr checks_run;
  match
    Lower.lower
      {
        Hir.structs = [ struct_def ];
        consts = [];
        const_arrays = [];
        funcs = [];
        strings = [];
      }
  with
  | Ok _ -> failwith (name ^ ": malformed struct lowered without error")
  | Error diagnostics ->
      let rendered = Diag.render_all ~source:None diagnostics in
      if not (contains rendered fragment) then
        failwith (name ^ ": unexpected diagnostic: " ^ rendered)

let lower_function_error name fragment params body =
  incr checks_run;
  match
    Lower.lower
      {
        Hir.structs = [];
        consts = [];
        const_arrays = [];
        funcs =
          [
            {
              Hir.name;
              params;
              ret = Hir.Void;
              body = Hir.Statements body;
              linkage = Hir.Internal;
              variadic = false;
            };
          ];
        strings = [];
      }
  with
  | Ok _ -> failwith (name ^ ": malformed HIR lowered without error")
  | Error diagnostics ->
      let rendered = Diag.render_all ~source:None diagnostics in
      if not (contains rendered fragment) then
        failwith (name ^ ": unexpected diagnostic: " ^ rendered)

let () =
  let expect_layout name expected ty =
    match Hir.layout [] ty with
    | Ok actual when actual = expected -> ()
    | Ok (size, align) ->
        failwith
          (Printf.sprintf "%s: expected layout (%d, %d), got (%d, %d)" name
             (fst expected) (snd expected) size align)
    | Error message -> failwith (name ^ ": " ^ message)
  in
  expect_layout "layout-v3i1" (1, 1) (Hir.Vec (3, Hir.Bool));
  expect_layout "layout-v9i1" (2, 2) (Hir.Vec (9, Hir.Bool));
  expect_layout "layout-v3i8" (4, 4) (Hir.Vec (3, Hir.Int Hir.U8));
  expect_layout "layout-v3i32" (16, 16) (Hir.Vec (3, Hir.Int Hir.I32));
  expect_layout "layout-v2i64" (16, 16) (Hir.Vec (2, Hir.Int Hir.I64));
  expect_layout "layout-array-v3i32" (32, 16)
    (Hir.Array (2, Hir.Vec (3, Hir.Int Hir.I32)));
  if Hir.ty_equal (Hir.Int Hir.Usize) (Hir.Int Hir.U64) then
    failwith "usize-identity: usize collapsed into u64";
  if Hir.ty_equal (Hir.Int Hir.Isize) (Hir.Int Hir.I64) then
    failwith "isize-identity: isize collapsed into i64";
  let target32 = { Target_layout.current with pointer_size = 4; pointer_align = 4 } in
  (match Hir.layout ~target:target32 [] (Hir.Int Hir.Usize) with
  | Ok (4, 4) -> ()
  | Ok (size, align) ->
      failwith (Printf.sprintf "usize-layout: expected (4, 4), got (%d, %d)" size align)
  | Error message -> failwith ("usize-layout: " ^ message));
  (match Hir.layout ~target:target32 [] (Hir.Int Hir.Isize) with
  | Ok (4, 4) -> ()
  | Ok (size, align) ->
      failwith (Printf.sprintf "isize-layout: expected (4, 4), got (%d, %d)" size align)
  | Error message -> failwith ("isize-layout: " ^ message));
  semantic_error "usize-distinct-from-u64" "type mismatch: expected usize, got u64"
    "fn convert(value u64) usize { return value }\n";
  semantic_error "u64-distinct-from-usize" "type mismatch: expected u64, got usize"
    "fn convert(value usize) u64 { return value }\n";
  semantic_error "isize-distinct-from-i64" "type mismatch: expected isize, got i64"
    "fn convert(value i64) isize { return value }\n";
  semantic_error "i64-distinct-from-isize" "type mismatch: expected i64, got isize"
    "fn convert(value isize) i64 { return value }\n";
  semantic_error "usize-call-distinct-from-u64" "type mismatch: expected usize, got u64"
    "fn take(value usize) void { return }\nfn use(value u64) void { take(value) }\n";
  semantic_error "named-bool-constant-keeps-type"
    "type mismatch: expected i32, got bool"
    "const Flag bool = true\nfn main() i32 { value i32 = Flag\n return value }\n";
  semantic_error "named-i8-constant-local-keeps-type"
    "type mismatch: expected i32, got i8"
    "const Small i8 = 7\nfn main() i32 { value i32 = Small\n return value }\n";
  semantic_error "named-negative-i8-constant-keeps-type"
    "type mismatch: expected u16, got i8"
    "const Negative i8 = -1\nfn main() i32 { value u16 = Negative\n return 0 }\n";
  semantic_error "named-i8-constant-return-keeps-type"
    "type mismatch: expected i32, got i8"
    "const Small i8 = 7\nfn value() i32 { return Small }\n";
  semantic_error "named-i8-constant-argument-keeps-type"
    "type mismatch: expected i32, got i8"
    "const Small i8 = 7\n\
     fn take(value i32) void { return }\n\
     fn use() void { take(Small) }\n";
  semantic_error "named-i8-constant-binary-keeps-type"
    "binary operands must have the same type"
    "const Small i8 = 7\nfn add(value i32) i32 { return Small + value }\n";
  semantic_error "named-i8-constant-generic-argument-keeps-type"
    "const argument type mismatch"
    "const Small i8 = 7\n\
     fn value[N const i32]() i32 { return N }\n\
     fn use() i32 { return value[Small]() }\n";
  let named_constant_explicit_conversions =
    llvm_of
      "const Negative i8 = -1\n\
       const Byte u8 = 255\n\
       fn signed_value() i32 { return sext[i32](Negative) }\n\
       fn unsigned_value() i32 { return zext[i32](Byte) }\n\
       fn byte_value() u8 { return Byte }\n\
       fn contextual_literal() i32 { return 1 }\n"
  in
  List.iter
    (fun expected ->
      if not (contains named_constant_explicit_conversions expected) then
        failwith ("named-constant-explicit-conversions: missing `" ^ expected ^ "`"))
    [
      "define internal i32 @signed_value";
      "sext i8 255 to i32";
      "define internal i32 @unsigned_value";
      "zext i8 255 to i32";
      "define internal i8 @byte_value";
      "ret i8 255\n";
      "ret i32 1\n";
    ];
  semantic_error "constant-zext-must-widen"
    "illegal cast for source and destination widths"
    "const X u8 = zext[u8](256)\nfn main() u8 { return X }\n";
  semantic_error "constant-zext-equal-width"
    "illegal cast for source and destination widths"
    "const A u8 = 1\nconst X u8 = zext[u8](A)\nfn main() u8 { return X }\n";
  semantic_error "constant-sext-equal-width"
    "illegal cast for source and destination widths"
    "const A i32 = 1\nconst X i32 = sext[i32](A)\nfn main() i32 { return X }\n";
  semantic_error "constant-trunc-equal-width"
    "illegal cast for source and destination widths"
    "const A i32 = 1\nconst X i32 = trunc[i32](A)\nfn main() i32 { return X }\n";
  semantic_error "runtime-zext-equal-width"
    "illegal cast for source and destination widths"
    "fn f(value u8) u8 { return zext[u8](value) }\n";
  semantic_error "runtime-sext-equal-width"
    "illegal cast for source and destination widths"
    "fn f(value i32) i32 { return sext[i32](value) }\n";
  semantic_error "runtime-trunc-equal-width"
    "illegal cast for source and destination widths"
    "fn f(value i32) i32 { return trunc[i32](value) }\n";
  semantic_error "constant-trunc-bool-source"
    "illegal cast for source and destination widths"
    "const X u8 = trunc[u8](true)\nfn main() u8 { return X }\n";
  semantic_error "runtime-trunc-bool-source"
    "illegal cast for source and destination widths"
    "fn f(value bool) u8 { return trunc[u8](value) }\n";
  semantic_error "constant-vector-bitcast-width"
    "illegal cast for source and destination widths"
    "const A i32 = 1\n\
     const X vec[2,i32] = bitcast[vec[2,i32]](A)\n\
     fn main() i32 { return 0 }\n";
  let constant_vector_bitcasts =
    llvm_of
      "const A i64 = 1\n\
       const X vec[2,i32] = bitcast[vec[2,i32]](A)\n\
       const Y vec[4,u16] = bitcast[vec[4,u16]](X)\n\
       const Z i64 = bitcast[i64](Y)\n\
       fn x_lane() i32 { return X[1] }\n\
       fn y_lane() u16 { return Y[2] }\n\
       fn main() i32 { return trunc[i32](Z) }\n"
  in
  List.iter
    (fun marker ->
      if not (contains constant_vector_bitcasts marker) then
        failwith ("constant-vector-bitcasts: missing `" ^ marker ^ "`"))
    [ "<i32 1, i32 0>"; "<i16 1, i16 0, i16 0, i16 0>"; "trunc i64 1 to i32" ];
  let constant_casts =
    llvm_of
      "const Byte u8 = 255\n\
       const Negative i8 = -1\n\
       const Word u16 = 258\n\
       const OddWord u16 = 259\n\
       const Zero bool = trunc[bool](Word)\n\
       const One bool = trunc[bool](OddWord)\n\
       const WideUnsigned u16 = zext[u16](Byte)\n\
       const WideSigned i16 = sext[i16](Negative)\n\
       const Narrow u8 = trunc[u8](Word)\n\
       const Bits i8 = bitcast[i8](Byte)\n\
       const SignedTruth i16 = sext[i16](true)\n\
       fn constants() i32 {\n\
      \ return zext[i32](WideUnsigned) + sext[i32](WideSigned) +zext[i32](Narrow) + \
       sext[i32](Bits) + sext[i32](SignedTruth) +zext[i32](Zero) +zext[i32](One)\n\
       }\n\
       fn runtime_sext(value bool) i16 { return sext[i16](value) }\n\
       fn runtime_low_bit(value u8) bool { return trunc[bool](value) }\n"
  in
  List.iter
    (fun marker ->
      if not (contains constant_casts marker) then
        failwith ("constant-casts: missing `" ^ marker ^ "`"))
    [
      "zext i16 255 to i32";
      "sext i16 65535 to i32";
      "zext i8 2 to i32";
      "sext i8 255 to i32";
      "zext i1 false to i32";
      "zext i1 true to i32";
      "sext i1 ";
      "trunc i8 ";
    ];
  let target_width_integers =
    llvm_of
      "struct Pair { left u8 right u64 }\n\
       fn to_u64(value usize) u64 { return bitcast[u64](value) }\n\
       fn to_i64(value isize) i64 { return bitcast[i64](value) }\n\
       fn unsigned_div(left usize, right usize) usize { return left / right }\n\
       fn signed_div(left isize, right isize) isize { return left / right }\n\
       fn sizes(values arr[3,u8]) usize {\n\
      \ return sizeof[Pair] + alignof[Pair] + offsetof[Pair,right] + len(values)\n\
       }\n"
  in
  List.iter
    (fun marker ->
      if not (contains target_width_integers marker) then
        failwith ("target-width-integers: missing `" ^ marker ^ "`"))
    [
      "define internal i64 @to_u64(i64";
      "define internal i64 @to_i64(i64";
      "udiv i64";
      "sdiv i64";
    ];
  let integer_vector_bitcasts =
    llvm_of
      "fn same_shape(values vec[4,i32]) vec[4,u32] {\n\
      \ return bitcast[vec[4,u32]](values)\n\
      \ }\n\
      \ fn reshape(value u64) u64 {\n\
      \ words vec[2,u32] = bitcast[vec[2,u32]](value)\n\
      \ bytes vec[8,u8] = bitcast[vec[8,u8]](words)\n\
      \ return bitcast[u64](bytes)\n\
      \ }\n\
      \ fn flags(value vec[8,bool]) u8 { return bitcast[u8](value) }\n"
  in
  if List.length (positions integer_vector_bitcasts "bitcast ") <> 5 then
    failwith "integer-vector-bitcast: expected five mechanical LLVM bitcasts";
  List.iter
    (fun forbidden ->
      if contains integer_vector_bitcasts forbidden then
        failwith ("integer-vector-bitcast: unexpected `" ^ forbidden ^ "` lowering"))
    [ "extractelement"; "insertelement"; "shufflevector" ];
  semantic_error "integer-vector-bitcast-width"
    "illegal cast for source and destination widths"
    "fn f(value u64) vec[4,u32] { return bitcast[vec[4,u32]](value) }\n";
  semantic_error "integer-vector-bitcast-implicit"
    "type mismatch: expected vec[4, u32], got vec[4, i32]"
    "fn f(value vec[4,i32]) vec[4,u32] { return value }\n";
  semantic_error "integer-vector-bitcast-array"
    "illegal cast for source and destination widths"
    "fn f(value arr[2,u32]) vec[2,u32] { return bitcast[vec[2,u32]](value) }\n";
  semantic_error "integer-vector-bitcast-struct"
    "illegal cast for source and destination widths"
    "struct Pair { left u32 right u32 }\n\
    \ fn f(value Pair) vec[2,u32] { return bitcast[vec[2,u32]](value) }\n";
  semantic_error "integer-vector-bitcast-pointer"
    "illegal cast for source and destination widths"
    "fn f(value ptr[u8]) vec[1,u64] { return bitcast[vec[1,u64]](value) }\n";
  let integer_vector_conversions =
    llvm_of
      "fn widen_unsigned(value vec[4,u8]) vec[4,u16] {\n\
      \ return zext[vec[4,u16]](value)\n\
       }\n\
       fn widen_signed(value vec[4,i8]) vec[4,i16] {\n\
      \ return sext[vec[4,i16]](value)\n\
       }\n\
       fn narrow(value vec[4,u16]) vec[4,u8] {\n\
      \ return trunc[vec[4,u8]](value)\n\
       }\n\
       fn truth_bits(value vec[4,u8]) vec[4,bool] {\n\
      \ return trunc[vec[4,bool]](value)\n\
       }\n\
       fn widen_bool_unsigned(value vec[4,bool]) vec[4,u8] {\n\
      \ return zext[vec[4,u8]](value)\n\
       }\n\
       fn widen_bool_signed(value vec[4,bool]) vec[4,i8] {\n\
      \ return sext[vec[4,i8]](value)\n\
       }\n"
  in
  List.iter
    (fun marker ->
      if not (contains integer_vector_conversions marker) then
        failwith ("integer-vector-conversions: missing `" ^ marker ^ "`"))
    [ "zext <4 x i8>"; "sext <4 x i8>"; "trunc <4 x i16>"; "trunc <4 x i8>" ];
  semantic_error "integer-vector-zext-lane-count"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,u8]) vec[2,u16] { return zext[vec[2,u16]](value) }\n";
  semantic_error "integer-vector-zext-must-widen"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,u16]) vec[4,u8] { return zext[vec[4,u8]](value) }\n";
  semantic_error "integer-vector-trunc-must-narrow"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,u8]) vec[4,u16] { return trunc[vec[4,u16]](value) }\n";
  semantic_error "integer-vector-bitcast-opaque"
    "illegal cast for source and destination widths"
    "opaque Handle\nfn f(value i64) void { bitcast[Handle](value)\n return }\n";
  semantic_error "integer-vector-bitcast-void"
    "illegal cast for source and destination widths"
    "fn f(value i64) void { bitcast[void](value)\n return }\n";
  semantic_error "constant-bool-arithmetic"
    "arithmetic requires integer or vector operands"
    "const Invalid bool = true + true\nfn main() i32 { return 0 }\n";
  semantic_error "constant-bool-shift"
    "shift value must be an integer or integer vector"
    "const Invalid bool = true << false\nfn main() i32 { return 0 }\n";
  semantic_error "constant-dead-ternary-type" "type mismatch: expected i32, got bool"
    "const Invalid i32 = true ? 1 : false\nfn main() i32 { return Invalid }\n";
  ignore (llvm_of "const Safe i32 = true ? 7 : 1 / 0\nfn main() i32 { return Safe }\n");
  let vector_constants =
    llvm_of
      "const Bytes vec[4,u8] = splat(255)\n\
       const Signed vec[4,i8] = bitcast[vec[4,i8]](Bytes)\n\
       const WideUnsigned vec[4,u16] = zext[vec[4,u16]](Bytes)\n\
       const WideSigned vec[4,i16] = sext[vec[4,i16]](Signed)\n\
       const Narrow vec[4,u8] = trunc[vec[4,u8]](WideUnsigned)\n\
       const Flags vec[4,bool] = splat(true)\n\
       const BoolUnsigned vec[4,u8] = zext[vec[4,u8]](Flags)\n\
       const BoolSigned vec[4,i8] = sext[vec[4,i8]](Flags)\n\
       const LowBits vec[4,bool] = trunc[vec[4,bool]](Narrow)\n\
       const Added vec[4,u8] = Narrow + splat(1)\n\
       const Shifted vec[4,u8] = Added << 1\n\
       const Matches vec[4,bool] = Shifted == splat(0)\n\
       fn unsigned_lane() u16 { return WideUnsigned[2] }\n\
       fn signed_lane() i16 { return WideSigned[1] }\n\
       fn narrow_lane() u8 { return Narrow[3] }\n\
       fn bool_unsigned_lane() u8 { return BoolUnsigned[0] }\n\
       fn bool_signed_lane() i8 { return BoolSigned[0] }\n\
       fn low_bit_lane() bool { return LowBits[0] }\n\
       fn match_lane() bool { return Matches[0] }\n"
  in
  List.iter
    (fun marker ->
      if not (contains vector_constants marker) then
        failwith ("vector-constants: missing `" ^ marker ^ "`"))
    [
      "<i16 255, i16 255, i16 255, i16 255>";
      "<i16 65535, i16 65535, i16 65535, i16 65535>";
      "<i8 255, i8 255, i8 255, i8 255>";
      "<i1 1, i1 1, i1 1, i1 1>";
      "<i8 1, i8 1, i8 1, i8 1>";
    ];
  semantic_error "named-vector-constant-keeps-type"
    "type mismatch: expected vec[4, u16], got vec[4, u8]"
    "const Bytes vec[4,u8] = splat(1)\n\
     fn take(value vec[4,u16]) void { return }\n\
     fn main() void { take(Bytes)\n\
    \ return }\n";
  semantic_error "constant-vector-conversion-lanes"
    "illegal cast for source and destination widths"
    "const Bytes vec[4,u8] = splat(1)\n\
     const Wide vec[2,u16] = zext[vec[2,u16]](Bytes)\n\
     fn main() i32 { return 0 }\n";
  semantic_error "constant-vector-division-by-zero"
    "division by zero in constant expression"
    "const Values vec[4,u8] = splat(8)\n\
     const Zero vec[4,u8] = splat(0)\n\
     const Invalid vec[4,u8] = Values / Zero\n\
     fn main() i32 { return 0 }\n";
  semantic_error "constant-vector-index-assignment" "cannot modify constant"
    "const Values vec[4,u8] = splat(1)\nfn main() i32 { Values[0] = 2\n return 0 }\n";
  semantic_error "constant-vector-index-compound-assignment" "cannot modify constant"
    "const Values vec[4,u8] = splat(1)\nfn main() i32 { Values[0] += 2\n return 0 }\n";
  semantic_error "constant-vector-address" "cannot take the address"
    "const Values vec[4,u8] = splat(1)\nfn main() i32 { &Values\n return 0 }\n";
  ignore
    (llvm_of
       "const Safe vec[4,u8] = true ? splat(7) : splat(1) / splat(0)\n\
        fn main() u8 { return Safe[0] }\n");
  semantic_error "constant-vector-dead-ternary-type"
    "type mismatch: expected u8, got bool"
    "const Invalid vec[4,u8] = true ? splat(7) : splat(true)\n\
     fn main() i32 { return 0 }\n";
  semantic_error "runtime-logical-integer-left" "logical operands must be bool"
    "fn f() bool { return 0 && (1 / 0 == 0) }\n";
  let integer_vector_comparisons =
    llvm_of
      "fn signed(left vec[4,i32], right vec[4,i32]) bool {\n\
      \ eq vec[4,bool] = left == right\n\
      \ ne vec[4,bool] = left != right\n\
      \ lt vec[4,bool] = left < right\n\
      \ le vec[4,bool] = left <= right\n\
      \ gt vec[4,bool] = left > right\n\
      \ ge vec[4,bool] = left >= right\n\
      \ return eq[0] && ne[0] && lt[0] && le[0] && gt[0] && ge[0]\n\
      \ }\n\
      \ fn unsigned(left vec[4,u32], right vec[4,u32]) bool {\n\
      \ lt vec[4,bool] = left < right\n\
      \ le vec[4,bool] = left <= right\n\
      \ gt vec[4,bool] = left > right\n\
      \ ge vec[4,bool] = left >= right\n\
      \ return lt[0] && le[0] && gt[0] && ge[0]\n\
      \ }\n\
      \ fn booleans(left vec[8,bool], right vec[8,bool]) bool {\n\
      \ eq vec[8,bool] = left == right\n\
      \ ne vec[8,bool] = left != right\n\
      \ return eq[0] && ne[0]\n\
      \ }\n"
  in
  List.iter
    (fun predicate ->
      if not (contains integer_vector_comparisons ("icmp " ^ predicate)) then
        failwith ("integer-vector-comparison: missing `icmp " ^ predicate ^ "`"))
    [ "eq"; "ne"; "slt"; "sle"; "sgt"; "sge"; "ult"; "ule"; "ugt"; "uge" ];
  if
    (not (contains integer_vector_comparisons "store <4 x i1>"))
    || (not (contains integer_vector_comparisons "extractelement <4 x i1>"))
    || not (contains integer_vector_comparisons "icmp eq <8 x i1>")
  then failwith "integer-vector-comparison: result vector was not preserved";
  semantic_error "integer-vector-comparison-lanes" "same type"
    "fn f(left vec[4,i32], right vec[8,i32]) vec[4,bool] { return left == right }\n";
  semantic_error "integer-vector-comparison-elements" "same type"
    "fn f(left vec[4,i32], right vec[4,u32]) vec[4,bool] { return left == right }\n";
  semantic_error "integer-vector-comparison-scalar" "same type"
    "fn f(left vec[4,i32], right i32) vec[4,bool] { return left == right }\n";
  semantic_error "integer-vector-comparison-bool-order" "requires integer operands"
    "fn f(left vec[4,bool], right vec[4,bool]) vec[4,bool] { return left < right }\n";
  semantic_error "integer-vector-comparison-condition" "if condition must be bool"
    "fn f(left vec[4,i32], right vec[4,i32]) i32 {\n\
    \ if left == right { return 1 }\n\
    \ return 0\n\
     }\n";
  semantic_error "integer-vector-comparison-no-reduction"
    "logical operands must be bool"
    "fn f(left vec[4,i32], right vec[4,i32]) vec[4,bool] {\n\
    \ return (left == right) && (left != right)\n\
     }\n";
  let integer_vector_shift_counts =
    llvm_of
      "fn shifts(values vec[4,u32], signed_values vec[4,i32], narrow u8, equal u32, \
       wide u64) vec[4,u32] {\n\
      \ shl_narrow vec[4,u32] = values << narrow\n\
      \ shl_equal vec[4,u32] = values << equal\n\
      \ shl_wide vec[4,u32] = values << wide\n\
      \ lshr_narrow vec[4,u32] = values >> narrow\n\
      \ lshr_equal vec[4,u32] = values >> equal\n\
      \ lshr_wide vec[4,u32] = values >> wide\n\
      \ ashr_narrow vec[4,i32] = signed_values >> narrow\n\
      \ ashr_equal vec[4,i32] = signed_values >> equal\n\
      \ ashr_wide vec[4,i32] = signed_values >> wide\n\
      \ rotl_narrow vec[4,u32] = rotl(values, narrow)\n\
      \ rotl_equal vec[4,u32] = rotl(values, equal)\n\
      \ rotl_wide vec[4,u32] = rotl(values, wide)\n\
      \ rotr_narrow vec[4,u32] = rotr(values, narrow)\n\
      \ rotr_equal vec[4,u32] = rotr(values, equal)\n\
      \ rotr_wide vec[4,u32] = rotr(values, wide)\n\
      \ return rotr_wide\n\
      \ }\n"
  in
  List.iter
    (fun marker ->
      if not (contains integer_vector_shift_counts marker) then
        failwith ("integer-vector-shift-count: missing `" ^ marker ^ "`"))
    [
      "shl <4 x i32>";
      "lshr <4 x i32>";
      "ashr <4 x i32>";
      "@llvm.fshl.v4i32";
      "@llvm.fshr.v4i32";
    ];
  List.iter
    (fun (needle, expected) ->
      let actual = List.length (positions integer_vector_shift_counts needle) in
      if actual <> expected then
        failwith
          (Printf.sprintf "integer-vector-shift-count: expected %d `%s`, got %d"
             expected needle actual))
    [ ("zext i8", 5); ("trunc i64", 5); ("and i32", 15); ("shufflevector", 15) ];
  List.iter
    (fun operation ->
      semantic_error
        ("integer-vector-rotate-count-splat-" ^ operation)
        "vector shifts require a scalar integer count"
        (Printf.sprintf
           "fn f(values vec[4,u32]) vec[4,u32] { return %s(values, splat(1)) }\n"
           operation);
      semantic_error
        ("integer-vector-rotate-count-named-" ^ operation)
        "vector shifts require a scalar integer count"
        (Printf.sprintf
           "fn f(values vec[4,u32], count vec[4,u32]) vec[4,u32] { return %s(values, \
            count) }\n"
           operation))
    [ "rotl"; "rotr" ];
  List.iter
    (fun (operation, name) ->
      semantic_error
        ("integer-vector-shift-count-splat-" ^ name)
        "splat requires a vector type context"
        (Printf.sprintf
           "fn f(values vec[4,u32]) vec[4,u32] { return values %s splat(1) }\n"
           operation))
    [ ("<<", "shl"); (">>", "lshr") ];
  semantic_error "integer-vector-shift-count-noninteger"
    "shift count must be an integer"
    "fn f(values vec[4,u32], count bool) vec[4,u32] { return values << count }\n";
  semantic_error "shift-count-scalar-value-vector-count"
    "shift count must be a scalar integer for a scalar value"
    "fn f(x u32, count vec[4,u32]) u32 { return x << count }\n";
  semantic_error "shift-count-lane-mismatch"
    "shift count lanes must match the value lanes"
    "fn f(values vec[4,u32], count vec[2,u32]) vec[4,u32] { return values << count }\n";
  semantic_error "shift-compound-lane-mismatch"
    "shift count lanes must match the value lanes"
    "fn f(values vec[4,u32], count vec[2,u32]) vec[4,u32] { values <<= count\n\
    \ return values }\n";
  semantic_error "shift-compound-bool-destination"
    "shift value must be an integer or integer vector"
    "fn f(values vec[4,bool], count u32) vec[4,bool] { values <<= count\n\
    \ return values }\n";
  semantic_error "shift-compound-scalar-value-vector-count"
    "shift count must be a scalar integer for a scalar value"
    "fn f(x u32, count vec[4,u32]) u32 { x <<= count\n return x }\n";
  semantic_error "shift-no-splat-lift" "type mismatch: expected vec[4, u32], got i32"
    "fn f(n u32) vec[4,u32] { return 1 << n }\n";
  semantic_error "len-returns-usize" "type mismatch: expected u64, got usize"
    "fn size(values arr[3,u8]) u64 { return len(values) }\n";
  semantic_error "sizeof-returns-usize" "type mismatch: expected u64, got usize"
    "fn size() u64 { return sizeof[u8] }\n";
  ignore
    (lower_of
       "struct Measure { left u8 right u64 }\n\
        const Size usize = sizeof[Measure]\n\
        const Alignment usize = alignof[Measure]\n\
        const Offset usize = offsetof[Measure,right]\n\
        fn size() usize { return Size + Alignment + Offset }\n");
  semantic_error "const-sizeof-returns-usize" "constant initializer type mismatch"
    "const Size u64 = sizeof[u8]\nfn size() u64 { return Size }\n";
  let usize_specialization =
    llvm_of
      "fn id[N const usize](value usize) usize { return value + N }\n\
       fn main() usize { return id[3](4) }\n"
  in
  if not (contains usize_specialization "N=usize:3\"") then
    failwith "usize-specialization: specialization key lost usize identity";
  let target_width_type_hir =
    expect_ok
      (Parser.parse
         (source
            "fn f[T](v T) T { return v }\n\
             fn main() usize { a usize = f[usize](1)\n\
            \ b u64 = f[u64](2)\n\
            \ return a }\n"))
    |> Sema.check |> expect_ok
  in
  let target_width_specs =
    List.filter
      (fun (func : Hir.func) -> contains func.name "$spec$")
      target_width_type_hir.Hir.funcs
  in
  if List.length target_width_specs <> 2 then
    failwith "target-width-instantiation: usize/u64 instantiations were merged";
  if
    not
      (List.exists
         (fun (func : Hir.func) -> String.ends_with ~suffix:"usize" func.name)
         target_width_specs)
  then failwith "target-width-instantiation: specialization key lost usize identity";
  if
    not
      (List.exists
         (fun (func : Hir.func) -> String.ends_with ~suffix:"u64" func.name)
         target_width_specs)
  then failwith "target-width-instantiation: specialization key lost u64 identity";
  ignore (Lower.lower target_width_type_hir |> expect_ok);
  let signed_target_width_hir =
    expect_ok
      (Parser.parse
         (source
            "fn f[T](v T) T { return v }\n\
             fn main() isize { a isize = f[isize](1)\n\
            \ b i64 = f[i64](2)\n\
            \ return a }\n"))
    |> Sema.check |> expect_ok
  in
  let signed_target_width_specs =
    List.filter
      (fun (func : Hir.func) -> contains func.name "$spec$")
      signed_target_width_hir.Hir.funcs
  in
  if List.length signed_target_width_specs <> 2 then
    failwith "target-width-instantiation: isize/i64 instantiations were merged";
  if
    not
      (List.exists
         (fun (func : Hir.func) -> String.ends_with ~suffix:"isize" func.name)
         signed_target_width_specs)
  then failwith "target-width-instantiation: specialization key lost isize identity";
  if
    not
      (List.exists
         (fun (func : Hir.func) -> String.ends_with ~suffix:"i64" func.name)
         signed_target_width_specs)
  then failwith "target-width-instantiation: specialization key lost i64 identity";
  ignore (Lower.lower signed_target_width_hir |> expect_ok);
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
  semantic_error "place-init-pointer-field-write" "use of uninitialized local `s`"
    "struct S { p ptr[i64] }\nfn main() i64 { s S\n s.p[0] = 1\n return 0 }\n";
  semantic_error "place-init-pointer-field-read" "use of uninitialized local `s`"
    "struct S { p ptr[i64] }\nfn main() i64 { s S\n x i64 = s.p[0]\n return x }\n";
  semantic_error "place-init-pointer-field-address" "use of uninitialized local `s`"
    "struct S { p ptr[i64] }\n\
     fn take(p ptr[i64]) void { return }\n\
     fn main() i64 { s S\n\
    \ take(&s.p[0])\n\
    \ return 0 }\n";
  semantic_error "place-init-pointer-array-element" "use of uninitialized local `a`"
    "fn main() i64 { a arr[2,ptr[i64]]\n a[0][0] = 1\n return 0 }\n";
  ignore
    (lower_of
       "struct S { p ptr[i64] }\n\
        fn main() i64 { x i64\n\
       \ s S\n\
       \ s.p = &x\n\
       \ s.p[0] = 1\n\
       \ return x }\n");
  semantic_error "place-init-whole-vector" "use of uninitialized local `v`"
    "fn f() i64 { v vec[2,i64]\n x vec[2,i64] = v\n return 0 }\n";
  ignore (lower_of "fn f() i64 { v vec[2,i64]\n v[0] = 1\n return v[0] }\n");
  semantic_error "place-init-vector-other-lane" "use of uninitialized local `v`"
    "fn f() i64 { v vec[2,i64]\n v[0] = 1\n return v[1] }\n";
  ignore
    (lower_of
       "fn f() i64 { v vec[2,i64]\n\
       \ v[0] = 1\n\
       \ v[1] = 2\n\
       \ x vec[2,i64] = v\n\
       \ return x[0] }\n");
  semantic_error "place-init-static-large-index" "array index is out of bounds"
    "const N u64 = 9223372036854775808\n\
     fn f() i64 { a arr[2,i64]\n\
    \ x i64 = a[N]\n\
    \ return x }\n";
  semantic_error "place-init-static-narrow-negative-index"
    "array index is out of bounds"
    "const N i8 = -1\nfn f() i64 { a arr[300,i64]\nreturn a[N] }\n";
  semantic_error "vector-lane-address-rejected" "cannot take address of a vector lane"
    "fn take(p ptr[i64]) void { return }\n\
     fn f() i64 { v vec[2,i64] = splat(1)\n\
     take(&v[0])\n\
     return 0 }\n";
  ignore
    (lower_of
       "fn take(p ptr[vec[2,i64]]) void { return }\n\
        fn f() i64 { v vec[2,i64] = splat(1)\n\
       \ take(&v)\n\
       \ return 0 }\n");
  let unsigned_narrow_index =
    llvm_of
      "const N u8 = 255\n\
       fn f() i64 { a arr[256,i64]\n\
      \ a[N] = 7\n\
      \ v vec[256, u8] = splat(1)\n\
      \ v[N] = 8\n\
      \ return a[N] + zext[i64](v[N]) }\n"
  in
  if not (contains unsigned_narrow_index "zext i8 255 to i64") then
    failwith "aggregate-index-u8: narrow unsigned index was not zero-extended";
  let index_evaluation_order =
    llvm_of
      "fn base() ptr[i64] { return null }\n\
       fn index() i64 { return 0 }\n\
       fn f() i64 { base()[index()] = 1\n\
      \ return 0 }\n"
  in
  let base_calls = positions index_evaluation_order "call ptr @base" in
  let index_calls = positions index_evaluation_order "call i64 @index" in
  (match (base_calls, index_calls) with
  | base :: _, index :: _ when base < index -> ()
  | _ -> failwith "aggregate-index-order: index was evaluated before its base");
  let vector_assign_alias =
    llvm_of
      "fn mutate(p ptr[vec[2,i64]]) i64 { p.* = splat(9)\n\
      \ return 7 }\n\
       fn f() i64 { v vec[2,i64] = splat(1)\n\
      \ v[0] = mutate(&v)\n\
      \ return v[1] }\n"
  in
  let assign_call = positions vector_assign_alias "call i64 @mutate" in
  let assign_load = positions vector_assign_alias "load <2 x i64>" in
  (match (assign_call, assign_load) with
  | call :: _, load :: _ when call < load -> ()
  | _ -> failwith "vector-lane-assignment: rhs was evaluated after stale vector load");
  let vector_compound_alias =
    llvm_of
      "fn mutate(p ptr[vec[2,i64]]) i64 { p.* = splat(9)\n\
      \ return 7 }\n\
       fn f() i64 { v vec[2,i64] = splat(1)\n\
      \ v[0] += mutate(&v)\n\
      \ return v[1] }\n"
  in
  let compound_call = positions vector_compound_alias "call i64 @mutate" in
  let compound_loads = positions vector_compound_alias "load <2 x i64>" in
  (match (compound_call, compound_loads) with
  | call :: _, first :: second :: _ when first < call && call < second -> ()
  | _ -> failwith "vector-lane-compound: rhs was not between vector loads");
  semantic_error "place-init-short-circuit-escape" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn f() i64 { s S\n\
    \ true || (&s != null)\n\
    \ t S = s\n\
    \ return 0 }\n";
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn take(p ptr[S]) void { return }\n\
        fn f() i64 { s S\n\
       \ take(&s)\n\
       \ true || false\n\
       \ t S = s\n\
       \ return 0 }\n");
  semantic_error "place-init-ternary-escape" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn f(p bool) i64 { s S\n\
    \ q ptr[S] = p ? &s : null\n\
     t S = s\n\
    \ return 0 }\n";
  semantic_error "place-init-ternary-cross-arm" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn choose(p ptr[S], x S) S { return x }\n\
     fn f(p bool) i64 { s S\n\
    \ t S = p ? choose(&s, s) : s\n\
    \ return 0 }\n";
  semantic_error "place-init-binding-identity" "use of uninitialized local `x`"
    "fn main() i64 { { x i64 = 1 }\n { x i64\n y i64 = x\n }\n return 0 }\n";
  semantic_error "aggregate-whole-read" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\nfn f() i64 { s S\n t S = s\n return 0 }\n";
  semantic_error "aggregate-partial-field" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\nfn f() i64 { s S\n s.x = 1\n return s.y }\n";
  semantic_error "aggregate-partial-whole" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\nfn f() i64 { s S\n s.x = 1\n t S = s\n return 0 }\n";
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f() i64 { s S\n\
       \ s.x = 1\n\
       \ s.y = 2\n\
       \ t S = s\n\
       \ return s.x }\n");
  semantic_error "aggregate-nested-field" "use of uninitialized local `o`"
    "struct I { x i64 y i64 }\n\
     struct O { i I z i64 }\n\
     fn f() i64 { o O\n\
    \ o.i.x = 1\n\
    \ return o.i.y }\n";
  semantic_error "aggregate-array-whole" "use of uninitialized local `a`"
    "fn f() i64 { a arr[2,i64]\n a[0] = 1\n t arr[2,i64] = a\n return 0 }\n";
  ignore
    (lower_of
       "fn f() i64 { a arr[2,i64]\n\
       \ a[0] = 1\n\
       \ x i64 = a[0]\n\
       \ a[1] = 2\n\
       \ t arr[2,i64] = a\n\
       \ return x }\n");
  let array_field_copy =
    llvm_of
      "struct S { a arr[2,i64] }\n\
       fn f() i64 { s S\n\
      \ s.a[0] = 1\n\
      \ s.a[1] = 2\n\
      \ t arr[2,i64] = s.a\n\
      \ return t[0] }\n"
  in
  if not (contains array_field_copy "load [2 x i64], ptr") then
    failwith "aggregate-array-field-copy: array field was not loaded as a value";
  let nested_array_copy =
    llvm_of
      "fn f() i64 { a arr[2,arr[2,i64]]\n\
      \ a[0][0] = 1\n\
      \ a[0][1] = 2\n\
      \ t arr[2,i64] = a[0]\n\
      \ return t[0] }\n"
  in
  if not (contains nested_array_copy "load [2 x i64], ptr") then
    failwith "aggregate-nested-array-copy: nested array was not loaded as a value";
  semantic_error "aggregate-whole-array-uninitialized" "use of uninitialized local `a`"
    "fn f() i64 { a arr[2,i64]\n return a[0] }\n";
  ignore (lower_of "struct E { }\nfn f() i64 { e E\n t E = e\n return 0 }\n");
  ignore (lower_of "fn f() i64 { a arr[0,i64]\n t arr[0,i64] = a\n return 0 }\n");
  ignore
    (lower_of
       "struct E { }\n\
        struct S { e E x i64 }\n\
        fn f() i64 { s S\n\
       \ s.x = 1\n\
       \ t E = s.e\n\
       \ return s.x }\n");
  ignore
    (lower_of
       "struct E { }\nstruct S { e E }\nfn f() i64 { s S\n t S = s\n return 0 }\n");
  ignore
    (lower_of
       "fn f() i64 { a arr[2,i64]\n\
       \ a[0] = 1\n\
       \ a[1] = 2\n\
       \ t arr[2,i64] = a\n\
       \ return t[0] }\n");
  semantic_error "aggregate-direct-return" "use of uninitialized local `s`"
    "struct S { x i64 }\nfn f() S { s S\nreturn s }\n";
  semantic_error "aggregate-branch-no-else" "use of uninitialized local `s`"
    "struct S { x i64 }\nfn f(p bool) i64 { s S\nif p { s.x = 1 }\nreturn s.x }\n";
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f(p bool) i64 { s S\n\
       \ if p { s.x = 1\n\
       \ s.y = 2 } else { s.x = 3\n\
       \ s.y = 4 }\n\
       \ return s.y }\n");
  ignore
    (lower_of
       "struct S { x i64 }\n\
        fn f(n i64) i64 { s S\n\
       \ switch n {\n\
       \ case 0: { s.x = 1 }\n\
       \ default: { s.x = 2 }\n\
       \ }\n\
       \ return s.x }\n");
  semantic_error "aggregate-switch-partial-merge" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn f(n i64) i64 { s S\n\
    \ switch n {\n\
    \ case 0: { s.x = 1 }\n\
    \ default: { s.y = 2 }\n\
    \ }\n\
    \ return s.x }\n";
  ignore
    (lower_of
       "fn f(i i64) i64 { a arr[2,arr[2,i64]]\n\
       \ a[0][0] = 1\n\
       \ a[0][1] = 2\n\
       \ return a[0][i] }\n");
  semantic_error "aggregate-nested-dynamic-prefix" "use of uninitialized local `a`"
    "fn f(i i64) i64 { a arr[2,arr[2,i64]]\na[0][0] = 1\nreturn a[0][i] }\n";
  ignore
    (lower_of "fn f(i i64) i64 { v vec[2,i64]\n v[0] = 1\n v[1] = 2\n return v[i] }\n");
  semantic_error "aggregate-dynamic-index" "use of uninitialized local `a`"
    "fn f() i64 { a arr[2,i64]\n i i64 = 0\n return a[i] }\n";
  semantic_error "aggregate-dynamic-write-read" "use of uninitialized local `a`"
    "fn f(i i64) i64 { a arr[2,i64]\na[i] = 1\nreturn a[i] }\n";
  ignore
    (lower_of "fn f(i i64) i64 { a arr[2,i64]\n a[0] = 1\n a[1] = 2\n return a[i] }\n");
  ignore
    (lower_of
       "struct S { a arr[2,i64] z i64 }\n\
        fn f(i i64) i64 { s S\n\
       \ s.a[0] = 1\n\
       \ s.a[1] = 2\n\
       \ return s.a[i] }\n");
  semantic_error "aggregate-dynamic-pointer-element" "use of uninitialized local `a`"
    "fn f(i i64) i64 { x i64\na arr[2,ptr[i64]]\na[0] = &x\na[i][0] = 1\nreturn x }\n";
  ignore
    (llvm_of
       "fn take(p ptr[i64]) void { return }\n\
        fn f() i64 { x i64\n\
       \ take(&x)\n\
       \ return x }\n");
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn take(p ptr[i64]) void { return }\n\
        fn f() i64 { s S\n\
       \ take(&s.x)\n\
       \ t S = s\n\
       \ return 0 }\n");
  semantic_error "aggregate-branch-partial" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn f(p bool) i64 { s S\n\
     if p { s.x = 1 } else { s.y = 2 }\n\
    \ return s.x }\n";
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f(p bool) i64 { s S\n\
        if p { s.x = 1 } else { return 0 }\n\
       \ return s.x }\n");
  semantic_error "aggregate-loop-only" "use of uninitialized local `s`"
    "struct S { x i64 }\nfn f(p bool) i64 { s S\n while p { s.x = 1 }\n return s.x }\n";
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn take(p ptr[S]) void { return }\n\
        fn f(p bool) i64 { s S\n\
       \ if p { take(&s) } else { s.x = 1 }\n\
        return s.x }\n");
  semantic_error "aggregate-branch-raw-missing-field" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn take(p ptr[S]) void { return }\n\
     fn f(p bool) i64 { s S\n\
    \ if p { take(&s) } else { s.x = 1 }\n\
     return s.y }\n";
  semantic_error "aggregate-branch-raw-whole" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn take(p ptr[S]) void { return }\n\
     fn f(p bool) i64 { s S\n\
    \ if p { take(&s) } else { s.x = 1 }\n\
     t S = s\n\
    \ return 0 }\n";
  semantic_error "aggregate-compound-read" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\nfn f() i64 { s S\n s.x += 1\n return 0 }\n";
  semantic_error "place-init-after-return" "use of uninitialized local `x`"
    "fn f(p bool) i64 { x i64\n if p { return 0\n x = 1 }\n return x }\n";
  semantic_error "place-init-after-break" "use of uninitialized local `x`"
    "fn f() i64 { x i64\n while true { break\n x = 1 }\n return x }\n";
  semantic_error "place-init-after-continue" "use of uninitialized local `x`"
    "fn f() i64 { x i64\n while true { continue\n x = 1 }\n return x }\n";
  let uninitialized_aggregate_llvm =
    llvm_of "struct S { x i64 y i64 }\nfn f() i64 { s S\n s.x = 1\n return s.x }\n"
  in
  if
    contains uninitialized_aggregate_llvm "memset"
    || contains uninitialized_aggregate_llvm "store %struct.S zeroinitializer"
  then failwith "place-init: aggregate declaration emitted implicit initialization";
  semantic_error "defer-does-not-initialize" "use of uninitialized local `x`"
    "fn f() i64 { x i64\n defer { x = 1 }\n return x }\n";
  ignore
    (lower_of "fn f() i64 { x i64\n defer { observed i64 = x }\n x = 1\n return 0 }\n");
  ignore (lower_of "fn f() void { x i64\n defer { observed i64 = x }\n x = 1 }\n");
  ignore
    (lower_of
       "fn f() i64 { x i64\n\
       \ defer { observed i64 = x }\n\
       \ { defer { x = 1 } }\n\
       \ return 0 }\n");
  semantic_error "defer-normal-exit-uninitialized" "use of uninitialized local `x`"
    "fn f() void { x i64\n defer { observed i64 = x } }\n";
  semantic_error "defer-return-exit-uninitialized" "use of uninitialized local `x`"
    "fn f() i64 { x i64\n defer { observed i64 = x }\n return 0 }\n";
  ignore
    (lower_of
       "fn f() i64 { x i64\n defer { observed i64 = x }\n while true { }\n return 0 }\n");
  ignore
    (lower_of
       "fn f() i64 { x i64\n\
       \ defer { observed i64 = x }\n\
       \ defer { x = 1 }\n\
       \ return 0 }\n");
  semantic_error "defer-lifo-read-before-write" "use of uninitialized local `x`"
    "fn f() i64 { x i64\n defer { x = 1 }\n defer { observed i64 = x }\n return 0 }\n";
  ignore
    (lower_of
       "fn f() i64 { x i64\n defer { observed i64 = x }\n defer { while true { } } }\n");
  semantic_error "defer-read-before-divergence" "use of uninitialized local `x`"
    "fn f() i64 { x i64\n defer { while true { } }\n defer { observed i64 = x } }\n";
  ignore
    (lower_of
       "fn f() void { while true { x i64\n\
       \ defer { observed i64 = x }\n\
       \ x = 1\n\
       \ break } }\n");
  semantic_error "defer-break-uninitialized" "use of uninitialized local `x`"
    "fn f() void { while true { x i64\n defer { observed i64 = x }\n break } }\n";
  ignore
    (lower_of
       "fn f() void { index i64 = 0\n\
       \ while index < 1 { x i64\n\
       \ defer { observed i64 = x }\n\
       \ x = 1\n\
       \ index += 1\n\
       \ continue } }\n");
  semantic_error "defer-continue-uninitialized" "use of uninitialized local `x`"
    "fn f() void { while true { x i64\n defer { observed i64 = x }\n continue } }\n";
  ignore
    (lower_of
       "fn take(value i64) void { return }\n\
       \ fn f() void { for value i64; true; take(value) {\n\
       \ defer { value = 1 }\n\
       \ continue\n\
       \ } }\n");
  ignore
    (lower_of
       "fn take(value i64) void { return }\n\
       \ fn f() void { for value i64; true; take(value) {\n\
       \ defer { value = 1 }\n\
       \ } }\n");
  semantic_error "for-step-uninitialized" "use of uninitialized local `value`"
    "fn take(value i64) void { return }\n\
    \ fn f() void { for value i64; true; take(value) { continue } }\n";
  semantic_error "for-step-path-merge" "use of uninitialized local `value`"
    "fn take(value i64) void { return }\n\
    \ fn f(condition bool) void { for value i64; true; take(value) {\n\
    \ if condition { value = 1 } else { continue }\n\
    \ } }\n";
  ignore
    (lower_of
       "fn f() i64 { value i64\n while true { value = 1\n break }\n return value }\n");
  ignore
    (lower_of
       "fn f() i64 { value i64\n\
       \ while true { defer { value = 1 }\n\
       \ break }\n\
       \ return value }\n");
  semantic_error "conditional-loop-initialization" "use of uninitialized local `value`"
    "fn f(condition bool) i64 { value i64\n\
    \ while condition { value = 1\n\
    \ break }\n\
    \ return value }\n";
  ignore
    (lower_of
       "fn f(choice i64) i64 { value i64\n\
       \ while true { switch choice {\n\
       \ case 0: { value = 1\n\
       \ break }\n\
       \ default: { value = 2\n\
       \ break }\n\
       \ } }\n\
       \ return value }\n");
  ignore
    (lower_of
       "fn f() i64 { value i64\n\
       \ while true { while true { break }\n\
       \ value = 1\n\
       \ break }\n\
       \ return value }\n");
  ignore
    (lower_of
       "fn f(condition bool) i64 { value i64\n\
       \ while true { if condition { value = 1\n\
       \ break } else { continue\n\
       \ switch 0 { default: { break } } } }\n\
       \ return value }\n");
  ignore
    (lower_of
       "fn f(choice i64) i64 { value i64\n\
       \ while true { switch choice {\n\
       \ case 0: { defer { value = 1 }\n\
       \ break }\n\
       \ default: { defer { value = 2 }\n\
       \ break }\n\
       \ } }\n\
       \ return value }\n");
  ignore
    (lower_of
       "fn f() i64 { value i64\n\
       \ while true { while true { switch 0 { default: { break } } }\n\
       \ value = 1\n\
       \ break }\n\
       \ return value }\n");
  ignore
    (lower_of
       "fn f() void { while true { continue\n\
       \ { value i64\n\
       \ defer { observed i64 = value }\n\
       \ continue } } }\n");
  semantic_error "switch-loop-exit-initialization" "use of uninitialized local `value`"
    "fn f(choice i64) i64 { value i64\n\
    \ while true { switch choice {\n\
    \ case 0: { value = 1\n\
    \ break }\n\
    \ default: { break }\n\
    \ } }\n\
    \ return value }\n";
  semantic_error "nested-defer" "nested defer is not allowed"
    "fn f() void { defer { defer { } } }\n";
  semantic_error "defer-return" "return is not allowed inside defer"
    "fn f() void { defer { return } }\n";
  semantic_error "defer-break" "break is not allowed inside defer"
    "fn f() void { while true { defer { break } break } }\n";
  semantic_error "defer-continue" "continue is not allowed inside defer"
    "fn f() void { while true { defer { continue } break } }\n";
  ignore (lower_of "fn f() u32 { x arr[4,u32] = raw\n x[0] = 1\n return x[3] }\n");
  ignore (lower_of "fn f() i64 { p ptr[i64] = raw\n p[0] = 1\n return p[0] }\n");
  ignore
    (lower_of "fn f() u32 { x arr[4,u32] = raw\n t arr[4,u32] = x\n return t[0] }\n");
  ignore
    (lower_of
       "fn f() u32 { x arr[4,u32] = raw\n\
       \ for i u32 = 0; i < 4; i += 1 { x[i] = i }\n\
       \ return x[0] }\n");
  ignore
    (lower_of
       "fn f(p bool) u32 { x arr[4,u32] = raw\n if p { x[0] = 1 }\n return x[1] }\n");
  ignore (lower_of "fn f() u64 { x u64 = raw\n defer { x = 1 }\n return x }\n");
  ignore
    (lower_of
       "struct S { x i64 y i64 }\nfn f() i64 { s S = raw\n s.x = 1\n return s.y }\n");
  ignore (lower_of "fn f() u32 { v vec[4,u32] = raw\n v[0] = 1\n return v[3] }\n");
  ignore (lower_of "fn f() i64 { x i64 = raw\n return x }\n");
  ignore (lower_of "fn f() i64 { x i64 = raw\n x = 5\n return x }\n");
  let raw_no_zero =
    llvm_of "fn f() u32 { x arr[4,u32] = raw\n x[0] = 1\n return x[3] }\n"
  in
  if contains raw_no_zero "memset" || contains raw_no_zero "zeroinitializer" then
    failwith "raw declaration emitted implicit initialization";
  parse_error_message "raw-not-a-value-return"
    "`raw` is a declaration marker, not a value" "fn f() i64 { return raw }\n";
  parse_error_message "raw-not-a-value-call"
    "`raw` is a declaration marker, not a value"
    "fn g(x i64) i64 { return x }\nfn f() i64 { return g(raw) }\n";
  parse_error_message "raw-not-a-value-assign"
    "`raw` is a declaration marker, not a value"
    "fn f() i64 { x i64 = 1\n x = raw\n return x }\n";
  parse_error "raw-init-trailing" "fn f() i64 { x i64 = raw + 1\n return x }\n";
  semantic_error "lexical-scope-same-block" "duplicate local `value`"
    "fn f() i64 { value i64 = 1\n value i64 = 2\n return value }\n";
  semantic_error "lexical-scope-parameter-body" "duplicate local `value`"
    "fn f(value i64) i64 { value i64 = 2\n return value }\n";
  semantic_error "lexical-scope-type-parameter-body" "duplicate local `T`"
    "fn f[T](value T) T { T i32 = 2\n\
    \ return value }\n\
     fn main(value i32) i32 { return f[i32](value) }\n";
  semantic_error "lexical-scope-const-parameter-body" "duplicate local `N`"
    "fn f[N const i32]() i32 { N i32 = 2\n\
    \ return N }\n\
     fn main() i32 { return f[1]() }\n";
  ignore
    (lower_of
       "fn f() i64 { value i64 = 1\n { value i64 = 2\n value += 1 }\n return value }\n");
  ignore
    (lower_of "fn f(value i64) i64 { { value i64 = 2\n value += 1 }\n return value }\n");
  semantic_error "lexical-scope-independent-initialization"
    "use of uninitialized local `value`"
    "fn f() i64 { value i64\n { value i64 = 2 }\n return value }\n";
  ignore
    (lower_of
       "fn f() i64 { total i64 = 0\n\
       \ { value i64 = 2\n\
       \ total += value }\n\
       \ { value i64 = 3\n\
       \ total += value }\n\
       \ return total }\n");
  ignore
    (lower_of
       "fn f(flag bool) i64 { total i64 = 0\n\
       \ if flag { value i64 = 2\n\
       \ total += value }\n\
       \ else { value i64 = 3\n\
       \ total += value }\n\
       \ return total }\n");
  ignore
    (lower_of
       "fn f(flag bool) i64 { value i64 = 1\n\
       \ if flag { value i64 = 2\n\
       \ value += 1 }\n\
       \ return value }\n");
  ignore
    (lower_of
       "fn f() i64 { total i64 = 0\n\
       \ for index i64 = 0; index < 3; index += 1 {\n\
       \   total += index\n\
       \ }\n\
       \ return total }\n");
  semantic_error "lexical-scope-for-escape" "unknown name `index`"
    "fn f() i64 { for index i64 = 0; index < 1; index += 1 { }\n return index }\n";
  ignore
    (lower_of
       "fn delayed(out ptr[i64], value i64) void {\n\
       \ defer { out.* += value }\n\
       \ { value i64 = 100\n\
       \ out.* += value - 100 }\n\
       \ }\n");
  let lexical_const_shadow =
    llvm_of "const value i64 = 1\nfn f() i64 { value i64 = 2\n return value }\n"
  in
  if
    (not (contains lexical_const_shadow "store i64 2"))
    || not (contains lexical_const_shadow "load i64, ptr")
  then failwith "lexical-scope-const-shadow: local did not win lookup";
  let lexical_const_array_shadow =
    llvm_of
      "const values arr[1,i64] = {1}\n fn f() i64 { values i64 = 2\n return values }\n"
  in
  if
    (not (contains lexical_const_array_shadow "store i64 2"))
    || not (contains lexical_const_array_shadow "load i64, ptr")
  then failwith "lexical-scope-const-array-shadow: local did not win lookup";
  let lexical_initializer_shadow =
    llvm_of "const value i64 = 1\nfn f() i64 { value i64 = value + 1\n return value }\n"
  in
  if not (contains lexical_initializer_shadow "add i64 1, 1") then
    failwith "lexical-scope-initializer-shadow: outer binding was not visible";
  semantic_error "lexical-scope-function-call-shadow" "value, not a function"
    "fn target() i32 { return 1 }\nfn use() i32 { target i32 = 2\n return target() }\n";
  semantic_error "lexical-scope-generic-call-shadow" "value, not a function"
    "fn target[N const i32]() i32 { return N }\n\
     fn use() i32 { target i32 = 2\n\
    \ return target[1]() }\n";
  semantic_error "lexical-scope-type-shadow" "value, not a type"
    "struct Item { value i32 }\nfn use(Item i32) i32 { local Item\n return 0 }\n";
  semantic_error "lexical-scope-aggregate-length-shadow" "not a compile-time constant"
    "const Count usize = 2\n\
     fn use(Count usize) i32 { local arr[Count,i32]\n\
    \ return 0 }\n";
  semantic_error "static-index-parameter-constant-shadow"
    "use of uninitialized local `values`"
    "const Index i32 = 0\n\
     fn read(Index i32) i32 { values arr[2,i32]\n\
    \ values[0] = 7\n\
    \ return values[Index] }\n\
     fn main() i32 { return read(1) }\n";
  semantic_error "static-index-nested-parameter-constant-shadow"
    "use of uninitialized local `values`"
    "const Index i32 = 0\n\
     fn read(Index i32) i32 { values arr[2,i32]\n\
    \ values[0] = 7\n\
    \ return values[Index + 0] }\n\
     fn main() i32 { return read(1) }\n";
  let static_global_index =
    llvm_of
      "const Index i32 = 1\n\
       fn read() i32 { values arr[2,i32]\n\
      \ values[Index] = 9\n\
      \ return values[Index] }\n"
  in
  if not (contains static_global_index "ret i32 %v") then
    failwith "static-global-index: constant index lost its initialization proof";
  let dynamic_shadowed_index =
    llvm_of
      "const Index i32 = 0\n\
       fn read(Index i32) i32 { values arr[2,i32]\n\
      \ values[0] = 7\n\
      \ values[1] = 11\n\
      \ return values[Index] }\n"
  in
  if not (contains dynamic_shadowed_index "sext i32 %") then
    failwith "dynamic-shadowed-index: parameter was replaced by the global constant";
  ignore
    (lower_of
       "const Index i32 = 1\n\
        const Hidden i32 = 0\n\
        fn read(Hidden i32) i32 { values arr[2,i32]\n\
       \ values[Index] = 9\n\
       \ return values[true ? Index : Hidden] }\n");
  semantic_error "lexical-scope-declaration-before-use" "unknown assignment target"
    "fn f() i64 { value = 1\n value i64 = 2\n return value }\n";
  semantic_error "void-value-return" "void function cannot return a value"
    "extern \"C\" { fn sink() void }\nfn f() void { return sink() }\n";
  semantic_error "nonvoid-fallthrough" "may reach the end without returning"
    "fn f() i64 { }\n";
  ignore (lower_of "fn spin() i32 { while true { } }\n");
  ignore (lower_of "fn spin() i32 { for ; ; { continue } }\n");
  ignore (lower_of "fn spin() i32 { while true { while true { break } } }\n");
  ignore (lower_of "fn finish() i32 { defer { while true { } } }\n");
  ignore
    (lower_of
       "fn finish(choice bool) i32 { if choice { return 1 }\n\
       \ defer { while true { } } }\n");
  ignore
    (lower_of "fn spin() i32 { while true { defer { while true { } }\n break } }\n");
  semantic_error "conditional-loop-fallthrough" "may reach the end without returning"
    "fn spin(condition bool) i32 { while condition { } }\n";
  semantic_error "unconditional-loop-reachable-break"
    "may reach the end without returning"
    "fn spin(condition bool) i32 { while true { if condition { break } } }\n";
  semantic_error "switch-break-exits-loop" "may reach the end without returning"
    "fn spin(value i32) i32 { while true { switch value {\n\
     case 0:\n\
     break\n\
     default:\n\
     continue\n\
     } } }\n";
  semantic_error "conditional-defer-divergence" "may reach the end without returning"
    "fn finish(choice bool) i32 { defer { if choice { while true { } } } }\n";
  semantic_error "unreached-defer-does-not-consume-break"
    "may reach the end without returning"
    "fn spin() i32 { while true { break\n defer { while true { } } } }\n";
  semantic_error "extern-c-struct-parameter" "cannot use `S` by value; use a pointer"
    "struct S { x i64 }\nextern \"C\" { fn take(value S) void }\n";
  semantic_error "extern-c-struct-return"
    "cannot return `S` by value; use an output pointer"
    "struct S { x i64 }\nextern \"C\" { fn make() S }\n";
  semantic_error "extern-c-vector-parameter"
    "cannot use `vec[4, i32]` by value; use a pointer"
    "extern \"C\" { fn take(value vec[4,i32]) void }\n";
  semantic_error "extern-c-array-return"
    "cannot return `arr[2, i64]` by value; use an output pointer"
    "extern \"C\" { fn make() arr[2,i64] }\n";
  semantic_error "extern-c-opaque-parameter"
    "opaque type `Handle` may only be used behind a pointer"
    "opaque Handle\nextern \"C\" { fn take(value Handle) void }\n";
  semantic_error "extern-c-definition-fallthrough" "may reach the end without returning"
    "extern \"C\" { fn value() i64 { } }\n";
  semantic_error "extern-c-definition-struct-parameter"
    "cannot use `S` by value; use a pointer"
    "struct S { x i64 }\nextern \"C\" { fn take(value S) void { return } }\n";
  parse_error_message "extern-c-variadic-definition"
    "extern \"C\" function definitions cannot be variadic"
    "extern \"C\" { fn take(marker u64, ...) void { return } }\n";
  semantic_error "extern-c-const-parameter"
    "extern \"C\" functions cannot have const parameters"
    "extern \"C\" { fn value[N const usize]() usize }\n";
  let c_scalar_abi =
    llvm_of
      "extern \"C\" {\n\
       fn b(x bool) bool\n\
       fn u8_value(x u8) u8\n\
       fn i8_value(x i8) i8\n\
       fn u16_value(x u16) u16\n\
       fn i16_value(x i16) i16\n\
       }\n\
       fn main() i32 {\n\
       x bool = b(true)\n\
       a u8 = u8_value(1)\n\
       c i8 = i8_value(-1)\n\
       d u16 = u16_value(2)\n\
       e i16 = i16_value(-2)\n\
       return zext[i32](x) + zext[i32](a) + sext[i32](c) + zext[i32](d) + sext[i32](e)\n\
       }\n"
  in
  List.iter
    (fun expected ->
      if not (contains c_scalar_abi expected) then
        failwith ("extern-c-scalar-extension: missing `" ^ expected ^ "`"))
    [
      "declare zeroext i1 @b(i1 zeroext)";
      "declare zeroext i8 @u8_value(i8 zeroext)";
      "declare signext i8 @i8_value(i8 signext)";
      "declare zeroext i16 @u16_value(i16 zeroext)";
      "declare signext i16 @i16_value(i16 signext)";
      "call zeroext i1 @b(i1 zeroext true)";
      "call signext i8 @i8_value(i8 signext 255)";
    ];
  let c_definition_abi =
    llvm_of
      "extern \"C\" {\n\
       fn b_value(x bool) bool { return x }\n\
       fn u8_value(x u8) u8 { return x }\n\
       fn i8_value(x i8) i8 { return x }\n\
       fn u16_value(x u16) u16 { return x }\n\
       fn i16_value(x i16) i16 { return x }\n\
       fn empty() void { }\n\
       }\n"
  in
  List.iter
    (fun expected ->
      if not (contains c_definition_abi expected) then
        failwith ("extern-c-definition: missing `" ^ expected ^ "`"))
    [
      "define zeroext i1 @b_value(i1 zeroext";
      "define zeroext i8 @u8_value(i8 zeroext";
      "define signext i8 @i8_value(i8 signext";
      "define zeroext i16 @u16_value(i16 zeroext";
      "define signext i16 @i16_value(i16 signext";
      "define void @empty()";
    ];
  let opaque_assembly_linkage =
    llvm_of
      "fn comment_name() i64 { return 1 }\n\
       fn Llocal() i64 { return 2 }\n\
       fn directive_name() i64 { return 3 }\n\
       asm fn raw_zero() i64 {\n\
       # comment_name\n\
       .Llocal:\n\
       .ascii \"directive_name\"\n\
       ret\n\
       }\n"
  in
  List.iter
    (fun name ->
      let expected = "define internal i64 @" ^ name ^ "()" in
      if not (contains opaque_assembly_linkage expected) then
        failwith ("assembly-linkage-opaque: missing `" ^ expected ^ "`"))
    [ "comment_name"; "Llocal"; "directive_name" ];
  let explicit_assembly_dependency =
    "extern \"C\" { fn assembly_helper(x i64) i64 { return x + 1 } }\n\
     asm fn assembly_call(x i64) i64 {\n\
     call assembly_helper\n\
     ret\n\
     }\n"
  in
  let explicit_assembly_program = lower_of explicit_assembly_dependency in
  let explicit_assembly_llvm = Ir.render explicit_assembly_program in
  if not (contains explicit_assembly_llvm "define i64 @assembly_helper(i64 %a0)") then
    failwith "assembly-linkage-explicit: C ABI helper is not externally visible";
  if not (contains (Ir.raw_assembly explicit_assembly_program) "call assembly_helper")
  then failwith "assembly-linkage-explicit: raw assembly dependency was not preserved";
  let repeated_assembly_program = lower_of explicit_assembly_dependency in
  if
    explicit_assembly_llvm <> Ir.render repeated_assembly_program
    || Ir.raw_assembly explicit_assembly_program
       <> Ir.raw_assembly repeated_assembly_program
  then failwith "assembly-linkage-determinism: output changed between compiler runs";
  let internal_aggregate_abi =
    llvm_of
      "struct S @align(16) { x i64 y i64 }\n\
       const A arr[2,i64] = {1, 2}\n\
       fn pass_struct(value S) S { return value }\n\
       fn pass_array(value arr[2,i64]) arr[2,i64] { return value }\n\
       fn pass_vector(value vec[3,i32]) vec[3,i32] { return value }\n\
       fn main() i32 {\n\
       s S = pass_struct((S){1, 2})\n\
       a arr[2,i64] = pass_array(A)\n\
       v vec[3,i32] = pass_vector(splat(3))\n\
       return zext[i32](s.x == a[0] && v[0] == 3)\n\
       }\n"
  in
  List.iter
    (fun expected ->
      if not (contains internal_aggregate_abi expected) then
        failwith ("internal-aggregate-abi: missing `" ^ expected ^ "`"))
    [
      "call %struct.S @pass_struct(%struct.S";
      "call [2 x i64] @pass_array([2 x i64]";
      "call <3 x i32> @pass_vector(<3 x i32>";
    ];
  let arity_messages =
    semantic_messages
      "fn id[N const usize](x u64) u64 { return x + bitcast[u64](N) }\n\
       fn main() u64 { return id[3](2, 4) }\n"
  in
  (match arity_messages with
  | [ "wrong number of arguments" ] -> ()
  | _ -> failwith "const-specialization-arity: wrong message");
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
  if not (contains u64_max "ret i64 -1\n") then
    failwith "fas-020: valid u64 maximum literal was rejected";
  let signed_const_eval =
    llvm_of
      "const X i8 = -4 / 2\n\
       const R i8 = -5 % 2\n\
       const A i8 = -4 >> 1\n\
       const B bool = -1 < 0\n\
       fn main() i32 { return sext[i32](X) + sext[i32](R) + sext[i32](A) + \
       sext[i32](B) }\n"
  in
  if
    (not (contains signed_const_eval "sext i8 254 to i32"))
    || (not (contains signed_const_eval "sext i8 255 to i32"))
    || not (contains signed_const_eval "sext i1 true to i32")
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
    (not
       (contains nested_align
          "%struct.Inner = type { [0 x <16 x i8>], i8, i8, [14 x i8] }"))
    || not
         (contains nested_align "%struct.Outer = type { i8, [15 x i8], %struct.Inner }")
  then failwith "fas-003: nested aligned struct layout lost internal padding";
  semantic_error "fas-003-excessive-alignment"
    "alignment exceeds target maximum of 2147483648"
    "struct S @align(4294967296) { x u8 }\n";
  semantic_error "fas-009-duplicate-opaque" "duplicate type `x`"
    "opaque x\nopaque x\nfn main() i32 { return 0 }\n";
  let neutral_named_type =
    expect_ok
      (Parser.parse
         (source "fn use(value ptr[Handle]) i64 { return 0 }\nopaque Handle\n"))
  in
  (match neutral_named_type.Ast.items with
  | Ast.Func { params = [ { ty = Ast.Ptr (Ast.Named_type "Handle"); _ } ]; _ } :: _ ->
      ()
  | _ -> failwith "named-type-neutral-ast: parser classified a declaration name");
  ignore (lower_of "fn use(value ptr[Handle]) i64 { return 0 }\nopaque Handle\n");
  let forward_struct =
    llvm_of
      "fn make() i64 { value Pair = (Pair){7, 9}\n\
      \ return value.right }\n\
       struct Pair { left i64 right i64 }\n"
  in
  if not (contains forward_struct "%struct.Pair = type { i64, i64 }") then
    failwith "forward-struct: declaration was not resolved before function checking";
  let use_file =
    ( "use.fas",
      "fn read(object ptr[Handle]) i64 { value Pair = (Pair){11}\n return value.x }\n"
    )
  in
  let declarations_file = ("types.fas", "opaque Handle\nstruct Pair { x i64 }\n") in
  List.iter
    (fun files -> ignore (expect_ok (check_files files) |> Lower.lower |> expect_ok))
    [ [ use_file; declarations_file ]; [ declarations_file; use_file ] ];
  semantic_error "unknown-named-type" "unknown type `Missing`"
    "fn use(value ptr[Missing]) i64 { return 0 }\n";
  semantic_error "opaque-struct-literal" "opaque type `Handle` is not a struct"
    "opaque Handle\nfn use() i64 { (Handle){}\n return 0 }\n";
  ignore
    (lower_of
       "fn preserve(value ptr[Handle]) ptr[Handle] { local ptr[Handle] = value\n\
       \ return local }\n\
        fn read_only(value ptr[Handle]) ptr[const Handle] { return value }\n\
        fn pointer_size() usize { return sizeof[ptr[Handle]] }\n\
        fn pointer_align() usize { return alignof[ptr[const Handle]] }\n\
        opaque Handle\n");
  semantic_error "opaque-local-by-value"
    "opaque type `Handle` may only be used behind a pointer"
    "opaque Handle\nfn use() void { value Handle }\n";
  semantic_error "opaque-struct-field-by-value" "opaque type `Handle` has no layout"
    "opaque Handle\nstruct Wrapper { value Handle }\n";
  semantic_error "opaque-array-by-value"
    "opaque type `Handle` may only be used behind a pointer"
    "opaque Handle\nfn use() void { values arr[2,Handle] }\n";
  semantic_error "opaque-vector-by-value" "vector element type must be a scalar"
    "opaque Handle\nfn use() void { values vec[2,Handle] }\n";
  semantic_error "opaque-parameter-by-value"
    "opaque type `Handle` may only be used behind a pointer"
    "opaque Handle\nfn use(value Handle) void { return }\n";
  semantic_error "opaque-return-by-value"
    "opaque type `Handle` may only be used behind a pointer"
    "opaque Handle\nfn use() Handle { }\n";
  semantic_error "opaque-sizeof" "opaque type `Handle` has no layout"
    "opaque Handle\nfn use() usize { return sizeof[Handle] }\n";
  semantic_error "opaque-alignof" "opaque type `Handle` has no layout"
    "opaque Handle\nfn use() usize { return alignof[Handle] }\n";
  semantic_error "opaque-dereference" "cannot dereference an opaque pointer"
    "opaque Handle\nfn use(value ptr[Handle]) void { value.* }\n";
  semantic_error "opaque-index" "opaque pointers cannot be indexed"
    "opaque Handle\nfn use(value ptr[Handle]) i32 { value[0]\n return 0 }\n";
  semantic_error "opaque-field-access" "cannot dereference an opaque pointer"
    "opaque Handle\nfn use(value ptr[Handle]) i32 { value.*.field\n return 0 }\n";
  semantic_error "opaque-implicit-erasure"
    "type mismatch: expected ptr[u8], got ptr[Handle]"
    "opaque Handle\nfn use(value ptr[Handle]) ptr[u8] { return value }\n";
  semantic_error "opaque-distinct-assignment"
    "type mismatch: expected ptr[Second], got ptr[First]"
    "opaque First\n\
     opaque Second\n\
     fn use(value ptr[First]) void { other ptr[Second] = value }\n";
  semantic_error "opaque-distinct-comparison" "binary operands must have the same type"
    "opaque First\n\
     opaque Second\n\
     fn use(left ptr[First], right ptr[Second]) bool { return left == right }\n";
  ignore
    (lower_of
       "fn accept(value ptr[const u64]) void { return }\n\
        fn return_read_only(value ptr[u64]) ptr[const u64] { return value }\n\
        fn choose(flag bool, mutable ptr[u64], read_only ptr[const u64]) ptr[const \
        u64] {\n\
       \ return flag ? mutable : read_only\n\
        }\n\
        fn use(value ptr[u64]) bool {\n\
       \ read_only ptr[const u64] = value\n\
       \ accept(value)\n\
       \ read_only = value\n\
       \ return value == read_only\n\
        }\n");
  semantic_error "pointer-const-to-mutable-assignment"
    "type mismatch: expected ptr[u64], got ptr[const u64]"
    "fn use(value ptr[const u64]) void { mutable ptr[u64] = value }\n";
  semantic_error "pointer-const-to-mutable-argument"
    "type mismatch: expected ptr[u64], got ptr[const u64]"
    "fn take(value ptr[u64]) void { return }\n\
     fn use(value ptr[const u64]) void { take(value) }\n";
  semantic_error "pointer-const-to-mutable-return"
    "type mismatch: expected ptr[u64], got ptr[const u64]"
    "fn use(value ptr[const u64]) ptr[u64] { return value }\n";
  semantic_error "pointer-const-to-mutable-ternary"
    "type mismatch: expected ptr[u64], got ptr[const u64]"
    "fn use(flag bool, mutable ptr[u64], read_only ptr[const u64]) ptr[u64] {\n\
    \ return flag ? mutable : read_only\n\
     }\n";
  semantic_error "pointer-different-integer-element"
    "type mismatch: expected ptr[u8], got ptr[u64]"
    "fn use(value ptr[u64]) void { other ptr[u8] = value }\n";
  semantic_error "pointer-different-struct-element"
    "type mismatch: expected ptr[Second], got ptr[First]"
    "struct First { value u64 }\n\
     struct Second { value u64 }\n\
     fn use(value ptr[First]) ptr[Second] { return value }\n";
  semantic_error "pointer-different-opaque-element"
    "type mismatch: expected ptr[Second], got ptr[First]"
    "opaque First\n\
     opaque Second\n\
     fn take(value ptr[Second]) void { return }\n\
     fn use(value ptr[First]) void { take(value) }\n";
  semantic_error "pointer-different-element-comparison"
    "binary operands must have the same type"
    "fn use(left ptr[u8], right ptr[u64]) bool { return left == right }\n";
  semantic_error "pointer-different-element-ternary" "ternary arms have different types"
    "fn use(flag bool, left ptr[u8], right ptr[u64]) ptr[u8] {\n\
    \ return flag ? left : right\n\
     }\n";
  semantic_error "pointer-nested-constness"
    "type mismatch: expected ptr[ptr[const u8]], got ptr[ptr[u8]]"
    "fn use(value ptr[ptr[u8]]) void { nested ptr[ptr[const u8]] = value }\n";
  semantic_error "pointer-implicit-to-integer"
    "type mismatch: expected usize, got ptr[u8]"
    "fn use(value ptr[u8]) void { bits usize = value }\n";
  semantic_error "integer-implicit-to-pointer"
    "type mismatch: expected ptr[u8], got usize"
    "fn use(value usize) void { pointer ptr[u8] = value }\n";
  semantic_error "pointer-bitcast-discards-const"
    "illegal cast for source and destination widths"
    "fn use(value ptr[const u8]) ptr[u8] { return bitcast[ptr[u8]](value) }\n";
  semantic_error "pointer-bitcast-u32-width"
    "illegal cast for source and destination widths"
    "fn use(value ptr[u8]) u32 { return bitcast[u32](value) }\n";
  semantic_error "pointer-bitcast-i32-width"
    "illegal cast for source and destination widths"
    "fn use(value i32) ptr[u8] { return bitcast[ptr[u8]](value) }\n";
  semantic_error "const-pointer-bitcast-u32-width"
    "illegal cast for source and destination widths"
    "fn use(value ptr[const u8]) u32 { return bitcast[u32](value) }\n";
  semantic_error "integer-bitcast-const-pointer-u32-width"
    "illegal cast for source and destination widths"
    "fn use(value u32) ptr[const u8] { return bitcast[ptr[const u8]](value) }\n";
  let before_messages =
    semantic_messages
      "struct Stable { x i64 }\nfn use(value Missing) i64 { return 0 }\n"
  in
  let after_messages =
    semantic_messages
      "fn use(value Missing) i64 { return 0 }\nstruct Stable { x i64 }\n"
  in
  if before_messages <> after_messages then
    failwith "named-type-order-diagnostic: declaration reordering changed diagnostics";
  ignore
    (lower_of
       "fn main() u64 { values arr[2,u64]\n\
       \ values[0] = id[3](4)\n\
       \ values[1] = 5\n\
       \ return values[0] }\n\
        fn id[N const usize](value u64) u64 { return value + bitcast[u64](N) }\n");
  ignore
    (lower_of "fn choose(value bool) i64 { if (value){ return 1 } else { return 0 } }\n");
  ignore
    (lower_of
       "fn controls(value i64, condition bool) i64 {\n\
        while (condition){ break }\n\
        switch (value){ case 0: { return 0 } default: { } }\n\
        for ; condition; (value){ break }\n\
        return value\n\
        }\n");
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
  List.iter
    (fun (name, source) ->
      parse_error_message
        ("forbidden-attribute-" ^ name)
        ("unknown attribute `@" ^ name ^ "`")
        source)
    [
      ("inline", "@inline\nfn f() i64 { return 7 }\n");
      ("noinline", "@noinline\nfn f() i64 { return 7 }\n");
      ("kernel", "@kernel\nfn f() i64 { return 7 }\n");
      ("optimize", "@optimize\nfn f() i64 { return 7 }\n");
      ("target", "@target(\"zen3\")\nfn f() i64 { return 7 }\n");
      ("expect_asm", "@expect_asm(\"add\")\nfn f() i64 { return 7 }\n");
      ("expect_no_call", "@expect_no_call\nfn f() i64 { return 7 }\n");
      ("expect_stack_max", "@expect_stack_max(\"16\")\nfn f() i64 { return 7 }\n");
      ("unroll", "@unroll(8)\nfn f() i64 { return 7 }\n");
      ("vector_width", "@vector_width(4)\nfn f() i64 { return 7 }\n");
      ("hot", "@hot\nfn f() i64 { return 7 }\n");
      ("cold", "@cold\nfn f() i64 { return 7 }\n");
    ];
  parse_error_message "align-restricted-to-structs" "unknown attribute `@align`"
    "@align(16)\nfn f() i64 { return 7 }\n";
  parse_error_message "struct-rejects-function-attribute" "unknown attribute `@inline`"
    "struct S @inline { x i64 }\n";
  let attribute_free_ir =
    llvm_of
      "fn id[N const i64](x i64) i64 { return x }\nfn main() i64 { return id[3](4) }\n"
  in
  List.iter
    (fun marker ->
      if contains attribute_free_ir marker then
        failwith ("optimizer attribute leaked into LLVM: " ^ marker))
    [
      "inlinehint";
      "noinline";
      "optnone";
      "\"target-cpu\"";
      "\"target-features\"";
      "noalias";
      "noundef align";
    ];
  let cli_profile = cli_run [ "-debug"; "-no-inline"; "helper"; "profile.fas" ] in
  assert (cli_profile.Cli.no_inline_function = Some "helper");
  let cli_profile_ordered =
    cli_run [ "-O3"; "-debug"; "-no-inline"; "helper"; "profile.fas" ]
  in
  assert (cli_profile_ordered.Cli.optimization = 3);
  cli_error "no-inline-missing-name" "requires a function name"
    [ "-debug"; "-no-inline" ];
  cli_error "no-inline-flag-name" "requires a function name"
    [ "-debug"; "-no-inline"; "-O3"; "profile.fas" ];
  cli_error "no-inline-needs-debug" "requires -debug"
    [ "-no-inline"; "helper"; "profile.fas" ];
  cli_error "no-inline-duplicate" "duplicate -no-inline"
    [ "-debug"; "-no-inline"; "helper"; "-no-inline"; "other"; "profile.fas" ];
  let ast_profile_path = Filename.temp_file "fas-profile-ast-" ".fas" in
  Fun.protect
    ~finally:(fun () -> Sys.remove ast_profile_path)
    (fun () ->
      let channel = open_out_bin ast_profile_path in
      output_string channel "fn helper() i64 { return 3 }\n";
      close_out channel;
      let config =
        cli_run [ "-debug"; "--emit-ast"; "-no-inline"; "helper"; ast_profile_path ]
      in
      match Driver.run config with
      | Error diagnostics ->
          let rendered = Diag.render_all ~source:None diagnostics in
          if
            not
              (contains rendered
                 "-no-inline function `helper` requires an emitted function")
          then failwith "no-inline: AST diagnostic changed"
      | Ok _ -> failwith "no-inline: AST emission was accepted");
  cli_error "removed-release-option" "unknown option: -release"
    [ "-release"; "profile.fas" ];
  cli_error "removed-kernel-option" "unknown option: -kernel"
    [ "-kernel"; "profile.fas" ];
  let profile_path = Filename.temp_file "fas-profile-" ".fas" in
  Fun.protect
    ~finally:(fun () -> Sys.remove profile_path)
    (fun () ->
      let channel = open_out_bin profile_path in
      output_string channel
        "fn helper() i64 { return 3 }\n\
         fn other() i64 { return 4 }\n\
         fn main() i64 { return helper() }\n";
      close_out channel;
      let config =
        cli_run [ "-debug"; "-O3"; "--emit-llvm"; "-no-inline"; "helper"; profile_path ]
      in
      assert (config.Cli.optimization = 3);
      let profile_ir =
        match Driver.run config with
        | Ok output -> output
        | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
      in
      if not (contains profile_ir "@helper() noinline {") then
        failwith "no-inline: selected function missing LLVM attribute";
      if contains profile_ir "@other() noinline {" then
        failwith "no-inline: attribute leaked to another function";
      let missing_config =
        cli_run [ "-debug"; "--emit-llvm"; "-no-inline"; "missing"; profile_path ]
      in
      match Driver.run missing_config with
      | Error diagnostics ->
          let rendered = Diag.render_all ~source:None diagnostics in
          if not (contains rendered "function `missing` was not emitted") then
            failwith "no-inline: missing function diagnostic changed"
      | Ok _ -> failwith "no-inline: missing function was accepted");
  let generic_profile_source =
    "fn use() i64 { return identity[i64](7) }\n\
     fn identity[T](value T) T { return value }\n"
  in
  let generic_profile_hir =
    expect_ok (Parser.parse (source generic_profile_source)) |> Sema.check |> expect_ok
  in
  let generic_profile_name =
    match
      List.find_opt
        (fun (func : Hir.func) -> contains func.name "identity$spec$")
        generic_profile_hir.Hir.funcs
    with
    | Some func -> func.name
    | None -> failwith "no-inline: generic specialization was not emitted"
  in
  let generic_profile_path = Filename.temp_file "fas-profile-generic-" ".fas" in
  Fun.protect
    ~finally:(fun () -> Sys.remove generic_profile_path)
    (fun () ->
      let channel = open_out_bin generic_profile_path in
      output_string channel generic_profile_source;
      close_out channel;
      let config =
        cli_run
          [
            "-debug";
            "-O3";
            "--emit-llvm";
            "-no-inline";
            generic_profile_name;
            generic_profile_path;
          ]
      in
      let output =
        match Driver.run config with
        | Ok output -> output
        | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
      in
      if
        not
          (contains output (Ir.quote_identifier generic_profile_name)
          && contains output "noinline {")
      then failwith "no-inline: generic specialization was not selected");
  let external_profile_path = Filename.temp_file "fas-profile-external-" ".fas" in
  Fun.protect
    ~finally:(fun () -> Sys.remove external_profile_path)
    (fun () ->
      let channel = open_out_bin external_profile_path in
      output_string channel
        "extern \"C\" { fn external() i64 }\nfn main() i64 { return 0 }\n";
      close_out channel;
      let config =
        cli_run
          [ "-debug"; "--emit-llvm"; "-no-inline"; "external"; external_profile_path ]
      in
      match Driver.run config with
      | Error diagnostics ->
          let rendered = Diag.render_all ~source:None diagnostics in
          if not (contains rendered "not a normal definition") then
            failwith "no-inline: external declaration diagnostic changed"
      | Ok _ -> failwith "no-inline: external declaration was accepted");
  let asm_profile_path = Filename.temp_file "fas-profile-asm-" ".fas" in
  Fun.protect
    ~finally:(fun () -> Sys.remove asm_profile_path)
    (fun () ->
      let channel = open_out_bin asm_profile_path in
      output_string channel
        "asm fn asm_zero() i64 {\n retq\n}\nfn main() i64 { return 0 }\n";
      close_out channel;
      let config =
        cli_run [ "-debug"; "--emit-llvm"; "-no-inline"; "asm_zero"; asm_profile_path ]
      in
      match Driver.run config with
      | Error diagnostics ->
          let rendered = Diag.render_all ~source:None diagnostics in
          if not (contains rendered "not a normal definition") then
            failwith "no-inline: raw assembly diagnostic changed"
      | Ok _ -> failwith "no-inline: raw assembly was accepted");
  let budget_source_path = Filename.temp_file "fas-budget-source-" ".fas" in
  Fun.protect
    ~finally:(fun () -> Sys.remove budget_source_path)
    (fun () ->
      let channel = open_out_bin budget_source_path in
      output_string channel "fn main() i32 {\n  s ptr[const u8] = \"";
      output_string channel (String.make 3_999_999 'A');
      output_string channel "\"\n  return 0\n}\n";
      close_out channel;
      let expect_budget_rejection config label =
        match Driver.run config with
        | Error diagnostics ->
            let rendered = Diag.render_all ~source:None diagnostics in
            if
              not
                (contains rendered
                   "rendered LLVM text exceeds the configured limit of 4000000 bytes")
            then failwith (label ^ ": unexpected diagnostic: " ^ rendered)
        | Ok _ -> failwith (label ^ ": oversized render was accepted")
      in
      let llvm_output_path = Filename.temp_file "fas-budget-output-" ".ll" in
      Sys.remove llvm_output_path;
      Fun.protect
        ~finally:(fun () ->
          if Sys.file_exists llvm_output_path then Sys.remove llvm_output_path)
        (fun () ->
          let config =
            cli_run [ "--emit-llvm"; "-o"; llvm_output_path; budget_source_path ]
          in
          expect_budget_rejection config "budget";
          if Sys.file_exists llvm_output_path then
            failwith "budget: rejected emission wrote output");
      let asm_output_path = Filename.temp_file "fas-budget-asm-" ".s" in
      Sys.remove asm_output_path;
      Fun.protect
        ~finally:(fun () ->
          if Sys.file_exists asm_output_path then Sys.remove asm_output_path)
        (fun () ->
          let config = cli_run [ "-S"; "-o"; asm_output_path; budget_source_path ] in
          expect_budget_rejection config "budget asm";
          if Sys.file_exists asm_output_path then
            failwith "budget: rejected assembly emission wrote output"));
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
  if not (contains signed_rem_const "ret i64 0\n") then
    failwith "signed-rem-constant-overflow: expected zero remainder";

  semantic_error "fas-026-const-array-write" "cannot modify constant"
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
      \ take(\"\")\n\
      \ take(c\"\")\n\
      \ return 0 }\n"
  in
  if
    (not (contains string_literals "[1 x i8] c\"x\""))
    || not (contains string_literals "[2 x i8] c\"x\\00\"")
  then failwith "fas-030-string-literals: incorrect literal storage";
  if
    (not (contains string_literals "[0 x i8] c\"\""))
    || not (contains string_literals "[1 x i8] c\"\\00\"")
  then failwith "fas-030-string-literals: incorrect empty literal storage";
  let string_pointer =
    llvm_of
      "fn read(p ptr[const u8]) i32 { return zext[i32](p[0]) }\n\
       fn main() i32 { return read(\"x\") }\n"
  in
  if not (contains string_pointer "call i32 @read(ptr") then
    failwith "fas-030-string-literals: read-only pointer call was rejected";
  let byte_literal_semantics =
    llvm_of
      "const ByteCount usize = len(\"a\\0b\") + len(\"\\n\") + len(\"é\")\n\
       fn bytes() ptr[const u8] { return \"a\\0b\" }\n\
       fn main() usize { return ByteCount }\n"
  in
  if not (contains byte_literal_semantics "[3 x i8] c\"a\\00b\"") then
    failwith "fas-030-string-literals: ordinary embedded NUL was not preserved";
  if not (contains byte_literal_semantics "ret i64 6\n") then
    failwith "fas-030-string-literals: literal length did not count decoded bytes";
  semantic_error "fas-030-string-literal-fixed-array"
    "type mismatch: expected arr[3, u8], got ptr[const u8]"
    "fn main() i32 { bytes arr[3,u8] = \"abc\"\n return 0 }\n";
  semantic_error "fas-030-c-string-literal-nul"
    "C string literal cannot contain embedded NUL"
    "fn main() i32 { c\"a\\0b\"[0]\n return 0 }\n";
  semantic_error "fas-030-const-c-string-literal-nul"
    "C string literal cannot contain embedded NUL"
    "const N usize = len(c\"a\\0b\")\nfn main() usize { return N }\n";
  let raw_literal_length =
    llvm_of "fn main() i64 { return bitcast[i64](len(\"abc\")) }\n"
  in
  if not (contains raw_literal_length "ret i64 3\n") then
    failwith "fas-031-len: raw literal length is incorrect";
  let c_literal_length =
    llvm_of "fn main() i64 { return bitcast[i64](len(c\"abc\")) }\n"
  in
  if not (contains c_literal_length "ret i64 3\n") then
    failwith "fas-031-len: C literal payload length is incorrect";
  let literal_const_specialization =
    llvm_of
      "fn literal_size[N const usize]() usize { return N }\n\
       fn main() usize { return literal_size[len(\"abc\")]() }\n"
  in
  if not (contains literal_const_specialization "N=usize:3\"") then
    failwith "fas-031-len: literal length was not accepted as a const argument";
  let array_length =
    llvm_of
      "const K arr[3, u8] = {1, 2, 3}\n\
       const N usize = len(\"abc\")\n\
       fn main() i64 { return bitcast[i64](len(K)) + bitcast[i64](N) }\n"
  in
  if
    (not (contains array_length "ret i64 %v"))
    || not (contains array_length "add i64 3, 3\n")
  then failwith "fas-031-len: fixed array length is incorrect";
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
      "const C u8 = 1 << 8\n\
       fn run(x u8, n u8) u8 { return x << n }\n\
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
       zext[i64](L16) + zext[i64](C32) +zext[i64](L32) + bitcast[i64](C64) + \
       bitcast[i64](L64) }\n"
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
  semantic_error "scalar-const-function-collision" "duplicate declaration `F`"
    "const F i64 = 1\nfn F() i64 { return 3 }\n";
  semantic_error "function-scalar-const-collision" "duplicate declaration `F`"
    "fn F() i64 { return 3 }\nconst F i64 = 1\n";
  semantic_error "type-function-collision" "duplicate declaration `F`"
    "struct F { value i64 }\nfn F() i64 { return 3 }\n";
  semantic_error "function-type-collision" "duplicate declaration `F`"
    "fn F() i64 { return 3 }\nstruct F { value i64 }\n";
  semantic_error "type-const-collision" "duplicate declaration `F`"
    "opaque F\nconst F i64 = 1\n";
  semantic_error "const-type-collision" "duplicate declaration `F`"
    "const F i64 = 1\nopaque F\n";
  semantic_error "constant-used-as-function" "constant, not a function"
    "const F i64 = 1\nfn use() i64 { return F() }\n";
  semantic_error "function-used-as-value" "function, not a value"
    "fn F() i64 { return 1 }\nfn use() i64 { return F }\n";
  semantic_error "type-used-as-value" "type, not a value"
    "opaque F\nfn use() i64 { return F }\n";
  semantic_error "const-env-duplicate-const" "duplicate const `A`"
    "const A arr[2, i64] = {1, 2}\nconst A i64 = 1\n";
  semantic_error "const-env-array-length-mismatch" "const array length mismatch"
    "const A arr[2, i64] = {1, 2, 3}\n";
  semantic_error "const-env-array-element-type-mismatch"
    "const array element type mismatch" "const A arr[2, i64] = {1, true}\n";
  semantic_error "const-env-array-needs-brace-list"
    "const array needs a brace-list initializer" "const A arr[2, i64] = 5\n";
  semantic_error "const-env-brace-list-requires-array"
    "brace-list requires an array type" "struct S { x i64 y i64 }\nconst C S = {1, 2}\n";

  semantic_error "fas-021-local-aggregate-limit"
    "aggregate element count exceeds the configured limit"
    "fn main() i32 { a arr[1000001,u8]\n return 0 }\n";
  semantic_error "vector-shape-cap-precedes-aggregate-limit"
    "vector lane count exceeds the portable cap of 256"
    "fn main() i32 { v vec[1000001,u8]\n return 0 }\n";
  semantic_error "fas-021-nested-aggregate-limit"
    "aggregate element count exceeds the configured limit"
    "fn main() i32 { a arr[100000,arr[20,u8]]\n return 0 }\n";

  let bool_sext =
    llvm_of "const B i64 = sext[i64](true)\nfn f() i64 { return sext[i64](true) }\n"
  in
  if not (contains bool_sext "sext i1 true to i64") then
    failwith "bool-sext: expected sign-extending lowering";

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
  if not (contains shadowed_template "ret i64 5\n") then
    failwith "fas-015: global const shadowed template const parameter";

  let nested_shadowed_template =
    llvm_of
      "const N i64 = 100\n\
       fn inner[N const i64]() i64 { return N }\n\
       fn outer[N const i64]() i64 { return inner[N]() }\n\
       fn main() i64 { return outer[5]() }\n"
  in
  if not (contains nested_shadowed_template "ret i64 5\n") then
    failwith "fas-015: nested specialization used global const over template parameter";

  let signed_narrow_specialization =
    llvm_of
      "fn b8[N const i8](x i8) i32 { return sext[i32](x) + sext[i32](N) }\n\
       fn main() i32 { return b8[-5](2) }\n"
  in
  if
    (not (contains signed_narrow_specialization "add i32"))
    || not (contains signed_narrow_specialization "sext i8 251 to i32")
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
        (Printf.sprintf "`%s` is reserved and cannot be used as a binding" name)
        (Printf.sprintf "fn %s(x i64) i64 { return x }\n" name))
    Names.operation_names;

  List.iter
    (fun name ->
      semantic_error ("reserved-type-" ^ name)
        "is reserved and cannot be used as a binding"
        (Printf.sprintf "struct %s { value i32 }\n" name))
    Names.primitive_type_names;

  List.iter
    (fun name ->
      semantic_error ("reserved-literal-" ^ name)
        "is reserved and cannot be used as a binding"
        (Printf.sprintf "const %s bool = false\n" name))
    Names.literal_names;

  List.iter
    (fun (name, text) ->
      semantic_error name "is reserved and cannot be used as a binding" text)
    [
      ("reserved-primitive-type", "struct i32 { value i32 }\n");
      ("reserved-literal-const", "const true bool = false\n");
      ("reserved-generic-parameter", "fn value[len](x i32) i32 { return x }\n");
      ("reserved-const-parameter", "fn value[splat const i32](x i32) i32 { return x }\n");
      ("reserved-runtime-parameter", "fn value(popcount i32) i32 { return popcount }\n");
      ("reserved-local", "fn value() i32 { len i32 = 1\n return len }\n");
    ];

  let released_shift_names =
    llvm_of
      "fn shl(x i32) i32 { return x + 1 }\n\
       fn lshr(x i32) i32 { return x + 2 }\n\
       fn ashr(x i32) i32 { return x + 3 }\n\
       fn shr(x i32) i32 { return x + 4 }\n\
       fn main() i32 { return shl(1) + lshr(2) + ashr(3) + shr(4) }\n"
  in
  List.iter
    (fun name ->
      if not (contains released_shift_names ("call i32 @" ^ name)) then
        failwith ("released-shift-name: user function was not called: " ^ name))
    [ "shl"; "lshr"; "ashr"; "shr" ];

  let released_unreserved_names =
    llvm_of
      "struct Members { len i32 i32 i32 true i32 }\n\
       fn sqrt(x i32) i32 { return x }\n\
       fn fma(x i32) i32 { return x }\n\
       fn main() i32 { value Members = (Members){1, 2, 3}\n\
       return sqrt(value.len) + fma(value.i32) + value.true }\n"
  in
  List.iter
    (fun marker ->
      if not (contains released_unreserved_names marker) then
        failwith ("released-unreserved-name: missing `" ^ marker ^ "`"))
    [ "call i32 @sqrt"; "call i32 @fma"; "%struct.Members = type" ];

  let hygienic_parameter_names =
    llvm_of
      "fn value_collision(v0 i32) i32 { return v0 }\n\
       fn block_collision(b0 i32) i32 { if b0 == 0 { return 1 } else { return b0 } }\n\
       fn main() i32 { return value_collision(7) + block_collision(2) }\n"
  in
  List.iter
    (fun marker ->
      if not (contains hygienic_parameter_names marker) then
        failwith ("hygienic-parameter-names: missing `" ^ marker ^ "`"))
    [
      "define internal i32 @value_collision(i32 %a0)";
      "define internal i32 @block_collision(i32 %a0)";
      "%v0 = alloca i32";
      "b0:";
    ];
  if
    contains hygienic_parameter_names "@value_collision(i32 %v0)"
    || contains hygienic_parameter_names "@block_collision(i32 %b0)"
  then failwith "hygienic-parameter-names: source name escaped into LLVM identity";

  (match
     Parser.parse
       (source
          "fn main() i32 {\nswitch 0 {\ndefault:\nreturn 1\ndefault:\nreturn 2\n}\n}\n")
   with
  | Ok _ -> failwith "duplicate-default: expected parse rejection"
  | Error [ diagnostic ] ->
      if diagnostic.Diag.message <> "duplicate default arm" then
        failwith "duplicate-default: unexpected message";
      if diagnostic.primary.Span.line <> 5 then
        failwith "duplicate-default: wrong primary location";
      if
        not
          (List.exists (fun note -> contains note "regression.fas:3:") diagnostic.notes)
      then failwith "duplicate-default: first location missing"
  | Error _ -> failwith "duplicate-default: unexpected diagnostic count");

  let leading_zero_literals =
    llvm_of
      "fn decimal() u64 { return 000000000000000000001 }\n\
       fn hexadecimal() u64 { return 0x000000000000000000001 }\n\
       fn octal() u64 { return 0o000000000000000000001 }\n\
       fn binary() u64 { return 0b000000000000000000001 }\n\
       fn separated() u64 { return 0000_0000_0000_0001 }\n\
       fn zero() u64 { return 000000000000000000000 }\n\
       fn decimal_max() u64 { return 00018446744073709551615 }\n\
       fn hexadecimal_max() u64 { return 0x0000FFFFFFFFFFFFFFFF }\n\
       fn octal_max() u64 { return 0o00001777777777777777777777 }\n\
       fn binary_max() u64 { return \
       0b00001111111111111111111111111111111111111111111111111111111111111111 }\n\
       fn signed_minimum() i64 { return -00009223372036854775808 }\n"
  in
  if List.length (positions leading_zero_literals "ret i64 1\n") <> 5 then
    failwith "leading-zero-literals: nonzero values were not normalized";
  if not (contains leading_zero_literals "ret i64 0\n") then
    failwith "leading-zero-literals: zero-only spelling was rejected";
  if List.length (positions leading_zero_literals "ret i64 -1\n") <> 4 then
    failwith "leading-zero-literals: padded maximum values were rejected";
  if not (contains leading_zero_literals "ret i64 -9223372036854775808\n") then
    failwith "leading-zero-literals: padded signed minimum was rejected";
  List.iter
    (fun (name, literal) ->
      semantic_error name "integer literal overflows 64 bits"
        (Printf.sprintf "fn value() u64 { return %s }\n" literal))
    [
      ("leading-zero-decimal-overflow", "00018446744073709551616");
      ("leading-zero-hex-overflow", "0x000100000000000000000");
      ("leading-zero-octal-overflow", "0o00002000000000000000000000");
      ( "leading-zero-binary-overflow",
        "0b000010000000000000000000000000000000000000000000000000000000000000000" );
    ];

  let spec_count_limits = { Limits.default with max_specializations = 1 } in
  let repeated_spec_at_count_limit =
    expect_ok
      (Sema.check ~limits:spec_count_limits
         (expect_ok
            (Parser.parse
               (source
                  "fn id[N const usize](x u64) u64 { return x + bitcast[u64](N) }\n\
                   fn main() u64 { return id[3](2) + id[1 + 2](3) }\n"))))
  in
  if List.length repeated_spec_at_count_limit.Hir.funcs <> 2 then
    failwith "spec-count-limit: repeated specialization was not deduplicated";
  let distinct_spec_at_count_limit =
    expect_ok
      (Parser.parse
         (source
            "fn id[N const usize](x u64) u64 { return x + bitcast[u64](N) }\n\
             fn main() u64 { return id[3](2) + id[4](3) }\n"))
  in
  let count_limit_failure () =
    match Sema.check ~limits:spec_count_limits distinct_spec_at_count_limit with
    | Ok _ -> failwith "spec-count-limit: expected rejection at the count limit"
    | Error diagnostics -> Diag.render_all ~source:None diagnostics
  in
  let first_count_limit_failure = count_limit_failure () in
  let second_count_limit_failure = count_limit_failure () in
  if first_count_limit_failure <> second_count_limit_failure then
    failwith "spec-count-limit: exhaustion diagnostic was not deterministic";
  ignore
    (expect_ok
       (Sema.check ~limits:spec_count_limits
          (expect_ok
             (Parser.parse
                (source
                   "fn id[N const usize](x u64) u64 { return x + bitcast[u64](N) }\n\
                    fn main() u64 { return id[3](2) + id[3](3) }\n")))));
  if
    not (contains first_count_limit_failure "const specialization count limit exceeded")
  then failwith "spec-count-limit: unexpected diagnostic";

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
  let specialization_order_source =
    "fn leaf[N const usize]() usize { return N }\n\
     fn left[N const usize]() usize { return leaf[N + 10]() }\n\
     fn right[N const usize]() usize { return leaf[N + 20]() }\n\
     fn main() usize { return left[1]() + right[2]() }\n"
  in
  let specialization_order = llvm_of specialization_order_source in
  if specialization_order <> llvm_of specialization_order_source then
    failwith "specialization-order: generated output was not deterministic";
  let markers =
    [
      "define internal i64 @\"left$spec$4:1:N=usize:1\"()";
      "define internal i64 @\"right$spec$5:1:N=usize:2\"()";
      "define internal i64 @\"leaf$spec$4:1:N=usize:11\"()";
      "define internal i64 @\"leaf$spec$4:1:N=usize:22\"()";
    ]
  in
  let marker_positions =
    List.map
      (fun marker ->
        match positions specialization_order marker with
        | position :: _ -> position
        | [] -> failwith ("specialization-order: missing `" ^ marker ^ "`"))
      markers
  in
  if marker_positions <> List.sort compare marker_positions then
    failwith "specialization-order: work queue did not preserve discovery order";
  let bool_const_generic_llvm =
    llvm_of
      "const Enabled bool = true\n\
       fn choose[Flag const bool]() i32 {\n\
      \ if Flag { return 7 } else { return 3 }\n\
       }\n\
       fn byte[N const u8]() i32 { return zext[i32](N) }\n\
       fn main() i32 {\n\
      \ return choose[true]() + choose[false]() + choose[1]() + choose[Enabled]() \
       +choose[trunc[bool](3)]() + byte[zext[u8](true)]()\n\
       }\n"
  in
  if
    (not
       (contains bool_const_generic_llvm
          "define internal i32 @\"choose$spec$6:4:Flag=bool:1\"()"))
    || not
         (contains bool_const_generic_llvm
            "define internal i32 @\"choose$spec$6:4:Flag=bool:0\"()")
  then failwith "const-param-bool: typed specializations were not emitted";
  if contains bool_const_generic_llvm "br i1" then
    failwith "const-param-bool: specialization condition was not pruned";
  semantic_error "const-param-bool-named-integer" "const argument type mismatch"
    "const One u8 = 1\n\
     fn choose[Flag const bool]() i32 { if Flag { return 1 } else { return 0 } }\n\
     fn main() i32 { return choose[One]() }\n";
  semantic_error "const-param-bool-literal-range"
    "integer literal is out of range for bool"
    "fn choose[Flag const bool]() i32 { if Flag { return 1 } else { return 0 } }\n\
     fn main() i32 { return choose[2]() }\n";
  semantic_error "const-param-integer-bool-value" "const argument type mismatch"
    "fn byte[N const u8]() i32 { return zext[i32](N) }\n\
     fn main() i32 { return byte[true]() }\n";

  semantic_error "const-param-pointer-rejected"
    "const parameter type must be a scalar integer or bool"
    "fn id[N const ptr[u8]](x u64) u64 { return x }\n";

  semantic_error "const-param-duplicate" "duplicate generic parameter `N`"
    "fn id[N const usize, N const usize](x u64) u64 { return x + N }\n";
  semantic_error "runtime-parameter-duplicate" "duplicate parameter `value`"
    "fn id(value i32, value i32) i32 { return value }\n";
  semantic_error "type-runtime-parameter-collision"
    "parameter `T` conflicts with a generic parameter"
    "fn id[T](T i32) i32 { return T }\n";
  semantic_error "const-runtime-parameter-collision"
    "parameter `N` conflicts with a generic parameter"
    "fn id[N const i32](N i32) i32 { return N }\n";
  ignore
    (lower_of
       "fn id[T](value T) T { { T i32 = 1 }\n\
       \ return value }\n\
        fn main() i32 { return id[i32](7) }\n");

  let generic_declarations =
    expect_ok
      (Parser.parse
         (source
            "struct Buffer[T, N const usize] { data arr[N, T] }\n\
             fn choose[T, N const usize, U](value T) U { return value }\n"))
  in
  (match generic_declarations.Ast.items with
  | [
   Ast.Struct
     {
       generic_params =
         [
           Ast.Type_param { name = "T"; _ };
           Ast.Const_param { name = "N"; ty = Ast.Int Ast.Usize; _ };
         ];
       fields = [ { ty = Ast.Array ("N", Ast.Named_type "T"); _ } ];
       _;
     };
   Ast.Func
     {
       generic_params =
         [
           Ast.Type_param { name = "T"; _ };
           Ast.Const_param { name = "N"; ty = Ast.Int Ast.Usize; _ };
           Ast.Type_param { name = "U"; _ };
         ];
       params = [ { ty = Ast.Named_type "T"; _ } ];
       ret = Ast.Named_type "U";
       _;
     };
  ] ->
      ()
  | _ -> failwith "type-generic-declarations: parameter kinds or order were lost");
  let rendered_generic_declarations = Ast.render_program generic_declarations in
  if
    (not (contains rendered_generic_declarations "struct Buffer[T, N const usize]"))
    || not (contains rendered_generic_declarations "fn choose[T, N const usize, U]")
  then failwith "type-generic-declarations: AST rendering lost generic parameters";
  let applied_type_syntax =
    expect_ok
      (Parser.parse (source "fn use(value Mixed[T, ptr[u8], 3]) i64 { return 0 }\n"))
  in
  (match applied_type_syntax.Ast.items with
  | [
   Ast.Func
     {
       params =
         [
           {
             ty =
               Ast.Applied_type
                 ( "Mixed",
                   [
                     Ast.Name_arg ("T", _);
                     Ast.Type_arg (Ast.Ptr (Ast.Int Ast.U8));
                     Ast.Const_arg (Ast.Int_lit ("3", _));
                   ],
                   _ );
             _;
           };
         ];
       _;
     };
  ] ->
      ()
  | _ -> failwith "generic-argument-syntax: argument forms were not preserved");
  parse_error_message "type-generic-missing-const-type" "expected a type, found `]`"
    "fn bad[T const](value T) T { return value }\n";
  let generic_function_source =
    "fn use() i64 { return identity[i64](identity[i64](7)) }\n\
     fn identity[T](value T) T { return value }\n"
  in
  let generic_function_hir =
    expect_ok (Parser.parse (source generic_function_source)) |> Sema.check |> expect_ok
  in
  let identity_specializations =
    List.filter
      (fun (func : Hir.func) ->
        contains func.name "identity$spec$" && func.name <> "identity")
      generic_function_hir.Hir.funcs
  in
  if List.length identity_specializations <> 1 then
    failwith "generic-function-deduplication: expected one concrete identity function";
  (match identity_specializations with
  | [ { params = [ { ty = Hir.Int Hir.I64; _ } ]; ret = Hir.Int Hir.I64; _ } ] -> ()
  | _ -> failwith "generic-function-substitution: signature was not specialized");
  let generic_function_llvm =
    Ir.render (expect_ok (Lower.lower generic_function_hir))
  in
  if generic_function_llvm <> llvm_of generic_function_source then
    failwith "generic-function-order: generated output was not deterministic";
  if contains generic_function_llvm "define internal i64 @identity(" then
    failwith "generic-function-template: template reached LLVM output";
  if not (contains generic_function_llvm "identity$spec$") then
    failwith "generic-function-lowering: concrete specialization did not reach LLVM";
  ignore
    (expect_ok
       (Sema.check
          (expect_ok
             (Parser.parse
                (source
                   "fn first[A, B](left A, right B) A { return left }\n\
                    fn main() i64 { return first[i64, u8](7, 1) }\n")))));
  let nested_generic_function_source =
    "struct Box[T] { value T }\n\
     fn inner[T](value T) Box[T] { return (Box[T]){value} }\n\
     fn outer[T](value T) Box[T] { return inner[T](value) }\n\
     fn use() Box[u8] { return outer[u8](3) }\n"
  in
  let nested_generic_function_hir =
    expect_ok (Parser.parse (source nested_generic_function_source))
    |> Sema.check |> expect_ok
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) -> contains func.name "$spec$")
         nested_generic_function_hir.Hir.funcs)
    <> 2
  then failwith "generic-function-nesting: nested specialization was not discovered";
  if
    List.length
      (List.filter
         (fun (definition : Hir.struct_def) -> contains definition.name "Box$spec$")
         nested_generic_function_hir.Hir.structs)
    <> 1
  then
    failwith
      "generic-function-struct-use: specialized body did not materialize its struct";
  let nested_generic_argument_hir =
    expect_ok
      (Parser.parse
         (source
            "fn pick[T, N const usize](v T) T { return v }\n\
             fn main() i64 { return pick[i64, 2](pick[i64, 1](7)) }\n"))
    |> Sema.check |> expect_ok
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) -> contains func.name "$spec$")
         nested_generic_argument_hir.Hir.funcs)
    <> 2
  then failwith "nested-generic-argument: mixed nested call specializations incorrect";
  ignore (Lower.lower nested_generic_argument_hir |> expect_ok);
  let nested_conversion_argument_hir =
    expect_ok
      (Parser.parse
         (source
            "fn pick[T, N const usize](v T) T { return v }\n\
             fn main() i64 { return pick[i64, 2](zext[i64](pick[u8, 1](3))) }\n"))
    |> Sema.check |> expect_ok
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) -> contains func.name "$spec$")
         nested_conversion_argument_hir.Hir.funcs)
    <> 2
  then
    failwith "nested-generic-argument: nested conversion call specializations incorrect";
  ignore (Lower.lower nested_conversion_argument_hir |> expect_ok);
  let nested_type_argument =
    llvm_of
      "struct Box[T] { value T }\n\
       fn sz[T]() usize { return sizeof[T] }\n\
       fn main() usize { return sz[Box[u8]]() }\n"
  in
  if not (contains nested_type_argument "ret i64 1\n") then
    failwith
      "nested-type-argument: sizeof over nested generic type argument was not evaluated";
  let sizeof_const_argument =
    llvm_of
      "fn ret[N const usize]() usize { return N }\n\
       fn main() usize { return ret[sizeof[u8]]() }\n"
  in
  if not (contains sizeof_const_argument "ret i64 1\n") then
    failwith "sizeof-const-argument: nested sizeof query was not evaluated";
  let alignof_const_argument =
    llvm_of
      "fn ret[N const usize]() usize { return N }\n\
       fn main() usize { return ret[1 + alignof[u16]]() }\n"
  in
  if not (contains alignof_const_argument "ret i64 3\n") then
    failwith "alignof-const-argument: nested alignof expression was not evaluated";
  let nested_sizeof_const_argument =
    llvm_of
      "struct Box[T] { value T }\n\
       fn ret[N const usize]() usize { return N }\n\
       fn main() usize { return ret[sizeof[Box[i64]]]() }\n"
  in
  if not (contains nested_sizeof_const_argument "ret i64 8\n") then
    failwith
      "nested-sizeof-const-argument: sizeof over nested generic type was not evaluated";
  let arithmetic_sizeof_const_argument =
    llvm_of
      "fn ret[N const usize]() usize { return N }\n\
       fn main() usize { return ret[sizeof[arr[3, u8]] - 2]() }\n"
  in
  if not (contains arithmetic_sizeof_const_argument "ret i64 1\n") then
    failwith
      "arithmetic-sizeof-const-argument: nested query arithmetic was not evaluated";
  semantic_error "const-argument-call-rejected" "invalid constant builtin call"
    "fn sz[T]() usize { return sizeof[T] }\n\
     fn pick[T, N const usize](v T) T { return v }\n\
     fn main() i64 { return pick[i64, sz[u8]()](7) }\n";
  let while_parameter_shadow =
    llvm_of
      "const N usize = 99\n\
       fn count[N const usize]() usize { i usize = 0\n\
      \ while i < N { i = i + 1 }\n\
      \ return i }\n\
       fn main() usize { return count[3]() }\n"
  in
  if not (contains while_parameter_shadow "icmp ult i64 %v1, 3\n") then
    failwith "while-const-argument: while condition did not use the const parameter";
  if contains while_parameter_shadow "99" then
    failwith "while-const-argument: while condition resolved the shadowing global";
  let defer_parameter_shadow =
    llvm_of
      "const N usize = 99\n\
       fn f[N const usize]() usize { defer { x usize = N\n\
      \ x = x + 1 }\n\
      \ return N }\n\
       fn main() usize { return f[2]() }\n"
  in
  if not (contains defer_parameter_shadow "store i64 2, ptr") then
    failwith "defer-const-argument: defer body did not use the const parameter";
  if contains defer_parameter_shadow "99" then
    failwith "defer-const-argument: defer body resolved the shadowing global";
  let statement_positions =
    llvm_of
      "const N usize = 99\n\
       fn walk[N const usize](p usize) usize { i usize = 0\n\
      \ for j usize = 0; j < N; j += 1 { i = i + N }\n\
      \ while i < N * 4 { i = i + 1 }\n\
      \ switch N { case 1: { i = i + N } case 2: { i = i + N } default: { i = 0 } }\n\
      \ if N > 1 { i = i + N }\n\
      \ defer { i = i + N }\n\
      \ i = i + N\n\
      \ return i }\n\
       fn main() usize { return walk[2](0) }\n"
  in
  if not (contains statement_positions "switch i64 2, label") then
    failwith "statement-positions: switch scrutinee did not use the const parameter";
  if not (contains statement_positions "icmp ult i64 %v3, 2\n") then
    failwith "statement-positions: for bound did not use the const parameter";
  if not (contains statement_positions "N=usize:2\"") then
    failwith "statement-positions: specialization key lost const argument identity";
  if contains statement_positions "99" then
    failwith "statement-positions: a statement position resolved the shadowing global";
  let expr_statement_shadow =
    llvm_of
      "const N usize = 99\n\
       fn f[N const usize]() usize { N + 1\n\
      \ return N }\n\
       fn main() usize { return f[2]() }\n"
  in
  if not (contains expr_statement_shadow "add i64 2, 1\n") then
    failwith "expr-stmt-const-argument: expr statement did not use the const parameter";
  if not (contains expr_statement_shadow "N=usize:2\"") then
    failwith "expr-stmt-const-argument: specialization key lost const argument identity";
  if contains expr_statement_shadow "99" then
    failwith "expr-stmt-const-argument: expr statement resolved the shadowing global";
  let expr_statement_call =
    llvm_of
      "const N usize = 99\n\
       fn g[M const usize]() usize { return M }\n\
       fn f[N const usize]() usize { g[N]()\n\
      \ return N }\n\
       fn main() usize { return f[2]() }\n"
  in
  if not (contains expr_statement_call "M=usize:2\"") then
    failwith
      "expr-stmt-call-const-argument: nested call did not use the const parameter";
  if contains expr_statement_call "99" then
    failwith "expr-stmt-call-const-argument: nested call resolved the shadowing global";
  let mixed_bitwise_compare =
    llvm_of
      "fn f(x u64, m u64, e u64) bool { return x & m == e }\n\
       fn main() i32 { return 0 }\n"
  in
  if not (contains mixed_bitwise_compare "and i64 %v3, %v4\n") then
    failwith "ruled-precedence: bitwise operands did not group first";
  if not (contains mixed_bitwise_compare "icmp eq i64 %v5, %v6\n") then
    failwith "ruled-precedence: comparison did not apply to the bitwise result";
  let ruled_additive_shift =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       fn main() usize { return w[1 + 2 << 3]() }\n"
  in
  if not (contains ruled_additive_shift "ret i64 24\n") then
    failwith "ruled-precedence: additive no longer binds tighter than shift";
  let ruled_shift_bitand =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       fn main() usize { return w[1 << 2 & 4]() }\n"
  in
  if not (contains ruled_shift_bitand "ret i64 4\n") then
    failwith "ruled-precedence: shift no longer binds tighter than bitwise and";
  let ruled_bitand_bitxor =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       fn main() usize { return w[2 & 3 ^ 1]() }\n"
  in
  if not (contains ruled_bitand_bitxor "ret i64 3\n") then
    failwith "ruled-precedence: bitwise and/xor group boundary moved";
  let ruled_bitxor_bitor =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       fn main() usize { return w[1 ^ 3 | 1]() }\n"
  in
  if not (contains ruled_bitxor_bitor "ret i64 3\n") then
    failwith "ruled-precedence: xor/or group boundary moved";
  let ruled_shift_assoc =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       fn main() usize { return w[16 >> 2 >> 1]() }\n"
  in
  if not (contains ruled_shift_assoc "ret i64 2\n") then
    failwith "ruled-precedence: shift chain lost left associativity";
  let literal_hex =
    llvm_of
      "fn w[N const usize]() usize { return N }\nfn main() usize { return w[0xff]() }\n"
  in
  if not (contains literal_hex "ret i64 255\n") then
    failwith "literal-bases: hex literal did not evaluate to 255";
  let literal_binary =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       fn main() usize { return w[0b1010]() }\n"
  in
  if not (contains literal_binary "ret i64 10\n") then
    failwith "literal-bases: binary literal did not evaluate to 10";
  let literal_octal =
    llvm_of
      "fn w[N const usize]() usize { return N }\nfn main() usize { return w[0o17]() }\n"
  in
  if not (contains literal_octal "ret i64 15\n") then
    failwith "literal-bases: octal literal did not evaluate to 15";
  let literal_negative_hex =
    llvm_of
      "fn w[N const i64]() i64 { return N }\nfn main() i64 { return w[-0x10]() }\n"
  in
  if not (contains literal_negative_hex "ret i64 -16\n") then
    failwith "literal-bases: negative hex literal did not evaluate to -16";
  let literal_underscores =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       fn main() usize { return w[0x1_00]() }\n"
  in
  if not (contains literal_underscores "ret i64 256\n") then
    failwith "literal-bases: underscore-separated literal did not evaluate to 256";
  semantic_error "generic-args-missing-rejected"
    "generic function `f` requires arguments"
    "fn f[N const usize]() usize { return N }\nfn main() usize { return f() }\n";
  semantic_error "generic-args-extra-rejected" "wrong number of const arguments to `f`"
    "fn f[N const usize]() usize { return N }\nfn main() usize { return f[1, 2]() }\n";
  semantic_error "generic-kind-type-for-const-rejected" "expected a const argument"
    "fn f[N const usize]() usize { return N }\nfn main() usize { return f[i64]() }\n";
  semantic_error "generic-kind-const-for-type-rejected" "expected a type argument"
    "fn f[T]() usize { return 0 }\nfn main() usize { return f[1]() }\n";
  semantic_error "generic-duplicate-parameter-rejected"
    "duplicate generic parameter `T`"
    "fn f[T, T]() usize { return 0 }\nfn main() usize { return f[i64]() }\n";
  semantic_error "generic-unknown-type-argument-rejected" "unknown type `Nope`"
    "fn f[T]() usize { return 0 }\nfn main() usize { return f[Nope]() }\n";
  ignore
    (llvm_of "fn f(x u8) usize { return zext[usize](x) }\nfn main() i32 { return 0 }\n");
  ignore
    (llvm_of "fn f(x i8) isize { return sext[isize](x) }\nfn main() i32 { return 0 }\n");
  ignore
    (llvm_of "fn f(x usize) u8 { return trunc[u8](x) }\nfn main() i32 { return 0 }\n");
  let zext_const_value =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       const B u8 = 255\n\
       fn main() usize { return w[zext[usize](B)]() }\n"
  in
  if not (contains zext_const_value "ret i64 255\n") then
    failwith "target-width-conversion: zext const value mismatch";
  let sext_const_value =
    llvm_of
      "fn w[N const isize]() isize { return N }\n\
       const B i8 = -1\n\
       fn main() isize { return w[sext[isize](B)]() }\n"
  in
  if not (contains sext_const_value "ret i64 -1\n") then
    failwith "target-width-conversion: sext const value mismatch";
  let trunc_const_value =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       const B usize = 300\n\
       fn main() usize { return w[zext[usize](trunc[u8](B))]() }\n"
  in
  if not (contains trunc_const_value "ret i64 44\n") then
    failwith "target-width-conversion: trunc low-bit value mismatch";
  semantic_error "target-width-equal-cast-rejected"
    "illegal cast for source and destination widths"
    "fn f(x usize) u64 { return zext[u64](x) }\nfn main() i32 { return 0 }\n";
  semantic_error "target-width-trunc-widen-rejected"
    "illegal cast for source and destination widths"
    "fn f(x u8) i64 { return trunc[i64](x) }\nfn main() i32 { return 0 }\n";
  semantic_error "target-width-zext-shrink-rejected"
    "illegal cast for source and destination widths"
    "fn f(x i64) i8 { return zext[i8](x) }\nfn main() i32 { return 0 }\n";
  semantic_error "target-width-sext-equal-rejected"
    "illegal cast for source and destination widths"
    "fn f(x i32) i32 { return sext[i32](x) }\nfn main() i32 { return 0 }\n";
  semantic_error "target-width-sext-shrink-rejected"
    "illegal cast for source and destination widths"
    "fn f(x i64) i8 { return sext[i8](x) }\nfn main() i32 { return 0 }\n";
  let zext_signed_value =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       const B i8 = -1\n\
       fn main() usize { return w[zext[usize](B)]() }\n"
  in
  if not (contains zext_signed_value "ret i64 255\n") then
    failwith "target-width-conversion: zext of signed source must zero-fill (255)";
  let sext_unsigned_value =
    llvm_of
      "fn w[N const isize]() isize { return N }\n\
       const D u8 = 255\n\
       fn main() isize { return w[sext[isize](D)]() }\n"
  in
  if not (contains sext_unsigned_value "ret i64 -1\n") then
    failwith "target-width-conversion: sext of unsigned source must sign-fill (-1)";
  let call_argument_order =
    llvm_of
      "fn g() i64 { return 1 }\n\
       fn h() i64 { return 2 }\n\
       fn f(a i64, b i64) i64 { return a + b }\n\
       fn main() i64 { return f(g(), h()) }\n"
  in
  if
    not
      (contains call_argument_order "\n  %v0 = call i64 @g()\n  %v1 = call i64 @h()\n")
  then failwith "eval-order: call arguments did not evaluate left to right";
  let compound_assign_dest_once =
    llvm_of
      "fn i() usize { return 0 }\n\
       fn v() i64 { return 5 }\n\
       fn main() i64 { a arr[4, i64]\n\
      \ a[0] = 0\n\
      \ a[1] = 0\n\
      \ a[2] = 0\n\
      \ a[3] = 0\n\
      \ a[i()] += v()\n\
      \ return a[0] }\n"
  in
  let dest_calls = positions compound_assign_dest_once "call i64 @i()" in
  let rhs_calls = positions compound_assign_dest_once "call i64 @v()" in
  (match (dest_calls, rhs_calls) with
  | [ dest ], [ rhs ] when dest < rhs -> ()
  | _ ->
      failwith
        "eval-order: compound assignment did not evaluate its destination exactly once \
         before the rhs");
  let short_circuit_guard =
    llvm_of
      "fn q() bool { return true }\n\
       fn k(a bool) bool { return a && q() }\n\
       fn main() i32 { return 0 }\n"
  in
  let guard_branches = positions short_circuit_guard "br i1" in
  let guarded_calls = positions short_circuit_guard "call i1 @q()" in
  (match (guard_branches, guarded_calls) with
  | br :: _, [ call ] when br < call -> ()
  | _ ->
      failwith
        "eval-order: short-circuit rhs was not evaluated exactly once after the guard \
         branch");
  let assign_dest_once =
    llvm_of
      "fn i() usize { return 0 }\n\
       fn v() i64 { return 5 }\n\
       fn main() i64 { a arr[4, i64]\n\
      \ a[0] = 0\n\
      \ a[1] = 0\n\
      \ a[2] = 0\n\
      \ a[3] = 0\n\
      \ a[i()] = v()\n\
      \ return a[0] }\n"
  in
  let dest_calls = positions assign_dest_once "call i64 @i()" in
  let rhs_calls = positions assign_dest_once "call i64 @v()" in
  (match (dest_calls, rhs_calls) with
  | [ dest ], [ rhs ] when dest < rhs -> ()
  | _ -> failwith "eval-order: assign dest not once before rhs");
  let return_before_defer =
    llvm_of
      "fn f() i64 { x i64 = 1\n\
       defer { x = 2 }\n\
       return x }\n\
       fn main() i64 { return f() }\n"
  in
  let capture_load = positions return_before_defer "load i64" in
  let cleanup_store = positions return_before_defer "store i64 2, ptr" in
  (match (capture_load, cleanup_store) with
  | [ load ], [ store ] when load < store -> ()
  | _ -> failwith "eval-order: defer ran before return capture");
  let defer_reverse_order =
    llvm_of
      "fn f() i64 { x i64 = 0\n\
       defer { x = 1 }\n\
       defer { x = 2 }\n\
       return x }\n\
       fn main() i64 { return f() }\n"
  in
  let second_defer = positions defer_reverse_order "store i64 2, ptr" in
  let first_defer = positions defer_reverse_order "store i64 1, ptr" in
  (match (second_defer, first_defer) with
  | [ second ], [ first ] when second < first -> ()
  | _ -> failwith "eval-order: defers not reverse order");
  let lane_update_dest =
    llvm_of
      "fn i() usize { return 0 }\n\
       fn v() i64 { return 5 }\n\
       fn main() i64 { x vec[2, i64] = splat(1)\n\
      \ x[i()] = v()\n\
      \ return 0 }\n"
  in
  let lane_dest_calls = positions lane_update_dest "call i64 @i()" in
  let lane_rhs_calls = positions lane_update_dest "call i64 @v()" in
  (match (lane_dest_calls, lane_rhs_calls) with
  | [ dest ], [ rhs ] when dest < rhs -> ()
  | _ -> failwith "eval-order: lane update dest not once before rhs");
  let lane_compound_dest =
    llvm_of
      "fn i() usize { return 0 }\n\
       fn v() i64 { return 5 }\n\
       fn main() i64 { x vec[2, i64] = splat(1)\n\
      \ x[i()] += v()\n\
      \ return 0 }\n"
  in
  let compound_dest_calls = positions lane_compound_dest "call i64 @i()" in
  let compound_rhs_calls = positions lane_compound_dest "call i64 @v()" in
  (match (compound_dest_calls, compound_rhs_calls) with
  | [ dest ], [ rhs ] when dest < rhs -> ()
  | _ -> failwith "eval-order: lane compound dest not once before rhs");
  let switch_no_fallthrough =
    llvm_of
      "fn main() i64 { n i64 = 1\n\
      \ switch n {\n\
      \  case 1: { n = 2 }\n\
      \  case 2: { n = 3 }\n\
      \  default: { n = 0 }\n\
      \ }\n\
      \ return n }\n"
  in
  if
    not
      (contains switch_no_fallthrough "store i64 2, ptr %v0, align 8\n  br label %b1\n")
  then failwith "control: switch case skipped its exit branch";
  if contains switch_no_fallthrough "store i64 2, ptr %v0, align 8\n  store i64 3" then
    failwith "control: switch case fell through";
  if contains switch_no_fallthrough "store i64 3, ptr %v0, align 8\n  store i64 0" then
    failwith "control: switch case fell through";
  let condition_before_branch =
    llvm_of
      "fn c() bool { return true }\n\
       fn main() i64 { x i64 = 0\n\
      \ if c() { x = 1 }\n\
      \ return x }\n"
  in
  let cond_calls = positions condition_before_branch "call i1 @c()" in
  let cond_branches = positions condition_before_branch "br i1" in
  let body_stores = positions condition_before_branch "store i64 1, ptr" in
  (match (cond_calls, cond_branches, body_stores) with
  | [ call ], [ branch ], [ store ] when call < branch && branch < store -> ()
  | _ -> failwith "eval-order: condition or body misordered");
  let or_short_circuit =
    llvm_of
      "fn q() bool { return true }\n\
       fn k(a bool) bool { return a || q() }\n\
       fn main() i32 { return 0 }\n"
  in
  let or_guards = positions or_short_circuit "br i1" in
  let or_calls = positions or_short_circuit "call i1 @q()" in
  (match (or_guards, or_calls) with
  | [ guard ], [ call ] when guard < call -> ()
  | _ -> failwith "eval-order: or rhs not once after guard");
  let defer_reads_current =
    llvm_of
      "fn f() i64 { x i64 = 1\n\
      \ defer { x = x + 1 }\n\
      \ x = 2\n\
      \ return x }\n\
       fn main() i64 { return f() }\n"
  in
  let defer_mutation = positions defer_reads_current "store i64 2, ptr" in
  let defer_reads = positions defer_reads_current "load i64" in
  (match (defer_mutation, defer_reads) with
  | [ mutation ], read :: _ when mutation < read -> ()
  | _ -> failwith "eval-order: defer read stale values");
  let break_messages =
    semantic_messages
      "fn main() i64 { n i64 = 0\n\
      \ switch n {\n\
      \  case 0: { break }\n\
      \  default: { n = 9 }\n\
      \ }\n\
      \ return n }\n"
  in
  (match break_messages with
  | [ "break outside loop" ] -> ()
  | _ -> failwith "break-outside-loop: wrong message");
  let continue_messages = semantic_messages "fn main() i64 { continue }\n" in
  (match continue_messages with
  | [ "continue outside loop" ] -> ()
  | _ -> failwith "continue-outside-loop: wrong message");
  let void_fallthrough_defer =
    llvm_of
      "fn f() void { x i64 = 0\n\
      \ defer { x = 1 }\n\
      \ }\n\
       fn main() i32 { f()\n\
      \ return 0 }\n"
  in
  let defer_store = positions void_fallthrough_defer "store i64 1, ptr" in
  let fallthrough_exit = positions void_fallthrough_defer "ret void" in
  (match (defer_store, fallthrough_exit) with
  | [ store ], [ exit ] when store < exit -> ()
  | _ -> failwith "control: void fallthrough skipped defer");
  let mask_negation_admit =
    llvm_of
      "fn not2(a vec[2, bool]) vec[2, bool] { return !a }\nfn main() i32 { return 0 }\n"
  in
  if not (contains mask_negation_admit "define internal <2 x i1> @not2(<2 x i1>") then
    failwith "control: mask negation not admitted";
  let bitnot_messages =
    semantic_messages
      "fn not2(a vec[2, bool]) vec[2, bool] { return ~a }\nfn main() i32 { return 0 }\n"
  in
  (match bitnot_messages with
  | [ "integer unary operator requires an integer" ] -> ()
  | _ -> failwith "mask-bitnot-integer-only: wrong message");
  let unterminated_messages = parse_messages "fn main() i64 {\n" in
  (match unterminated_messages with
  | [ "unterminated block" ] -> ()
  | _ -> failwith "unterminated-block: wrong message");
  let case_messages =
    parse_messages "fn main() i64 { n i64 = 0\n switch n { foo }\n return 0 }\n"
  in
  (match case_messages with
  | [ "expected case, default, or `}`" ] -> ()
  | _ -> failwith "switch-case-parse: wrong message");
  let separator_messages =
    parse_messages "fn main() i64 { x i64 = 1 y i64 = 2\n return x }\n"
  in
  (match separator_messages with
  | [ "expected end of statement (newline or `;`)" ] -> ()
  | _ -> failwith "statement-separator-parse: wrong message");
  let extern_abi_messages = parse_messages "extern \"c\" fn f(x i64) i64\n" in
  (match extern_abi_messages with
  | [ "only extern \"C\" is supported" ] -> ()
  | _ -> failwith "extern-abi: wrong message");
  let variadic_messages = parse_messages "extern \"C\" { fn f(...) i64 }\n" in
  (match variadic_messages with
  | [ "a variadic declaration needs a fixed parameter" ] -> ()
  | _ -> failwith "variadic-fixed-param: wrong message");
  let literal_comma_messages = parse_messages "const a arr[2, i64] = { 1 2 }\n" in
  (match literal_comma_messages with
  | [ "expected comma between literal elements" ] -> ()
  | _ -> failwith "literal-comma: wrong message");
  let ellipsis_messages = parse_messages "fn f(...) i64 { return 0 }\n" in
  (match ellipsis_messages with
  | [ "`...` is legal only in extern \"C\"" ] -> ()
  | _ -> failwith "ellipsis-extern-only: wrong message");
  let block_comment_messages =
    parse_messages "fn main() i64 { return 0 }\n/* unclosed\n"
  in
  (match block_comment_messages with
  | [ "unterminated block comment" ] -> ()
  | _ -> failwith "block-comment: wrong message");
  let string_literal_messages = parse_messages "fn main() i64 { return 0 }\n\"abc\n" in
  (match string_literal_messages with
  | [ "unterminated string literal" ] -> ()
  | _ -> failwith "string-literal: wrong message");
  let integer_literal_messages =
    parse_messages "fn main() i64 { x i64 = 0x\n return 0 }\n"
  in
  (match integer_literal_messages with
  | [ "invalid integer literal" ] -> ()
  | _ -> failwith "integer-literal: wrong message");
  let unterminated_escape_messages =
    parse_messages "fn main() i64 { return 0 }\n\"abc\\"
  in
  (match unterminated_escape_messages with
  | [ "unterminated escape" ] -> ()
  | _ -> failwith "string-escape: wrong message");
  let unknown_escape_messages =
    parse_messages "fn main() i64 { return 0 }\n\"\\q\"\n"
  in
  (match unknown_escape_messages with
  | [ "unknown string escape" ] -> ()
  | _ -> failwith "unknown-escape: wrong message");
  let min_sext_i8 =
    llvm_of
      "fn w[N const isize]() isize { return N }\n\
       const B i8 = -128\n\
       fn main() isize { return w[sext[isize](B)]() }\n"
  in
  if not (contains min_sext_i8 "ret i64 -128\n") then
    failwith "value: i8 min sext drifted";
  let min_sext_i16 =
    llvm_of
      "fn w[N const isize]() isize { return N }\n\
       const B i16 = -32768\n\
       fn main() isize { return w[sext[isize](B)]() }\n"
  in
  if not (contains min_sext_i16 "ret i64 -32768\n") then
    failwith "value: i16 min sext drifted";
  let min_zext_i8_fill =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       const B i8 = -128\n\
       fn main() usize { return w[zext[usize](B)]() }\n"
  in
  if not (contains min_zext_i8_fill "ret i64 128\n") then
    failwith "value: i8 min zext fill drifted";
  let neg_hex_literal =
    llvm_of
      "fn w[N const isize]() isize { return N }\n\
       const B i8 = -0x80\n\
       fn main() isize { return w[sext[isize](B)]() }\n"
  in
  if not (contains neg_hex_literal "ret i64 -128\n") then
    failwith "value: negative hex literal drifted";
  let neg_binary_literal =
    llvm_of
      "fn w[N const isize]() isize { return N }\n\
       const B i8 = -0b10000000\n\
       fn main() isize { return w[sext[isize](B)]() }\n"
  in
  if not (contains neg_binary_literal "ret i64 -128\n") then
    failwith "value: negative binary literal drifted";
  let neg_octal_literal =
    llvm_of
      "fn w[N const isize]() isize { return N }\n\
       const B i8 = -0o200\n\
       fn main() isize { return w[sext[isize](B)]() }\n"
  in
  if not (contains neg_octal_literal "ret i64 -128\n") then
    failwith "value: negative octal literal drifted";
  let agreement_bitand_eq =
    llvm_of
      "const C bool = 4 & 2 == 2\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains agreement_bitand_eq "ret i64 0\n") then
    failwith
      "ruled-agreement: bitwise-and mixed form disagreed with its parenthesized twin";
  if contains agreement_bitand_eq "ret i64 1\n" then
    failwith "ruled-agreement: const generic branch was not pruned";
  let agreement_bitor_eq =
    llvm_of
      "const C bool = 1 | 0 == 0\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains agreement_bitor_eq "ret i64 0\n") then
    failwith
      "ruled-agreement: bitwise-or mixed form disagreed with its parenthesized twin";
  if contains agreement_bitor_eq "ret i64 1\n" then
    failwith "ruled-agreement: const generic branch was not pruned";
  let agreement_bitxor_eq =
    llvm_of
      "const C bool = 2 ^ 3 == 2\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains agreement_bitxor_eq "ret i64 0\n") then
    failwith
      "ruled-agreement: bitwise-xor mixed form disagreed with its parenthesized twin";
  if contains agreement_bitxor_eq "ret i64 1\n" then
    failwith "ruled-agreement: const generic branch was not pruned";
  let agreement_bitand_zero =
    llvm_of
      "const C bool = 6 & 3 == 0\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains agreement_bitand_zero "ret i64 0\n") then
    failwith
      "ruled-agreement: bitwise-and equality form disagreed with its parenthesized twin";
  if contains agreement_bitand_zero "ret i64 1\n" then
    failwith "ruled-agreement: const generic branch was not pruned";
  let agreement_bitand_rel =
    llvm_of
      "const C bool = 5 & 3 < 4\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains agreement_bitand_rel "ret i64 1\n") then
    failwith
      "ruled-agreement: bitwise-and relational form disagreed with its parenthesized \
       twin";
  if contains agreement_bitand_rel "ret i64 0\n" then
    failwith "ruled-agreement: const generic branch was not pruned";
  let agreement_runtime_shape =
    llvm_of "fn g(x u64) bool { return x & 3 == 1 }\nfn main() i32 { return 0 }\n"
  in
  if not (contains agreement_runtime_shape "and i64 %v1, 3\n") then
    failwith "ruled-agreement: runtime bitwise operands did not group first";
  if not (contains agreement_runtime_shape "icmp eq i64 %v2, 1\n") then
    failwith "ruled-agreement: runtime comparison did not apply to the bitwise result";
  let bool_bitand_true =
    llvm_of
      "const C bool = true & true\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains bool_bitand_true "ret i64 1\n") then
    failwith "bool-bitops: true & true did not evaluate to true";
  if contains bool_bitand_true "ret i64 0\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitand_false =
    llvm_of
      "const C bool = true & false\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains bool_bitand_false "ret i64 0\n") then
    failwith "bool-bitops: true & false did not evaluate to false";
  if contains bool_bitand_false "ret i64 1\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitor_false =
    llvm_of
      "const C bool = false | false\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains bool_bitor_false "ret i64 0\n") then
    failwith "bool-bitops: false | false did not evaluate to false";
  if contains bool_bitor_false "ret i64 1\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitor_true =
    llvm_of
      "const C bool = true | false\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains bool_bitor_true "ret i64 1\n") then
    failwith "bool-bitops: true | false did not evaluate to true";
  if contains bool_bitor_true "ret i64 0\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitxor_false =
    llvm_of
      "const C bool = true ^ true\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains bool_bitxor_false "ret i64 0\n") then
    failwith
      "bool-bitops: true ^ true did not evaluate to false (xor must agree with !=)";
  if contains bool_bitxor_false "ret i64 1\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitxor_true =
    llvm_of
      "const C bool = true ^ false\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains bool_bitxor_true "ret i64 1\n") then
    failwith
      "bool-bitops: true ^ false did not evaluate to true (xor must agree with !=)";
  if contains bool_bitxor_true "ret i64 0\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitand_both_false =
    llvm_of
      "const C bool = false & false\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains bool_bitand_both_false "ret i64 0\n") then
    failwith "bool-bitops: false & false did not evaluate to false";
  if contains bool_bitand_both_false "ret i64 1\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitor_both_true =
    llvm_of
      "const C bool = true | true\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains bool_bitor_both_true "ret i64 1\n") then
    failwith "bool-bitops: true | true did not evaluate to true";
  if contains bool_bitor_both_true "ret i64 0\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitxor_both_false =
    llvm_of
      "const C bool = false ^ false\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains bool_bitxor_both_false "ret i64 0\n") then
    failwith
      "bool-bitops: false ^ false did not evaluate to false (xor must agree with !=)";
  if contains bool_bitxor_both_false "ret i64 1\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitand_false_true =
    llvm_of
      "const C bool = false & true\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains bool_bitand_false_true "ret i64 0\n") then
    failwith "bool-bitops: false & true did not evaluate to false";
  if contains bool_bitand_false_true "ret i64 1\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitor_false_true =
    llvm_of
      "const C bool = false | true\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains bool_bitor_false_true "ret i64 1\n") then
    failwith "bool-bitops: false | true did not evaluate to true";
  if contains bool_bitor_false_true "ret i64 0\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitxor_false_true =
    llvm_of
      "const C bool = false ^ true\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn main() usize { return r[C]() }\n"
  in
  if not (contains bool_bitxor_false_true "ret i64 1\n") then
    failwith
      "bool-bitops: false ^ true did not evaluate to true (xor must agree with !=)";
  if contains bool_bitxor_false_true "ret i64 0\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitops_runtime =
    llvm_of
      "fn f(a bool, b bool) bool { return a & b }\n\
       fn g(a bool, b bool) bool { return a | b }\n\
       fn h(a bool, b bool) bool { return a ^ b }\n\
       fn main() i32 { return 0 }\n"
  in
  if not (contains bool_bitops_runtime " %v4 = and i1 %v2, %v3\n") then
    failwith "bool-bitops: runtime bitwise-and did not lower to one-bit and";
  if not (contains bool_bitops_runtime " %v4 = or i1 %v2, %v3\n") then
    failwith "bool-bitops: runtime bitwise-or did not lower to one-bit or";
  if not (contains bool_bitops_runtime " %v4 = xor i1 %v2, %v3\n") then
    failwith "bool-bitops: runtime bitwise-xor did not lower to one-bit xor";
  semantic_error "bool-mask-bitop-rejected"
    "arithmetic requires integer or vector operands"
    "fn f(a vec[2, bool], b vec[2, bool]) vec[2, bool] { return a & b }\n\
     fn main() i32 { return 0 }\n";
  semantic_error "int-bool-bitop-rejected" "binary operands must have the same type"
    "fn f(x u8, b bool) bool { return x & b }\nfn main() i32 { return 0 }\n";
  ignore
    (llvm_of "fn f(a vec[256, u8]) u8 { return a[0] }\nfn main() i32 { return 0 }\n");
  ignore
    (llvm_of "fn f(a vec[32, u64]) u64 { return a[0] }\nfn main() i32 { return 0 }\n");
  ignore
    (llvm_of "fn f(a vec[256, bool]) bool { return a[0] }\nfn main() i32 { return 0 }\n");
  semantic_error "vector-lane-cap-rejected"
    "vector lane count exceeds the portable cap of 256"
    "fn f(a vec[257, u8]) u8 { return a[0] }\nfn main() i32 { return 0 }\n";
  semantic_error "vector-size-cap-rejected"
    "vector size exceeds the portable cap of 2048 bits"
    "fn f(a vec[33, u64]) u64 { return a[0] }\nfn main() i32 { return 0 }\n";
  semantic_error "literal-range-const-u8-rejected"
    "integer literal is out of range for u8"
    "const C u8 = 256\n\
     fn w[N const u8]() u8 { return N }\n\
     fn main() u8 { return w[C]() }\n";
  semantic_error "literal-range-const-i8-rejected"
    "integer literal is out of range for i8"
    "const C i8 = -129\n\
     fn w[N const i8]() i8 { return N }\n\
     fn main() i8 { return w[C]() }\n";
  semantic_error "literal-range-runtime-u8-rejected"
    "integer literal is out of range for u8"
    "fn f() u8 { return 256 }\nfn main() i32 { return 0 }\n";
  semantic_error "vector-lane-cap-local-rejected"
    "vector lane count exceeds the portable cap of 256"
    "fn f() i64 { v vec[257, u8] = splat(1)\n\
    \ return 0 }\n\
     fn main() i64 { return f() }\n";
  semantic_error "vector-size-cap-const-rejected"
    "vector size exceeds the portable cap of 2048 bits"
    "const X vec[33, u64] = splat(0)\nfn main() i64 { return 0 }\n";
  let vec32_usize_legal =
    llvm_of "fn f(a vec[32, usize]) usize { return a[0] }\nfn main() i32 { return 0 }\n"
  in
  if not (contains vec32_usize_legal "extractelement <32 x i64>") then
    failwith "vector-caps: vec[32, usize] (2048 bits at max width) was not admitted";
  let vec32_isize_legal =
    llvm_of "fn f(a vec[32, isize]) isize { return a[0] }\nfn main() i32 { return 0 }\n"
  in
  if not (contains vec32_isize_legal "extractelement <32 x i64>") then
    failwith "vector-caps: vec[32, isize] (2048 bits at max width) was not admitted";
  semantic_error "vector-size-cap-usize-rejected"
    "vector size exceeds the portable cap of 2048 bits"
    "fn f(a vec[33, usize]) usize { return a[0] }\nfn main() i32 { return 0 }\n";
  semantic_error "vector-size-cap-isize-rejected"
    "vector size exceeds the portable cap of 2048 bits"
    "fn f(a vec[33, isize]) isize { return a[0] }\nfn main() i32 { return 0 }\n";
  semantic_error "dead-branch-literal-range-rejected"
    "integer literal is out of range for u8"
    "fn main() u8 { if false { return 256 }\n return 0 }\n";
  semantic_error "pruned-specialization-literal-range-rejected"
    "integer literal is out of range for u8"
    "fn r[B const bool]() u8 { if B { return 256 }\n\
    \ return 0 }\n\
     fn main() u8 { return r[false]() }\n";
  semantic_error "dead-branch-type-error-rejected"
    "type mismatch: expected u8, got bool"
    "fn main() u8 { if false { return true }\n return 0 }\n";
  semantic_error "dead-branch-unknown-name-rejected" "unknown name `nope`"
    "fn main() u8 { if false { return nope }\n return 0 }\n";
  let unused_generic_function =
    expect_ok
      (Sema.check
         (expect_ok
            (Parser.parse
               (source
                  "fn unused[T](value T) T { return value + value }\n\
                   fn main() i64 { return 0 }\n"))))
  in
  if
    List.exists
      (fun (func : Hir.func) -> contains func.name "unused")
      unused_generic_function.Hir.funcs
  then failwith "generic-function-template: unused template was emitted";
  let type_generic_failure =
    "fn bad[T](value T) T { return value + value }\n\
     fn main(value ptr[u8]) ptr[u8] { return bad[ptr[u8]](value) }\n"
  in
  (match semantic_diagnostics type_generic_failure with
  | [ diagnostic ] ->
      if diagnostic.message <> "arithmetic requires integer or vector operands" then
        failwith "generic-instantiation-type: root message changed";
      if
        diagnostic.primary.Span.file <> "regression.fas"
        || diagnostic.primary.Span.line <> 1
        || diagnostic.primary.Span.column <> 37
      then failwith "generic-instantiation-type: root span changed";
      if
        diagnostic.notes
        <> [ "while instantiating `bad[ptr[u8]]` at regression.fas:2:44" ]
      then
        failwith
          ("generic-instantiation-type: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-type: expected one diagnostic");
  let const_generic_failure =
    "fn bad[N const u64](value ptr[u8]) ptr[u8] { return value + value }\n\
     fn main(value ptr[u8]) ptr[u8] {\n\
     return bad[18446744073709551615](value)\n\
     }\n"
  in
  (match semantic_diagnostics const_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes
        <> [ "while instantiating `bad[18446744073709551615]` at regression.fas:3:11" ]
      then
        failwith
          ("generic-instantiation-const: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-const: expected one diagnostic");
  let mixed_generic_failure =
    "fn bad[T, N const u64](value T) T { return value + value }\n\
     fn main(value ptr[u8]) ptr[u8] {\n\
     return bad[ptr[u8], 18446744073709551615](value)\n\
     }\n"
  in
  (match semantic_diagnostics mixed_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes
        <> [
             "while instantiating `bad[ptr[u8], 18446744073709551615]` at \
              regression.fas:3:11";
           ]
      then
        failwith
          ("generic-instantiation-mixed: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-mixed: expected one diagnostic");
  let interleaved_generic_failure =
    "fn bad[A, N const u8, B, M const i8](left A, right B) A {\n\
     return left + left\n\
     }\n\
     fn main(value ptr[u8], other ptr[u16]) ptr[u8] {\n\
     return bad[ptr[u8], 2, ptr[u16], -3](value, other)\n\
     }\n"
  in
  (match semantic_diagnostics interleaved_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes
        <> [
             "while instantiating `bad[ptr[u8], 2, ptr[u16], -3]` at \
              regression.fas:5:11";
           ]
      then
        failwith
          ("generic-instantiation-order: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-order: expected one diagnostic");
  let nested_generic_failure =
    "fn inner[T](value T) T { return value + value }\n\
     fn outer[T](value T) T { return inner[T](value) }\n\
     fn main(value ptr[u8]) ptr[u8] { return outer[ptr[u8]](value) }\n"
  in
  (match semantic_diagnostics nested_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes
        <> [
             "while instantiating `outer[ptr[u8]]` at regression.fas:3:46";
             "while instantiating `inner[ptr[u8]]` at regression.fas:2:38";
           ]
      then
        failwith
          ("generic-instantiation-nested: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-nested: expected one diagnostic");
  let nested_const_generic_failure =
    "fn inner[N const usize](value ptr[u8]) ptr[u8] { return value + value }\n\
     fn outer[T](value T) T { return inner[4](value) }\n\
     fn main(value ptr[u8]) ptr[u8] { return outer[ptr[u8]](value) }\n"
  in
  (match semantic_diagnostics nested_const_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes
        <> [
             "while instantiating `outer[ptr[u8]]` at regression.fas:3:46";
             "while instantiating `inner[4]` at regression.fas:2:38";
           ]
      then
        failwith
          ("generic-instantiation-nested-const: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-nested-const: expected one diagnostic");
  let generic_struct_field_failure =
    "struct Bad[T] { value Missing }\nfn main(value Bad[u8]) i64 { return 0 }\n"
  in
  (match semantic_diagnostics generic_struct_field_failure with
  | [ diagnostic ] ->
      if diagnostic.message <> "unknown type `Missing`" then
        failwith "generic-instantiation-struct-field: root message changed";
      if diagnostic.notes <> [ "while instantiating `Bad[u8]` at regression.fas:2:18" ]
      then
        failwith
          ("generic-instantiation-struct-field: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-struct-field: expected one diagnostic");
  let generic_struct_layout_failure =
    "struct Bad[T] { value void }\nfn main(value Bad[u8]) i64 { return 0 }\n"
  in
  (match semantic_diagnostics generic_struct_layout_failure with
  | [ diagnostic ] ->
      if diagnostic.message <> "void has no object layout" then
        failwith "generic-instantiation-struct-layout: root message changed";
      if diagnostic.notes <> [ "while instantiating `Bad[u8]` at regression.fas:2:18" ]
      then
        failwith
          ("generic-instantiation-struct-layout: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-struct-layout: expected one diagnostic");
  let recursive_generic_struct_failure =
    "struct Recursive[T] { value Recursive[T] }\n\
     fn main(value Recursive[u8]) i64 { return 0 }\n"
  in
  (match semantic_diagnostics recursive_generic_struct_failure with
  | [ diagnostic ] ->
      if diagnostic.message <> "recursive by-value struct `Recursive[u8]`" then
        failwith
          ("generic-instantiation-recursive-struct: unexpected message: "
         ^ diagnostic.message);
      if
        diagnostic.notes
        <> [ "while instantiating `Recursive[u8]` at regression.fas:2:24" ]
      then
        failwith
          ("generic-instantiation-recursive-struct: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-recursive-struct: expected one diagnostic");
  let nested_struct_argument_failure =
    "struct Box[T] { value T }\n\
     fn bad[T](value T) T { return value + value }\n\
     fn main(value Box[Box[u8]]) Box[Box[u8]] {\n\
     return bad[Box[Box[u8]]](value)\n\
     }\n"
  in
  (match semantic_diagnostics nested_struct_argument_failure with
  | [ diagnostic ] ->
      let rendered = Diag.render_all ~source:None [ diagnostic ] in
      if
        diagnostic.notes
        <> [ "while instantiating `bad[Box[Box[u8]]]` at regression.fas:4:11" ]
      then
        failwith
          ("generic-instantiation-nested-struct: unexpected trace: "
          ^ String.concat " | " diagnostic.notes);
      if contains rendered "$spec$" then
        failwith "generic-instantiation-nested-struct: internal name leaked"
  | _ -> failwith "generic-instantiation-nested-struct: expected one diagnostic");
  let specialized_type_message_failure =
    "struct Box[T] { value T }\n\
     fn bad[T](value T) i64 {\n\
     local Box[u8] = value\n\
     return 0\n\
     }\n\
     fn main(value Box[Box[u8]]) i64 {\n\
     return bad[Box[Box[u8]]](value)\n\
     }\n"
  in
  (match semantic_diagnostics specialized_type_message_failure with
  | [ diagnostic ] ->
      let rendered = Diag.render_all ~source:None [ diagnostic ] in
      if diagnostic.message <> "type mismatch: expected Box[u8], got Box[Box[u8]]" then
        failwith
          ("generic-instantiation-specialized-type-message: unexpected message: "
         ^ diagnostic.message);
      if contains rendered "$spec$" then
        failwith "generic-instantiation-specialized-type-message: internal name leaked"
  | _ ->
      failwith "generic-instantiation-specialized-type-message: expected one diagnostic");
  let repeated_generic_failure =
    "fn bad[T](value T) T { return value + value }\n\
     fn main(value ptr[u8]) ptr[u8] {\n\
     first ptr[u8] = bad[ptr[u8]](value)\n\
     return bad[ptr[u8]](first)\n\
     }\n"
  in
  (match semantic_diagnostics repeated_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes
        <> [ "while instantiating `bad[ptr[u8]]` at regression.fas:3:20" ]
      then
        failwith
          ("generic-instantiation-cache: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-cache: expected one diagnostic");
  let diamond_cache_failure =
    "fn leaf[T](value T) T { return value + value }\n\
     fn left[T](value T) T { return leaf[T](value) }\n\
     fn right[T](value T) T { return leaf[T](value) }\n\
     fn root[T](value T) T {\n\
     first T = left[T](value)\n\
     return right[T](first)\n\
     }\n\
     fn main(value ptr[u8]) ptr[u8] { return root[ptr[u8]](value) }\n"
  in
  (match semantic_diagnostics diamond_cache_failure with
  | [ diagnostic ] ->
      let rendered = Diag.render_all ~source:None [ diagnostic ] in
      if
        diagnostic.notes
        <> [
             "while instantiating `root[ptr[u8]]` at regression.fas:8:45";
             "while instantiating `left[ptr[u8]]` at regression.fas:5:15";
             "while instantiating `leaf[ptr[u8]]` at regression.fas:2:36";
           ]
      then
        failwith
          ("generic-instantiation-diamond-cache: unexpected trace: "
          ^ String.concat " | " diagnostic.notes);
      if contains rendered "$spec$" then
        failwith "generic-instantiation-diamond-cache: internal name leaked"
  | _ -> failwith "generic-instantiation-diamond-cache: expected one diagnostic");
  let render_failure source_text =
    Diag.render_all ~source:None (semantic_diagnostics source_text)
  in
  let deterministic_failure = render_failure nested_generic_failure in
  for _ = 1 to 4 do
    if render_failure nested_generic_failure <> deterministic_failure then
      failwith "generic-instantiation-determinism: rendered output changed"
  done;
  if contains deterministic_failure "$spec$" then
    failwith "generic-instantiation-determinism: internal name leaked";
  let issue45_source =
    "fn broken[N const usize](value i64) i64 {\n\
     if true { return value + bitcast[i64](N) }\n\
     }\n\
     fn main() i64 { return broken[1](0) }\n"
  in
  let issue45_program = expect_ok (Parser.parse (source issue45_source)) in
  (match Sema.check issue45_program with
  | Ok _ -> failwith "const-generic-specialization-span: expected missing-return error"
  | Error diagnostics ->
      let diagnostic = List.hd diagnostics in
      let rendered =
        Diag.render_all ~source:(Some (source issue45_source)) diagnostics
      in
      if
        diagnostic.primary.Span.file <> "regression.fas"
        || diagnostic.primary.Span.line <> 1
        || diagnostic.primary.Span.column <> 1
      then
        failwith
          ("const-generic-specialization-span: unexpected location: "
          ^ Span.to_string diagnostic.primary);
      if
        (not (contains rendered "specialized function `broken`"))
        || (not (contains rendered "fn broken[N const usize](value i64) i64 {"))
        || (not (contains rendered "while instantiating `broken[1]`"))
        || contains rendered "$spec$"
      then
        failwith
          ("const-generic-specialization-span: missing source excerpt: " ^ rendered));
  semantic_error "generic-function-arity" "wrong number of type arguments to `pair`"
    "fn pair[A, B](value A) A { return value }\nfn main() i64 { return pair[i64](1) }\n";
  semantic_error "generic-function-argument-kind" "expected a type argument"
    "fn identity[T](value T) T { return value }\n\
     fn main() i64 { return identity[3](1) }\n";
  semantic_error "generic-call-to-concrete" "function `identity` is not generic"
    "fn identity(value i64) i64 { return value }\n\
     fn main() i64 { return identity[i64](1) }\n";
  semantic_error "unknown-generic-function" "unknown generic function `identity`"
    "fn main() i64 { return identity[i64](1) }\n";
  semantic_error "generic-function-missing-type-arguments"
    "generic function `identity` requires arguments"
    "fn identity[T](value T) T { return value }\nfn main() i64 { return identity(1) }\n";
  semantic_error "generic-function-unknown-type-argument" "unknown type `Missing`"
    "fn ignore[T]() i64 { return 7 }\nfn main() i64 { return ignore[Missing]() }\n";
  semantic_error "generic-name-as-value" "`f` is a function, not a value"
    "fn f[T]() usize { return 0 }\nfn main() usize { return f }\n";
  semantic_error "generic-specialization-as-value" "function `f` is not a place"
    "fn f[T]() usize { return 0 }\nfn main() usize { return f[i64] }\n";
  ignore
    (expect_ok
       (Sema.check
          (expect_ok
             (Parser.parse
                (source
                   "fn inner[T, M const usize](x T) u64 { return bitcast[u64](M) }\n\
                    fn outer[N const usize](x i64) u64 { return inner[i64, 3](x) }\n\
                    fn main() u64 { return outer[5](40) }\n")))));
  semantic_error "extern-c-type-parameter"
    "extern \"C\" functions cannot have type parameters"
    "extern \"C\" { fn identity[T](value T) T }\n";
  semantic_error "generic-main" "entry point `main` cannot have generic parameters"
    "fn main[T]() i64 { return 42 }\n";
  let forward_constant_use =
    ( "use.fas",
      "const SIZE usize = LATER_SIZE\n\
       const FLAG bool = LATER_FLAG\n\
       fn main() usize { return add[SIZE](choose[FLAG]()) }\n" )
  in
  let forward_constant_declarations =
    ( "declarations.fas",
      "const LATER_SIZE usize = 3\n\
       const LATER_FLAG bool = true\n\
       fn add[N const usize](value usize) usize { return value + N }\n\
       fn choose[Flag const bool]() usize {\n\
      \ if Flag { return 4 } else { return 0 }\n\
      \ }\n" )
  in
  List.iter
    (fun files -> ignore (expect_ok (check_files files) |> Lower.lower |> expect_ok))
    [
      [ forward_constant_use; forward_constant_declarations ];
      [ forward_constant_declarations; forward_constant_use ];
    ];
  let cross_file_generic_decls =
    ("decls.fas", "fn add[N const usize](value usize) usize { return value + N }\n")
  in
  let cross_file_generic_use_a =
    ("use_a.fas", "fn main() usize { return add[3](1) + add[3](2) }\n")
  in
  let cross_file_generic_use_b =
    ("use_b.fas", "fn other() usize { return add[4](3) + add[3](4) }\n")
  in
  List.iter
    (fun files ->
      let hir = expect_ok (check_files files) in
      let specialized =
        List.filter (fun (func : Hir.func) -> contains func.name "$spec$") hir.Hir.funcs
      in
      if List.length specialized <> 2 then
        failwith
          "cross-file-generic-reuse: canonical specializations were not reused across \
           files";
      if
        List.length
          (List.filter
             (fun (func : Hir.func) -> String.ends_with ~suffix:"N=usize:3" func.name)
             hir.Hir.funcs)
        <> 1
      then failwith "cross-file-generic-reuse: duplicate add[3] specialization";
      if
        List.length
          (List.filter
             (fun (func : Hir.func) -> String.ends_with ~suffix:"N=usize:4" func.name)
             hir.Hir.funcs)
        <> 1
      then failwith "cross-file-generic-reuse: missing add[4] specialization";
      ignore (Lower.lower hir |> expect_ok))
    [
      [ cross_file_generic_decls; cross_file_generic_use_a; cross_file_generic_use_b ];
      [ cross_file_generic_decls; cross_file_generic_use_b; cross_file_generic_use_a ];
      [ cross_file_generic_use_a; cross_file_generic_decls; cross_file_generic_use_b ];
      [ cross_file_generic_use_a; cross_file_generic_use_b; cross_file_generic_decls ];
      [ cross_file_generic_use_b; cross_file_generic_decls; cross_file_generic_use_a ];
      [ cross_file_generic_use_b; cross_file_generic_use_a; cross_file_generic_decls ];
    ];
  (match
     check_files
       [
         ("dup_a.fas", "fn add[N const usize](value usize) usize { return value + N }\n");
         ("dup_b.fas", "fn add[N const usize](value usize) usize { return value + N }\n");
       ]
   with
  | Ok _ -> failwith "cross-file-duplicate-template: duplicate template accepted"
  | Error diagnostics ->
      let rendered = Diag.render_all ~source:None diagnostics in
      if not (contains rendered "duplicate function `add`") then
        failwith ("cross-file-duplicate-template: unexpected diagnostic: " ^ rendered));
  ignore
    (llvm_of
       "const NARROW u8 = trunc[u8](WIDE)\n\
        const WIDE u16 = 7\n\
        fn value[N const u8]() u8 { return N }\n\
        fn main() u8 { return value[NARROW]() }\n");
  semantic_error "forward-constant-type-preservation"
    "constant initializer type mismatch" "const NARROW u8 = WIDE\nconst WIDE u16 = 7\n";
  semantic_error "forward-constant-cycle" "cyclic constant dependency"
    "const LEFT usize = RIGHT\nconst RIGHT usize = LEFT\n";
  let forward_array_scalar =
    llvm_of
      "const A arr[2, i64] = {B, 0}\nconst B i64 = 1\nfn main() i64 { return A[0] }\n"
  in
  if not (contains forward_array_scalar "[2 x i64] [i64 1, i64 0]") then
    failwith "forward-array-scalar: order-independent scalar did not resolve into array";
  semantic_error "forward-array-dependency-rejected"
    "expression is not compile-time constant"
    "const A arr[2, i64] = {H[0], 0}\n\
     const H arr[2, i64] = {1, 2}\n\
     fn main() i64 { return A[0] }\n";
  ignore
    (llvm_of
       "const LEFT bool = false && RIGHT\n\
        const RIGHT bool = LEFT\n\
        fn main() bool { return RIGHT }\n");
  let mixed_generic_source =
    "const THREE usize = 3\n\
     struct Box[T] { value T }\n\
     fn stamp[T, N const usize](value T) T { seen usize = N\n\
     return value }\n\
     fn wrap[T, N const usize](value T) Box[T] { seen usize = N\n\
     return (Box[T]){stamp[T, N](value)} }\n\
     fn main() Box[i64] { first Box[i64] = wrap[i64, THREE](7)\n\
     return wrap[i64, THREE](first.value) }\n"
  in
  let mixed_generic_hir =
    expect_ok (Parser.parse (source mixed_generic_source)) |> Sema.check |> expect_ok
  in
  let mixed_specializations =
    List.filter
      (fun (func : Hir.func) -> contains func.name "$spec$")
      mixed_generic_hir.Hir.funcs
  in
  if List.length mixed_specializations <> 2 then
    failwith "mixed-generic-deduplication: expected two concrete functions";
  if
    not
      (List.for_all
         (fun (func : Hir.func) -> List.length (positions func.name "$spec$") = 1)
         mixed_specializations)
  then failwith "mixed-generic-key: final names did not use one canonical key";
  if
    not
      (List.for_all
         (fun (func : Hir.func) ->
           match func.params with [ { ty = Hir.Int Hir.I64; _ } ] -> true | _ -> false)
         mixed_specializations)
  then failwith "mixed-generic-substitution: type argument was not substituted";
  if
    List.length
      (List.filter
         (fun (definition : Hir.struct_def) -> contains definition.name "Box$spec$")
         mixed_generic_hir.Hir.structs)
    <> 1
  then failwith "mixed-generic-struct-use: concrete struct was not materialized";
  let mixed_generic_llvm = Ir.render (expect_ok (Lower.lower mixed_generic_hir)) in
  if mixed_generic_llvm <> llvm_of mixed_generic_source then
    failwith "mixed-generic-order: generated output was not deterministic";
  if contains mixed_generic_llvm "@stamp(" || contains mixed_generic_llvm "@wrap(" then
    failwith "mixed-generic-template: template reached LLVM output";
  if not (contains mixed_generic_llvm "store i64 3") then
    failwith "mixed-generic-const-substitution: const value did not reach the body";
  let mixed_layout_argument =
    llvm_of
      "struct Sized[T] { value T }\n\
       fn size[T, N const usize]() usize { return N }\n\
       fn main() usize { return size[Sized[i64], sizeof[Sized[i64]]]() }\n"
  in
  if not (contains mixed_layout_argument "ret i64 8\n") then
    failwith
      "mixed-generic-layout-argument: layout was not available to const evaluation";
  let interleaved_generic =
    expect_ok
      (Sema.check
         (expect_ok
            (Parser.parse
               (source
                  "fn sum[A, N const usize, B, M const usize](left A, right B) usize {\n\
                   return N + M }\n\
                   fn main() usize { return sum[i64, 2, u8, 3](7, 1) }\n"))))
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) -> contains func.name "sum$spec$")
         interleaved_generic.Hir.funcs)
    <> 1
  then failwith "mixed-generic-ordering: interleaved parameters were not preserved";
  let distinct_mixed_specializations =
    expect_ok
      (Sema.check
         (expect_ok
            (Parser.parse
               (source
                  "fn value[T, N const usize](input T) usize { return N }\n\
                   fn main() usize { return value[i64, 1](7) + value[i64, 2](7) }\n"))))
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) -> contains func.name "value$spec$")
         distinct_mixed_specializations.Hir.funcs)
    <> 2
  then failwith "mixed-generic-key: distinct const arguments shared a specialization";
  let mixed_specialization_limits = { Limits.default with max_specializations = 2 } in
  ignore
    (expect_ok
       (Sema.check ~limits:mixed_specialization_limits
          (expect_ok
             (Parser.parse
                (source
                   "fn value[T, N const usize](input T) usize { return N }\n\
                    fn main() usize { return value[i64, 1](7) + value[i64, 1](7) }\n")))));
  (match
     Sema.check ~limits:mixed_specialization_limits
       (expect_ok
          (Parser.parse
             (source
                "fn value[T, N const usize](input T) usize { return N }\n\
                 fn main() usize { return value[i64, 1](7) + value[i64, 2](7) }\n")))
   with
  | Ok _ -> failwith "mixed-generic-count-limit: expected rejection"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "const specialization count limit exceeded")
      then failwith "mixed-generic-count-limit: unexpected diagnostic");
  semantic_error "mixed-generic-arity" "wrong number of generic arguments to `identity`"
    "fn identity[T, N const usize](value T) T { return value }\n\
     fn main() i64 { return identity[i64](1) }\n";
  semantic_error "mixed-generic-type-argument-kind" "expected a type argument"
    "fn identity[T, N const usize](value T) T { return value }\n\
     fn main() i64 { return identity[3, 1](1) }\n";
  semantic_error "mixed-generic-const-argument-kind" "expected a const argument"
    "fn identity[T, N const usize](value T) T { return value }\n\
     fn main() i64 { return identity[i64, u8](1) }\n";
  semantic_error "mixed-generic-instantiated-body-error" "arithmetic requires"
    "fn bad[T, N const usize](value T) T { seen usize = N\n\
     return value + value }\n\
     fn main(value ptr[u8]) ptr[u8] { return bad[ptr[u8], 1](value) }\n";
  let direct_recursive_limits = { Limits.default with max_specialization_depth = 1 } in
  let direct_recursive_specialization =
    expect_ok
      (Sema.check ~limits:direct_recursive_limits
         (expect_ok
            (Parser.parse
               (source
                  "fn recurse[T](value T, count u8) T {\n\
                   if count == 0 { return value }\n\
                   return recurse[T](value, count - 1) }\n\
                   fn main() u8 { return recurse[u8](7, 2) }\n"))))
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) -> contains func.name "recurse$spec$")
         direct_recursive_specialization.Hir.funcs)
    <> 1
  then failwith "generic-function-recursion: expected one concrete function";
  ignore (expect_ok (Lower.lower direct_recursive_specialization));
  let mutual_recursive_limits = { Limits.default with max_specialization_depth = 2 } in
  let mutual_recursive_specializations =
    expect_ok
      (Sema.check ~limits:mutual_recursive_limits
         (expect_ok
            (Parser.parse
               (source
                  "fn left[T](value T, count u8) T {\n\
                   if count == 0 { return value }\n\
                   return right[T](value, count - 1) }\n\
                   fn right[T](value T, count u8) T {\n\
                   if count == 0 { return value }\n\
                   return left[T](value, count - 1) }\n\
                   fn main() u8 { return left[u8](7, 2) }\n"))))
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) ->
           contains func.name "left$spec$" || contains func.name "right$spec$")
         mutual_recursive_specializations.Hir.funcs)
    <> 2
  then failwith "generic-function-mutual-recursion: expected two concrete functions";
  ignore (expect_ok (Lower.lower mutual_recursive_specializations));
  let recursive_function_limits =
    { Limits.default with max_specialization_depth = 2 }
  in
  (match
     Sema.check ~limits:recursive_function_limits
       (expect_ok
          (Parser.parse
             (source
                "fn grow[T]() i64 { return grow[ptr[T]]() }\n\
                 fn main() i64 { return grow[u8]() }\n")))
   with
  | Ok _ -> failwith "generic-function-depth-limit: expected rejection"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "function specialization recursion depth limit exceeded")
      then failwith "generic-function-depth-limit: unexpected diagnostic");
  let one_function_limit = { Limits.default with max_specializations = 1 } in
  ignore
    (expect_ok
       (Sema.check ~limits:one_function_limit
          (expect_ok
             (Parser.parse
                (source
                   "fn identity[T](value T) T { return value }\n\
                    fn main() i64 { return identity[i64](identity[i64](1)) }\n")))));
  let canonical_type_specialization =
    expect_ok
      (Sema.check ~limits:one_function_limit
         (expect_ok
            (Parser.parse
               (source
                  "fn identity[T](value T) T { return value }\n\
                   fn main(left arr[1, u8], right arr[01, u8]) arr[1, u8] {\n\
                   first arr[1, u8] = identity[arr[01, u8]](right)\n\
                   return identity[arr[1, u8]](first) }\n"))))
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) -> contains func.name "identity$spec$")
         canonical_type_specialization.Hir.funcs)
    <> 1
  then failwith "generic-function-canonical-key: expected one concrete function";
  (match
     Sema.check ~limits:one_function_limit
       (expect_ok
          (Parser.parse
             (source
                "fn identity[T](value T) T { return value }\n\
                 fn main() i64 { a u8 = identity[u8](1)\n\
                 return identity[i64](1) }\n")))
   with
  | Ok _ -> failwith "generic-function-count-limit: expected rejection"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "function specialization count limit exceeded")
      then failwith "generic-function-count-limit: unexpected diagnostic");

  let generic_struct_source =
    "struct Box[T] { value T pointer ptr[T] }\n\
     struct Pair[A, B] { first A second B }\n\
     struct Wrapper[T] { boxed Box[T] }\n\
     fn main() usize {\n\
    \ box Box[i64] = (Box[i64]){7, null}\n\
    \ pair64 Pair[i64, u8] = (Pair[i64, u8]){9, 1}\n\
    \ pair32 Pair[u32, u8] = (Pair[u32, u8]){9, 1}\n\
    \ wrapped Wrapper[u8] = (Wrapper[u8]){(Box[u8]){1, null}}\n\
    \ return sizeof[Box[i64]] + sizeof[Pair[i64, u8]]\n\
     }\n"
  in
  let generic_struct_hir =
    expect_ok (Parser.parse (source generic_struct_source)) |> Sema.check |> expect_ok
  in
  let specialization_named base (definition : Hir.struct_def) =
    let prefix = base ^ "$spec$" in
    String.length definition.name >= String.length prefix
    && String.sub definition.name 0 (String.length prefix) = prefix
  in
  let box_specializations =
    List.filter (specialization_named "Box") generic_struct_hir.Hir.structs
  in
  if List.length box_specializations <> 2 then
    failwith "generic-struct-deduplication: expected two concrete Box layouts";
  if
    not
      (List.exists
         (fun (definition : Hir.struct_def) ->
           match definition.fields with
           | [ { ty = Hir.Int Hir.I64; _ }; { ty = Hir.Ptr (Hir.Int Hir.I64); _ } ] ->
               true
           | _ -> false)
         box_specializations)
  then failwith "generic-struct-substitution: nested pointer substitution failed";
  let pair_sizes =
    generic_struct_hir.Hir.structs
    |> List.filter (specialization_named "Pair")
    |> List.map (fun (definition : Hir.struct_def) -> definition.size)
    |> List.sort compare
  in
  if pair_sizes <> [ 8; 16 ] then
    failwith
      "generic-struct-layouts: distinct arguments did not produce distinct layouts";
  let generic_struct_llvm = Ir.render (expect_ok (Lower.lower generic_struct_hir)) in
  if generic_struct_llvm <> llvm_of generic_struct_source then
    failwith "generic-struct-order: generated output was not deterministic";
  if contains generic_struct_llvm "%struct.Box = type" then
    failwith "generic-struct-template: template reached LLVM output";
  if not (contains generic_struct_llvm "%struct.Box$spec$") then
    failwith "generic-struct-lowering: concrete specialization did not reach LLVM";
  let unused_generic_struct =
    expect_ok
      (Sema.check
         (expect_ok
            (Parser.parse
               (source "struct Unused[T] { value T }\nfn main() i64 { return 0 }\n"))))
  in
  if unused_generic_struct.Hir.structs <> [] then
    failwith "generic-struct-template: unused template was emitted";
  semantic_error "generic-struct-invalid-alignment"
    "alignment must be a positive power of two"
    "struct Bad[T] @align(3) { value T }\nfn main() i64 { return 0 }\n";
  semantic_error "generic-struct-bare-use"
    "generic struct `Box` requires type arguments"
    "struct Box[T] { value T }\nfn main(value Box) i64 { return 0 }\n";
  semantic_error "generic-struct-arity" "wrong number of generic arguments to `Pair`"
    "struct Pair[A, B] { first A second B }\n\
     fn main(value Pair[i64]) i64 { return 0 }\n";
  semantic_error "generic-struct-argument-kind" "expected a type argument"
    "struct Box[T] { value T }\nfn main(value Box[3]) i64 { return 0 }\n";
  semantic_error "generic-struct-aggregate-limit"
    "aggregate element count exceeds the configured limit"
    "struct Box[T] { value T }\n\
     fn consume(value Box[arr[1000001,u8]]) i32 { return 0 }\n\
     fn main() i32 { return 0 }\n";
  semantic_error "generic-application-to-concrete" "struct `Box` is not generic"
    "struct Box { value i64 }\nfn main(value Box[i64]) i64 { return 0 }\n";
  semantic_error "unknown-generic-struct" "unknown generic struct `Missing`"
    "fn main(value Missing[i64]) i64 { return 0 }\n";
  semantic_error "generic-struct-duplicate-param" "duplicate generic parameter `T`"
    "struct Pair[T, T] { first T second T }\nfn main() i64 { return 0 }\n";

  let const_generic_struct_source =
    "struct Buffer[T, N const usize] { data arr[N, T] }\n\
     struct Bytes[N const usize] { data arr[N, u8] }\n\
     struct Lanes[T, N const usize] { data vec[N, T] }\n\
     struct Wrapped[T, N const usize] { value Buffer[T, N] }\n\
     fn main() usize {\n\
    \ return sizeof[Buffer[u8, 3]] + sizeof[Buffer[u8, 1 + 2]] + "
    ^ "sizeof[Buffer[u16, 3]] + sizeof[Buffer[u8, 4]] + "
    ^ "sizeof[Bytes[5]] + sizeof[Lanes[u32, 4]] + sizeof[Wrapped[u8, 3]]\n}\n"
  in
  let const_generic_struct_hir =
    expect_ok (Parser.parse (source const_generic_struct_source))
    |> Sema.check |> expect_ok
  in
  let buffer_specializations =
    List.filter (specialization_named "Buffer") const_generic_struct_hir.Hir.structs
  in
  if List.length buffer_specializations <> 3 then
    failwith "const-generic-struct-deduplication: expected three concrete layouts";
  if
    not
      (List.exists
         (fun (definition : Hir.struct_def) ->
           match definition.fields with
           | [ { ty = Hir.Array (3, Hir.Int Hir.U8); _ } ] -> true
           | _ -> false)
         buffer_specializations)
  then failwith "const-generic-struct-substitution: array length was not substituted";
  if
    not
      (List.exists
         (fun (definition : Hir.struct_def) ->
           specialization_named "Bytes" definition
           &&
           match definition.fields with
           | [ { ty = Hir.Array (5, Hir.Int Hir.U8); _ } ] -> true
           | _ -> false)
         const_generic_struct_hir.Hir.structs)
  then
    failwith "const-generic-struct-substitution: const-only layout was not materialized";
  if
    not
      (List.exists
         (fun (definition : Hir.struct_def) ->
           specialization_named "Lanes" definition
           &&
           match definition.fields with
           | [ { ty = Hir.Vec (4, Hir.Int Hir.U32); _ } ] -> true
           | _ -> false)
         const_generic_struct_hir.Hir.structs)
  then failwith "const-generic-struct-substitution: vector length was not substituted";
  if
    not
      (List.exists
         (fun (definition : Hir.struct_def) ->
           specialization_named "Wrapped" definition
           &&
           match definition.fields with
           | [ { ty = Hir.Struct name; _ } ] -> contains name "Buffer$spec$"
           | _ -> false)
         const_generic_struct_hir.Hir.structs)
  then failwith "const-generic-struct-nesting: outer const value was not forwarded";
  let const_generic_struct_llvm =
    Ir.render (expect_ok (Lower.lower const_generic_struct_hir))
  in
  if const_generic_struct_llvm <> llvm_of const_generic_struct_source then
    failwith "const-generic-struct-order: generated output was not deterministic";
  if
    contains const_generic_struct_llvm "%struct.Buffer = type"
    || contains const_generic_struct_llvm "%struct.Bytes = type"
    || contains const_generic_struct_llvm "%struct.Lanes = type"
    || contains const_generic_struct_llvm "%struct.Wrapped = type"
  then failwith "const-generic-struct-template: template reached LLVM output";
  let bool_const_generic_struct_hir =
    expect_ok
      (Parser.parse
         (source
            "struct Tagged[Flag const bool] { value u8 }\n\
             fn main() usize {\n\
            \ return sizeof[Tagged[true]] + sizeof[Tagged[false]] +sizeof[Tagged[1]] + \
             sizeof[Tagged[trunc[bool](3)]]\n\
             }\n"))
    |> Sema.check |> expect_ok
  in
  let tagged_specializations =
    List.filter
      (specialization_named "Tagged")
      bool_const_generic_struct_hir.Hir.structs
  in
  if List.length tagged_specializations <> 2 then
    failwith "const-generic-struct-bool: expected two typed specializations";
  let issue33_llvm =
    llvm_of
      "struct Bytes[N const usize] { data arr[N, u8] }\n\
       fn identity(value Bytes[3]) Bytes[3] { return value }\n\
       fn main() usize { return sizeof[Bytes[3]] }\n"
  in
  if not (contains issue33_llvm "%\"struct.Bytes$spec$c7:usize:3\" = type { [3 x i8] }")
  then failwith "const-generic-struct-llvm-name: specialization name was not quoted";
  if
    not
      (contains issue33_llvm
         "define internal %\"struct.Bytes$spec$c7:usize:3\" @identity")
  then failwith "const-generic-struct-llvm-use: specialization type was not quoted";
  semantic_error "const-generic-struct-arity"
    "wrong number of generic arguments to `Buffer`"
    "struct Buffer[T, N const usize] { data arr[N, T] }\n\
     fn main(value Buffer[u8]) i64 { return 0 }\n";
  semantic_error "const-generic-struct-argument-kind" "expected a const argument"
    "struct Buffer[T, N const usize] { data arr[N, T] }\n\
     fn main(value Buffer[u8, u16]) i64 { return 0 }\n";
  semantic_error "const-generic-struct-argument-type" "const argument type mismatch"
    "struct Buffer[T, N const u8] { data arr[N, T] }\n\
     fn main(value Buffer[u8, sizeof[u8]]) i64 { return 0 }\n";
  semantic_error "const-generic-struct-negative-length" "negative aggregate length"
    "struct Buffer[T, N const isize] { data arr[N, T] }\n\
     fn main(value Buffer[u8, -1]) i64 { return 0 }\n";

  let global_const_generic_struct_source =
    "struct Unit { value u64 }\n\
     struct Box[T] { value T }\n\
     const THREE usize = 3\n\
     const BOX_BYTES usize = sizeof[Box[u16]]\n\
     struct Buffer[T, N const usize] { data arr[N, T] }\n\
     struct Holder { value Buffer[u8, THREE] }\n\
     fn fixed[T](value Buffer[T, THREE]) Buffer[T, THREE] { return value }\n\
     fn main(three Buffer[u8, THREE], boxed Buffer[u8, BOX_BYTES],\n\
     unit Buffer[u8, sizeof[Unit]]) usize {\n\
    \ fixed[u8](three)\n\
    \ return sizeof[Holder] + sizeof[Buffer[u8, BOX_BYTES]] + sizeof[Buffer[u8, \
     sizeof[Unit]]]\n\
     }\n"
  in
  let global_const_generic_struct_hir =
    expect_ok (Parser.parse (source global_const_generic_struct_source))
    |> Sema.check |> expect_ok
  in
  if
    List.length
      (List.filter
         (specialization_named "Buffer")
         global_const_generic_struct_hir.Hir.structs)
    <> 3
  then
    failwith "const-generic-struct-global-const: expected three concrete Buffer layouts";
  if
    not
      (List.exists
         (fun (definition : Hir.struct_def) ->
           definition.name = "Holder"
           &&
           match definition.fields with
           | [ { ty = Hir.Struct name; _ } ] -> contains name "Buffer$spec$"
           | _ -> false)
         global_const_generic_struct_hir.Hir.structs)
  then
    failwith "const-generic-struct-global-field: named const did not resolve in a field";
  let global_const_generic_struct_llvm =
    Ir.render (expect_ok (Lower.lower global_const_generic_struct_hir))
  in
  if global_const_generic_struct_llvm <> llvm_of global_const_generic_struct_source then
    failwith "const-generic-struct-global-order: generated output was not deterministic";
  semantic_error "const-generic-struct-global-type" "const argument type mismatch"
    "const COUNT u8 = 3\n\
     struct Buffer[T, N const usize] { data arr[N, T] }\n\
     fn main(value Buffer[u8, COUNT]) usize { return 0 }\n";

  let const_generic_struct_type_argument_source =
    "const THREE usize = 3\n\
     const FOUR usize = 4\n\
     struct Buffer[T, N const usize] { data arr[N, T] }\n\
     fn identity[T](value T) T { return value }\n\
     fn forward[T](value T) T { return identity[T](value) }\n\
     fn main(three Buffer[u8, THREE], four Buffer[u8, FOUR]) usize {\n\
    \ a Buffer[u8, THREE] = identity[Buffer[u8, THREE]](three)\n\
    \ b Buffer[u8, FOUR] = identity[Buffer[u8, FOUR]](four)\n\
    \ forward[Buffer[u8, THREE]](a)\n\
    \ forward[Buffer[u8, FOUR]](b)\n\
    \ return sizeof[Buffer[u8, THREE]] + sizeof[Buffer[u8, FOUR]]\n\
     }\n"
  in
  let const_generic_struct_type_argument_hir =
    expect_ok (Parser.parse (source const_generic_struct_type_argument_source))
    |> Sema.check |> expect_ok
  in
  let specialized_functions name =
    List.filter
      (fun (func : Hir.func) -> contains func.name (name ^ "$spec$"))
      const_generic_struct_type_argument_hir.Hir.funcs
  in
  if List.length (specialized_functions "identity") <> 2 then
    failwith "const-generic-struct-type-key: distinct identity types were merged";
  if List.length (specialized_functions "forward") <> 2 then
    failwith "const-generic-struct-type-key: nested specializations were merged";
  if
    List.length
      (List.filter
         (specialization_named "Buffer")
         const_generic_struct_type_argument_hir.Hir.structs)
    <> 2
  then failwith "const-generic-struct-type-key: distinct layouts were merged";
  let const_generic_struct_type_argument_llvm =
    Ir.render (expect_ok (Lower.lower const_generic_struct_type_argument_hir))
  in
  if
    const_generic_struct_type_argument_llvm
    <> llvm_of const_generic_struct_type_argument_source
  then failwith "const-generic-struct-type-key: generated output was not deterministic";

  let deferred_nested_struct_source =
    "struct Inner[N const usize] { data arr[N, u8] }\n\
     struct Outer[T] { value T }\n\
     fn pass[T, N const usize](value Outer[Inner[N]]) Outer[Inner[N]] {\n\
     return value\n\
     }\n\
     fn main(value Outer[Inner[1]]) Outer[Inner[1]] {\n\
     return pass[u8, 1](value)\n\
     }\n"
  in
  let deferred_nested_struct_hir =
    expect_ok (Parser.parse (source deferred_nested_struct_source))
    |> Sema.check |> expect_ok
  in
  if
    List.length
      (List.filter
         (specialization_named "Inner")
         deferred_nested_struct_hir.Hir.structs)
    <> 1
  then failwith "nested-const-struct-deferral: expected one concrete inner layout";
  if
    List.length
      (List.filter
         (specialization_named "Outer")
         deferred_nested_struct_hir.Hir.structs)
    <> 1
  then failwith "nested-const-struct-deferral: expected one concrete outer layout";
  let deferred_nested_struct_llvm =
    Ir.render (expect_ok (Lower.lower deferred_nested_struct_hir))
  in
  if
    contains deferred_nested_struct_llvm "Inner["
    || contains deferred_nested_struct_llvm "Outer["
  then failwith "nested-const-struct-deferral: unresolved type reached LLVM";

  let const_generic_function_type_source =
    "const THREE usize = 3\n\
     fn array_identity[T, N const usize](value arr[N, T]) arr[N, T] {\n\
    \ result arr[N, T] = value\n\
    \ return result\n\
     }\n\
     fn array_outer[T, N const usize](value arr[N, T]) arr[N, T] {\n\
    \ return array_identity[T, N](value)\n\
     }\n\
     fn byte_identity[N const usize](value arr[N, u8]) arr[N, u8] {\n\
    \ return value\n\
     }\n\
     fn vector_identity[T, N const usize](value vec[N, T]) vec[N, T] {\n\
    \ return value\n\
     }\n\
     fn aggregate_metrics[T, N const usize](value ptr[arr[N, T]]) usize {\n\
    \ same ptr[arr[N, T]] = bitcast[ptr[arr[N, T]]](value)\n\
    \ return sizeof[arr[N, T]] + alignof[arr[N, T]] + sizeof[vec[N, T]]\n\
     }\n\
     fn main(value arr[3, u8], pointer ptr[arr[4, u16]], lanes vec[4, u16]) usize {\n\
    \ first arr[3, u8] = array_outer[u8, THREE](value)\n\
    \ second arr[3, u8] = array_outer[u8, 3](first)\n\
    \ third arr[3, u8] = byte_identity[THREE](second)\n\
    \ same_lanes vec[4, u16] = vector_identity[u16, 4](lanes)\n\
    \ return len(third) + sizeof[vec[4, u16]] + aggregate_metrics[u16, 4](pointer)\n\
     }\n"
  in
  let const_generic_function_type_hir =
    expect_ok (Parser.parse (source const_generic_function_type_source))
    |> Sema.check |> expect_ok
  in
  let function_specializations =
    List.filter
      (fun (func : Hir.func) -> contains func.name "$spec$")
      const_generic_function_type_hir.Hir.funcs
  in
  if List.length function_specializations <> 5 then
    failwith "const-generic-function-type-deduplication: expected five functions";
  if
    not
      (List.exists
         (fun (func : Hir.func) ->
           contains func.name "array_identity$spec$"
           &&
           match (func.params, func.ret) with
           | [ { ty = Hir.Array (3, Hir.Int Hir.U8); _ } ], Hir.Array (3, Hir.Int Hir.U8)
             ->
               true
           | _ -> false)
         function_specializations)
  then failwith "const-generic-function-type-signature: length was not substituted";
  if
    not
      (List.exists
         (fun (func : Hir.func) ->
           contains func.name "aggregate_metrics$spec$"
           &&
           match func.params with
           | [ { ty = Hir.Ptr (Hir.Array (4, Hir.Int Hir.U16)); _ } ] -> true
           | _ -> false)
         function_specializations)
  then
    failwith "const-generic-function-type-nesting: pointer length was not substituted";
  let const_generic_function_type_llvm =
    Ir.render (expect_ok (Lower.lower const_generic_function_type_hir))
  in
  if const_generic_function_type_llvm <> llvm_of const_generic_function_type_source then
    failwith "const-generic-function-type-order: generated output was not deterministic";
  if
    contains const_generic_function_type_llvm "@array_identity("
    || contains const_generic_function_type_llvm "@array_outer("
    || contains const_generic_function_type_llvm "@byte_identity("
    || contains const_generic_function_type_llvm "@vector_identity("
    || contains const_generic_function_type_llvm "@aggregate_metrics("
  then failwith "const-generic-function-type-template: template reached LLVM output";
  semantic_error "const-generic-function-type-mismatch"
    "type mismatch: expected arr[4, u8], got arr[3, u8]"
    "fn identity[N const usize](value arr[N, u8]) arr[N, u8] { return value }\n\
     fn main(value arr[3, u8]) arr[4, u8] { return identity[4](value) }\n";
  semantic_error "const-generic-function-negative-length" "negative aggregate length"
    "fn identity[N const isize](value arr[N, u8]) arr[N, u8] { return value }\n\
     fn main(value arr[1, u8]) arr[1, u8] { return identity[-1](value) }\n";
  semantic_error "const-generic-function-machine-length"
    "aggregate length is not a machine integer"
    "fn identity[N const u64](value ptr[arr[N, u8]]) ptr[arr[N, u8]] { return value }\n\
     fn main(value ptr[arr[1, u8]]) ptr[arr[1, u8]] { return \
     identity[18446744073709551615](value) }\n";
  let const_array_len_generic_llvm =
    llvm_of
      "const DATA arr[3, u8] = { 10, 20, 30 }\n\
       fn width[N const usize]() usize { return N }\n\
       fn main() usize { return width[len(DATA)]() }\n"
  in
  if not (contains const_array_len_generic_llvm "ret i64 3\n") then
    failwith "const-generic-array-len: array length was not used as a const argument";
  ignore
    (llvm_of
       "fn count[N const i64](value i64) i64 {\n\
       \ if N > 0 { return count[N - 1](value + 1) }\n\
       \ return value\n\
        }\n\
        fn main() i64 { return count[0](41) }\n");
  let runtime_const_generic_if =
    llvm_of
      "fn choose[N const i64](value i64, condition i64) i64 {\n\
      \ if condition > 0 { return value + N } else { return value - N }\n\
       }\n\
       fn main() i64 { return choose[2](40, 0) }\n"
  in
  if not (contains runtime_const_generic_if "br i1") then
    failwith "const-generic-runtime-if: runtime condition was pruned";
  let shadowed_specialization_condition =
    llvm_of
      "const Flag bool = false\n\
       fn choose[N const i32](Flag bool) i32 {\n\
      \ if Flag && true { return 7 }\n\
      \ return 3\n\
       }\n\
       fn main() i32 { return choose[1](true) + choose[1](false) }\n"
  in
  if not (contains shadowed_specialization_condition "br i1") then
    failwith "shadowed-specialization-condition: runtime parameter branch was pruned";
  let const_dependent_if =
    llvm_of
      "fn choose[Flag const i32]() i32 {\n\
      \ if Flag == 1 { return 7 } else { return 3 }\n\
       }\n\
       fn main() i32 { return choose[1]() + choose[0]() }\n"
  in
  if
    (not (contains const_dependent_if "ret i32 7\n"))
    || not (contains const_dependent_if "ret i32 3\n")
  then failwith "const-dependent-if: specialization branches were not selected";
  if contains const_dependent_if "br i1" then
    failwith "const-dependent-if: selected specializations retained runtime branches";
  semantic_error "nondependent-specialization-condition" "unknown name `Missing`"
    "const Flag bool = false\n\
     fn choose[N const i32]() i32 {\n\
    \ if Flag { return Missing }\n\
    \ return 3\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-unknown-value" "unknown name `Missing`"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return Missing }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-unknown-function"
    "unknown function `missing`"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return missing() }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-unknown-generic"
    "unknown generic function `missing`"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return missing[N]() }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-unknown-type" "unknown type `Missing`"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { value Missing\n\
    \ return 3 }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-unknown-target"
    "unknown assignment target `Missing`"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { Missing = 3\n\
    \ return 3 }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-unknown-length" "unknown name `Missing`"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { values arr[Missing,i32]\n\
    \ return 3 }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-type-as-value" "type, not a value"
    "opaque Item\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return Item }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-const-as-function" "value, not a function"
    "const Item i32 = 2\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return Item() }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-local-shadows-function"
    "value, not a function"
    "fn Item() i32 { return 2 }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { Item i32 = 3\n\
    \ return Item() }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-const-target"
    "constant `Item` is not assignable"
    "const Item i32 = 2\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { Item = 3\n\
    \ return Item }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-invalid-operation"
    "arithmetic requires integer or vector operands"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return true + true }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-local-type-mismatch"
    "type mismatch: expected i32, got bool"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { value i32 = true\n\
    \ return value }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-assignment-type-mismatch"
    "type mismatch: expected i32, got bool"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { value i32 = 0\n\
    \ value = true\n\
    \ return value }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-call-type-mismatch"
    "type mismatch: expected i32, got bool"
    "fn plain(value i32) i32 { return value }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain(true) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-dependent-call-sibling"
    "type mismatch: expected i32, got bool"
    "fn plain(left i32, right i32) i32 { return left + right }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain(N, true) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-builtin-type"
    "builtin argument must be an integer"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return popcount(true) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-illegal-cast"
    "illegal cast for source and destination widths"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return zext[u8](256) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-fixed-cast-target"
    "illegal cast target type"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return zext[void](N) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-nongeneric-application"
    "function `plain` is not generic"
    "fn plain(value i32) i32 { return value }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain[N](1) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-wrong-generic-arity"
    "wrong number of generic arguments"
    "fn plain[T](value T) T { return value }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain[i32, i32](1) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-wrong-generic-kind"
    "expected a type argument"
    "fn plain[T](value T) T { return value }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain[1](1) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-generic-call-type"
    "type mismatch: expected i32, got bool"
    "fn plain[T](value T) T { return value }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain[i32](true) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-const-argument-range"
    "integer literal is out of range for u8"
    "fn plain[N const u8]() i32 { return 1 }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain[256]() }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-named-const-argument-type"
    "type mismatch: expected u8, got i32"
    "const Wide i32 = 7\n\
     fn plain[N const u8]() i32 { return 1 }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain[Wide]() }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  ignore
    (llvm_of
       "const Wide i32 = 7\n\
        fn plain[N const u8]() i32 { return zext[i32](N) }\n\
        fn choose[N const i32]() i32 {\n\
       \ if N == 1 { return 7 } else { return plain[trunc[u8](Wide)]() }\n\
        }\n\
        fn main() i32 { return choose[1]() }\n");
  ignore
    (llvm_of
       "fn choose[N const i32](value i32) i32 {\n\
       \ if N == 1 { return value } else { return value + N }\n\
        }\n\
        fn main() i32 { return choose[1](7) }\n");
  semantic_error "unselected-specialization-duplicate-local" "duplicate local `value`"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { value i32 = 1\n\
    \ value i32 = 2\n\
    \ return value }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  ignore
    (llvm_of
       "fn choose[N const i32](value i32) i32 {\n\
       \ prior i32 = value\n\
       \ if N == 1 { return prior } else { result i32 = prior\n\
       \ return result }\n\
        }\n\
        fn main() i32 { return choose[1](7) }\n");
  ignore
    (llvm_of
       "fn choose[N const i32]() i32 {\n\
       \ if N == 1 { return 7 } else { value i32 = 1\n\
       \ { value i32 = 2 }\n\
       \ return value }\n\
        }\n\
        fn main() i32 { return choose[1]() }\n");
  ignore
    (llvm_of
       "const One i32 = 1\n\
        fn leaf[N const i32]() i32 { return N }\n\
        fn choose[N const i32]() i32 {\n\
       \ if N == 1 { return 7 } else { return leaf[One]() }\n\
        }\n\
        fn main() i32 { return choose[1]() }\n");

  let const_generic_struct_function_source =
    "const THREE usize = 3\n\
     struct Unit { value u64 }\n\
     struct Buffer[T, N const usize] { data arr[N, T] }\n\
     struct Wrapped[T, N const usize] { value Buffer[T, N] }\n\
     fn pass[T, N const usize](value Buffer[T, N]) Buffer[T, N] {\n\
    \ result Buffer[T, N] = value\n\
    \ return result\n\
     }\n\
     fn wrap[T, N const usize](value Buffer[T, N]) Wrapped[T, N] {\n\
    \ return (Wrapped[T, N]){pass[T, N](value)}\n\
     }\n\
     fn sized(value Buffer[u8, 8]) Buffer[u8, 8] {\n\
    \ return pass[u8, sizeof[Unit]](value)\n\
     }\n\
     fn main(value Buffer[u8, 3]) Wrapped[u8, 3] {\n\
    \ return wrap[u8, THREE](value)\n\
     }\n"
  in
  let const_generic_struct_function_hir =
    expect_ok (Parser.parse (source const_generic_struct_function_source))
    |> Sema.check |> expect_ok
  in
  if
    List.length
      (List.filter
         (specialization_named "Buffer")
         const_generic_struct_function_hir.Hir.structs)
    <> 2
  then
    failwith "const-generic-struct-function-deduplication: expected two Buffer layouts";
  if
    List.length
      (List.filter
         (specialization_named "Wrapped")
         const_generic_struct_function_hir.Hir.structs)
    <> 1
  then failwith "const-generic-struct-function-nesting: expected one Wrapped layout";
  let struct_function_specializations =
    List.filter
      (fun (func : Hir.func) -> contains func.name "$spec$")
      const_generic_struct_function_hir.Hir.funcs
  in
  if List.length struct_function_specializations <> 3 then
    failwith
      "const-generic-struct-function-discovery: expected three concrete functions";
  if
    not
      (List.for_all
         (fun (func : Hir.func) ->
           match func.params with [ { ty = Hir.Struct _; _ } ] -> true | _ -> false)
         struct_function_specializations)
  then
    failwith
      "const-generic-struct-function-signature: applied type was not materialized";
  let const_generic_struct_function_llvm =
    Ir.render (expect_ok (Lower.lower const_generic_struct_function_hir))
  in
  if const_generic_struct_function_llvm <> llvm_of const_generic_struct_function_source
  then
    failwith
      "const-generic-struct-function-order: generated output was not deterministic";

  let recursive_struct_limits = { Limits.default with max_specialization_depth = 1 } in
  ignore
    (expect_ok
       (Sema.check ~limits:recursive_struct_limits
          (expect_ok
             (Parser.parse
                (source
                   "struct Node[T] { next ptr[Node[T]] value T }\n\
                    fn main(value Node[u8]) i64 { return 0 }\n")))));
  (match
     Sema.check ~limits:recursive_struct_limits
       (expect_ok
          (Parser.parse
             (source
                "struct Inner[T] { value T }\n\
                 struct Outer[T] { inner Inner[T] }\n\
                 fn main(value Outer[u8]) i64 { return 0 }\n")))
   with
  | Ok _ -> failwith "generic-struct-depth-limit: expected rejection"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "struct specialization recursion depth limit exceeded")
      then failwith "generic-struct-depth-limit: unexpected diagnostic");
  let one_struct_limit = { Limits.default with max_specializations = 1 } in
  ignore
    (expect_ok
       (Sema.check ~limits:one_struct_limit
          (expect_ok
             (Parser.parse
                (source
                   "struct Box[T] { value T }\n\
                    fn main(left Box[u8], right Box[u8]) i64 { return 0 }\n")))));
  (match
     Sema.check ~limits:one_struct_limit
       (expect_ok
          (Parser.parse
             (source
                "struct Box[T] { value T }\n\
                 fn id[N const usize]() usize { return N }\n\
                 fn main(value Box[u8]) usize { return id[1]() }\n")))
   with
  | Ok _ -> failwith "shared-specialization-limit: expected rejection"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "const specialization count limit exceeded")
      then failwith "shared-specialization-limit: unexpected diagnostic");

  let i8_min_specialization =
    llvm_of
      "fn b[N const i8](x i8) i32 { return sext[i32](x) + sext[i32](N) }\n\
       fn main() i32 { return b[-128](1) }\n"
  in
  if
    (not (contains i8_min_specialization "add i32"))
    || not (contains i8_min_specialization "sext i8 128 to i32")
  then failwith "const-param-i8-min: i8 minimum constant was not sign-extended";

  let u8_max_specialization =
    llvm_of
      "fn w[N const u8](x u8) i32 { return zext[i32](x) + zext[i32](N) }\n\
       fn main() i32 { return w[255](1) }\n"
  in
  if
    (not (contains u8_max_specialization "add i32"))
    || not (contains u8_max_specialization "zext i8 255 to i32")
  then failwith "const-param-u8-max: u8 maximum constant was not zero-extended";

  semantic_error "const-param-i8-overflow" "integer literal is out of range for i8"
    "fn b[N const i8](x i8) i32 { return sext[i32](x) + sext[i32](N) }\n\
     fn main() i32 { return b[128](1) }\n";

  semantic_error "const-param-u8-overflow" "integer literal is out of range for u8"
    "fn w[N const u8](x u8) i32 { return zext[i32](x) + zext[i32](N) }\n\
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

  lower_struct_error "layout-invariant-overlap" "overlaps a preceding field"
    {
      Hir.name = "Overlap";
      fields =
        [
          { Hir.name = "a"; ty = Hir.Int Hir.U64; offset = 0 };
          { Hir.name = "b"; ty = Hir.Int Hir.U8; offset = 4 };
        ];
      size = 8;
      align = 8;
    };
  lower_struct_error "layout-invariant-size" "size is smaller than its fields"
    {
      Hir.name = "Short";
      fields = [ { Hir.name = "x"; ty = Hir.Int Hir.U64; offset = 0 } ];
      size = 4;
      align = 8;
    };
  lower_struct_error "layout-invariant-alignment"
    "alignment must be a positive power of two"
    {
      Hir.name = "BadAlign";
      fields = [ { Hir.name = "x"; ty = Hir.Int Hir.U8; offset = 0 } ];
      size = 3;
      align = 3;
    };

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
               params = [ { Hir.name = "x"; ty = Hir.Opaque "X"; id = 0 } ];
               ret = Hir.Void;
               body = Hir.Statements [];
               linkage = Hir.Internal;
               variadic = false;
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
         const_arrays = [];
         strings = [];
         funcs =
           [
             {
               Hir.name = "fallthrough";
               params = [];
               ret = Hir.Int Hir.I32;
               body = Hir.Statements [];
               linkage = Hir.Internal;
               variadic = false;
             };
           ];
       }
   with
  | Ok _ -> failwith "lower-fallthrough: malformed non-void HIR lowered"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "non-void function `fallthrough` reached lowering with fallthrough")
      then failwith "lower-fallthrough: unexpected diagnostic");

  let infinite_loop_ir = llvm_of "fn spin() i64 { while true { } }\n" in
  if not (contains infinite_loop_ir "unreachable") then
    failwith "lower-infinite-loop: proven-dead exit was not terminated";

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
               Hir.name = "malformed_switch";
               params = [];
               ret = Hir.Void;
               body =
                 Hir.Statements
                   [
                     Hir.Switch
                       ( Hir.EInt (0L, Hir.Int Hir.I32, Span.synthetic),
                         [ (Hir.EString (0, Span.synthetic), []) ],
                         None,
                         Span.synthetic );
                   ];
               linkage = Hir.Internal;
               variadic = false;
             };
           ];
       }
   with
  | Ok _ -> failwith "lower-switch-case: malformed HIR lowered"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "internal error: non-constant switch case")
      then failwith "lower-switch-case: unexpected diagnostic");

  let malformed_compound_local = { Hir.name = "x"; ty = Hir.Int Hir.I8; id = 0 } in
  lower_function_error "lower-compound-operator"
    "internal error: non-arithmetic operator reached binary lowering"
    [ malformed_compound_local ]
    [
      Hir.Compound_assign
        ( Hir.ALocal malformed_compound_local,
          Ast.And,
          Hir.EInt (1L, Hir.Int Hir.I8, Span.synthetic),
          Hir.Int Hir.I8,
          Span.synthetic );
    ];
  lower_function_error "lower-index-type"
    "internal error: index lowering received a non-integer value" []
    [
      Hir.Expr
        ( Hir.Index
            ( Hir.EVector ([ 7L ], Hir.Vec (1, Hir.Int Hir.I8), Span.synthetic),
              Hir.EBool (true, Span.synthetic),
              Hir.Int Hir.I8,
              Span.synthetic ),
          Span.synthetic );
    ];
  lower_function_error "lower-intrinsic-type"
    "internal error: integer intrinsic has a non-integer type" []
    [
      Hir.Expr
        ( Hir.Call
            ( Hir.Builtin Hir.Popcount,
              [ Hir.EBool (true, Span.synthetic) ],
              Hir.Bool,
              Span.synthetic ),
          Span.synthetic );
    ];
  lower_function_error "lower-complement-type"
    "internal error: bitwise complement has a non-integer type" []
    [
      Hir.Expr
        ( Hir.Unary
            ( Ast.Bit_not,
              Hir.Null (Hir.Ptr (Hir.Int Hir.I8), Span.synthetic),
              Hir.Ptr (Hir.Int Hir.I8),
              Span.synthetic ),
          Span.synthetic );
    ];
  lower_function_error "lower-logical-not-type"
    "internal error: logical not has a non-bool type" []
    [
      Hir.Expr
        ( Hir.Unary
            ( Ast.Not,
              Hir.EInt (1L, Hir.Int Hir.I64, Span.synthetic),
              Hir.Int Hir.I64,
              Span.synthetic ),
          Span.synthetic );
    ];
  lower_function_error "lower-condition-type"
    "internal error: condition lowering received a non-bool value" []
    [
      Hir.If (Hir.EInt (1L, Hir.Int Hir.I64, Span.synthetic), [], None, Span.synthetic);
    ];

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

  lower_function_error "layout-invariant-local" "internal error: type `X` has no layout"
    []
    [ Hir.Let ({ Hir.name = "x"; ty = Hir.Opaque "X"; id = 0 }, None, Span.synthetic) ];
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

  ignore (llvm_of "const Z bool = false && (1 / 0 == 0)\n");

  ignore (llvm_of "fn f() bool { return false && (1 / 0 == 0) }\n");

  semantic_error "const-logical-reaches-right" "division by zero in constant expression"
    "const Z bool = true && (1 / 0 == 0)\n";

  semantic_error "runtime-logical-reaches-right"
    "division by zero is not a defined runtime operation"
    "fn f() bool { return true && (1 / 0 == 0) }\n";

  semantic_error "const-logical-bool-only" "logical operands must be bool"
    "const B bool = 1 && true\n";
  semantic_error "runtime-logical-bool-only" "logical operands must be bool"
    "fn f() bool { return 1 && true }\n";
  semantic_error "logical-not-integer" "logical not requires bool or a bool vector"
    "fn f(value i64) bool { return !value }\n";
  semantic_error "logical-not-integer-vector"
    "logical not requires bool or a bool vector"
    "fn f(value vec[4,i64]) vec[4,bool] { return !value }\n";
  semantic_error "if-condition-bool-only" "if condition must be bool"
    "fn f(value i64) i64 { if value { return 1 } return 0 }\n";
  semantic_error "while-condition-bool-only" "while condition must be bool"
    "fn f(value ptr[i64]) void { while value { break } }\n";
  semantic_error "for-condition-bool-only" "for condition must be bool"
    "fn f() void { for ; 1; (1) { break } }\n";
  semantic_error "ternary-condition-bool-only" "ternary condition must be bool"
    "fn f(value i64) i64 { return value ? 1 : 0 }\n";
  semantic_error "constant-ternary-condition-bool-only" "ternary condition must be bool"
    "const X i64 = 1 ? 2 : 3\n";
  ignore
    (llvm_of
       "const Explicit bool = (1 != 0) && true\n\
        fn integer(value i64) i64 {\n\
       \ if value != 0 { return (value != 0) ? 1 : 0 }\n\
       \ return 0\n\
       \ }\n\
        fn pointer(value ptr[i64]) bool {\n\
       \ while value != null { break }\n\
       \ return value != null\n\
       \ }\n\
        fn counted(value i64) i64 {\n\
       \ for ; value != 0; (value) { break }\n\
       \ return value\n\
       \ }\n");
  let vector_logical_not =
    llvm_of
      "const Clear vec[4,bool] = splat(false)\n\
       const Set vec[4,bool] = !Clear\n\
       fn invert(value vec[4,bool]) vec[4,bool] { return !value }\n\
       fn clear() vec[4,bool] { return !splat(true) }\n\
       fn first() bool { return Set[0] }\n"
  in
  if not (contains vector_logical_not "xor <4 x i1>") then
    failwith "vector-logical-not: mask lowering missing";
  ignore
    (llvm_of
       "struct Pair[T] { left T right T }\n\
        fn main() i64 { return ((Pair[i64]){12, 4}).left }\n");

  ignore (llvm_of "fn f(x u64) bool { return 1 == x }\n");
  let context_literal_arith_left =
    llvm_of "fn f(x u64) bool { return (1 + 2) == x }\n"
  in
  if not (contains context_literal_arith_left "add i64 1, 2") then
    failwith
      "context-literal-arith-left-peer: literal subtree did not take the peer type";
  ignore (llvm_of "fn f(x u64) bool { return x == (1 + 2) }\n");
  let context_splat_left =
    llvm_of "fn f(x vec[4,u32]) vec[4,bool] { return splat(1) == x }\n"
  in
  if not (contains context_splat_left "icmp eq <4 x i32>") then
    failwith "context-splat-left-peer: splat element did not take the peer type";
  if contains context_splat_left "icmp eq <4 x i1>" then
    failwith "context-splat-left-peer: comparison result leaked into splat element";
  ignore (llvm_of "fn f(x vec[4,u32]) vec[4,bool] { return x == splat(1) }\n");
  ignore (llvm_of "fn f() bool { return (1 + 2) == 3 }\n");
  ignore (llvm_of "const C u64 = 3\nfn f() bool { return (1 + 2) == C }\n");
  let context_const_splat_destination =
    llvm_of
      "const C vec[4,u32] = splat(1)\n\
       fn f(x vec[4,u32]) vec[4,bool] { return C == x }\n"
  in
  if not (contains context_const_splat_destination "icmp eq <4 x i32>") then
    failwith
      "context-const-splat-destination: splat shape did not come from the declared type";
  ignore
    (llvm_of "const C u64 = 3\nconst D bool = (1 + 2) == C\nfn f() bool { return D }\n");
  ignore
    (llvm_of
       "const C vec[4,u32] = splat(1)\n\
        const D vec[4,bool] = splat(1) == C\n\
        const E vec[4,bool] = C == splat(1)\n\
        fn f() vec[4,bool] { return D }\n\
        fn g() vec[4,bool] { return E }\n");
  semantic_error "context-null-unconstrained" "null requires a pointer context"
    "fn f() bool { return null == null }\n";
  semantic_error "context-conflicting-anchors-comparison"
    "binary operands must have the same type"
    "fn f(x u32, y u64) bool { return x == y }\n";
  semantic_error "context-conflicting-anchors-arithmetic"
    "binary operands must have the same type"
    "fn f(x u32, y u64) u64 { return x + y }\n";
  semantic_error "context-splat-no-invented-lanes"
    "splat requires a vector type context" "fn f() usize { return len(splat(1)) }\n";
  semantic_error "context-literal-range-left" "integer literal is out of range for u8"
    "fn f(x u8) bool { return 300 == x }\n";
  semantic_error "context-literal-range-right" "integer literal is out of range for u8"
    "fn f(x u8) bool { return x == 300 }\n";
  semantic_error "context-generic-call-needs-explicit-arguments"
    "generic function `id` requires arguments"
    "fn id[N const u64](x u64) u64 { return x }\nfn f() u64 { return id(1) }\n";
  ignore (llvm_of "fn f(x u32, y u64) u64 { return zext[u64](x) + y }\n");
  ignore (llvm_of "fn f(x i32, y i64) i64 { return sext[i64](x) + y }\n");
  ignore (llvm_of "fn f(x u64) u32 { return trunc[u32](x) }\n");
  semantic_error "context-no-implicit-equal-width"
    "type mismatch: expected u64, got u32" "fn f(x u32) u64 { return x + 1 }\n";
  semantic_error "context-no-implicit-wrong-direction"
    "type mismatch: expected u32, got u64" "fn f(x u64) u32 { return x + 1 }\n";
  semantic_error "context-literal-takes-peer-not-destination"
    "type mismatch: expected u64, got u32" "fn f(x u32) u64 { return 1 + x }\n";
  ignore
    (llvm_of
       "fn f(p ptr[u8]) bool { return p == null }\n\
        fn g(p ptr[u8]) bool { return null == p }\n");
  let shift_unsigned_right = llvm_of "fn f(x u64, n u32) u64 { return x >> n }\n" in
  if not (contains shift_unsigned_right "lshr i64") then
    failwith "shift-unsigned-right: missing lshr";
  if contains shift_unsigned_right "ashr" then
    failwith "shift-unsigned-right: unexpected ashr";
  let shift_signed_right = llvm_of "fn f(x i64, n u32) i64 { return x >> n }\n" in
  if not (contains shift_signed_right "ashr i64") then
    failwith "shift-signed-right: missing ashr";
  if contains shift_signed_right "lshr" then
    failwith "shift-signed-right: unexpected lshr";
  let shift_left =
    llvm_of
      "fn f(x u64, n u32) u64 { return x << n }\nfn g(x u64) u64 { return x << 3 }\n"
  in
  if not (contains shift_left "shl i64") then failwith "shift-left: missing shl";
  if not (contains shift_left "and i64") then
    failwith "shift-left: missing count normalization";
  let shift_generic =
    llvm_of
      "fn shg[T](x T, n u32) T { return x << n }\n\
       fn main() i32 {\n\
      \  a u8 = shg[u8](1, 1)\n\
      \  b i32 = shg[i32](1, 1)\n\
      \  return 0\n\
       }\n"
  in
  if not (contains shift_generic "shl i8") then
    failwith "shift-generic-u8: missing shl i8";
  if not (contains shift_generic "and i8") then
    failwith "shift-generic-u8: normalization not at element width 8";
  if not (contains shift_generic "shl i32") then
    failwith "shift-generic-i32: missing shl i32";
  if not (contains shift_generic "and i32") then
    failwith "shift-generic-i32: normalization not at element width 32";
  let shift_compound =
    llvm_of "fn f(n u32) u64 { x u64 = 8\n  x >>= n\n  return x }\n"
  in
  if not (contains shift_compound "lshr i64") then
    failwith "shift-compound: missing lshr";
  let shift_vector_broadcast =
    llvm_of "fn f(v vec[4,u32], n u32) vec[4,u32] { return v << n }\n"
  in
  if contains shift_vector_broadcast "poison" || contains shift_vector_broadcast "undef"
  then failwith "shift-vector-broadcast: undefined operand in count broadcast";
  let defined_construction =
    llvm_of
      "fn main() i64 {\n\
      \  a vec[4, u32] = splat(6)\n\
      \  b vec[4, u32] = splat(2)\n\
      \  s vec[4, u32] = a << b\n\
      \  m vec[4, bool] = s == splat(24)\n\
      \  n vec[4, bool] = !m\n\
      \  q vec[4, u32] = a / b\n\
      \  z vec[4, u64] = zext[vec[4,u64]](b)\n\
      \  c vec[4, i32] = splat(-6)\n\
      \  d vec[4, i32] = splat(2)\n\
      \  e vec[4, i32] = c / d\n\
      \  f vec[4, u32] = splat(7) % splat(4)\n\
      \  g vec[4, i32] = splat(-7) % splat(4)\n\
      \  if n[0] { return 1 }\n\
      \  if !m[1] { return 2 }\n\
      \  if q[2] != 3 { return 3 }\n\
      \  if z[3] != 2 { return 4 }\n\
      \  if s[0] != 24 { return 5 }\n\
      \  if e[0] != -3 { return 6 }\n\
      \  if f[0] != 3 { return 7 }\n\
      \  if g[0] != -3 { return 8 }\n\
       return 0\n\
       }\n"
  in
  List.iter
    (fun needle ->
      if not (contains defined_construction needle) then
        failwith ("defined-construction: missing " ^ needle))
    [
      "shl <4 x i32>";
      "icmp eq <4 x i32>";
      "xor <4 x i1>";
      "zext <4 x i32>";
      "udiv <4 x i32>";
      "sdiv <4 x i32>";
      "urem <4 x i32>";
      "srem <4 x i32>";
      "insertelement <4 x i32> zeroinitializer, i32 6, i32 0\n";
      "insertelement <4 x i1> zeroinitializer, i1 true, i32 0\n";
      "insertelement <4 x i32> zeroinitializer, i32 2147483648, i32 0\n";
    ];
  if contains defined_construction "poison" || contains defined_construction "undef"
  then failwith "defined-construction: undefined seed in computed values";
  List.iter
    (fun (name, ty, a, b, op, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf
             "fn w[N const %s]() %s { return N }\n\
              const A %s = %s\n\
              const B %s = %s\n\
              fn main() %s { return w[%s(A, B)]() }\n"
             ty ty ty a ty b ty op)
      in
      if not (contains ir needle) then failwith ("sat: " ^ name ^ " drifted"))
    [
      ("u8-add-high", "u8", "200", "100", "add_sat", "ret i8 255\n");
      ("u8-add-mid", "u8", "1", "2", "add_sat", "ret i8 3\n");
      ("u8-sub-low", "u8", "5", "10", "sub_sat", "ret i8 0\n");
      ("i8-add-high", "i8", "100", "100", "add_sat", "ret i8 127\n");
      ("i8-add-low", "i8", "-100", "-100", "add_sat", "ret i8 128\n");
      ("i8-sub-high", "i8", "127", "-1", "sub_sat", "ret i8 127\n");
      ("i8-sub-low", "i8", "-128", "1", "sub_sat", "ret i8 128\n");
      ( "i64-add-min",
        "i64",
        "-9223372036854775808",
        "-1",
        "add_sat",
        "ret i64 -9223372036854775808\n" );
      ( "i64-add-max",
        "i64",
        "9223372036854775807",
        "1",
        "add_sat",
        "ret i64 9223372036854775807\n" );
      ("usize-add", "usize", "1", "2", "add_sat", "ret i64 3\n");
      ("u64-add-wrap", "u64", "18446744073709551615", "1", "add_sat", "ret i64 -1\n");
      ("u64-sub-under", "u64", "0", "1", "sub_sat", "ret i64 0\n");
      ("isize-sub", "isize", "-2", "1", "sub_sat", "ret i64 -3\n");
      ("i8-sub-degenerate", "i8", "0", "-128", "sub_sat", "ret i8 127\n");
    ];
  let sat_vec_add =
    llvm_of
      "fn w[N const u32]() u32 { return N }\n\
       const A u32 = 16712136\n\
       const B u32 = 83886692\n\
       const AV vec[4,u8] = bitcast[vec[4,u8]](A)\n\
       const BV vec[4,u8] = bitcast[vec[4,u8]](B)\n\
       const X vec[4,u8] = add_sat(AV, BV)\n\
       fn main() u32 { return w[bitcast[u32](X)]() }\n"
  in
  if not (contains sat_vec_add "ret i32 100598783\n") then
    failwith "sat: vec add lanes drifted";
  let sat_vec_sub =
    llvm_of
      "fn w[N const u32]() u32 { return N }\n\
       const A u32 = 16712136\n\
       const B u32 = 83886692\n\
       const AV vec[4,u8] = bitcast[vec[4,u8]](A)\n\
       const BV vec[4,u8] = bitcast[vec[4,u8]](B)\n\
       const Y vec[4,u8] = sub_sat(AV, BV)\n\
       fn main() u32 { return w[bitcast[u32](Y)]() }\n"
  in
  if not (contains sat_vec_sub "ret i32 16711780\n") then
    failwith "sat: vec sub lanes drifted";
  let sat_runtime =
    llvm_of
      "fn f(a vec[4,u8], b vec[4,u8]) vec[4,u8] { return add_sat(a, b) }\n\
       fn g(a i32, b i32) i32 { return sub_sat(a, b) }\n\
       fn h(a u32, b u32) u32 { return sub_sat(a, b) }\n\
       fn i(a i8, b i8) i8 { return add_sat(a, b) }\n\
       fn p(a isize, b isize) isize { return add_sat(a, b) }\n\
       fn q(a usize, b usize) usize { return sub_sat(a, b) }\n\
       fn r(a vec[3,u8], b vec[3,u8]) vec[3,u8] { return add_sat(a, b) }\n\
       fn s(a vec[1,i32], b vec[1,i32]) vec[1,i32] { return sub_sat(a, b) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains sat_runtime needle) then failwith ("sat: missing " ^ needle))
    [
      "@llvm.uadd.sat.v4i8(";
      "@llvm.ssub.sat.i32(";
      "@llvm.usub.sat.i32(";
      "@llvm.sadd.sat.i8(";
      "@llvm.sadd.sat.i64(";
      "@llvm.usub.sat.i64(";
      "@llvm.uadd.sat.v3i8(";
      "@llvm.ssub.sat.v1i32(";
    ];
  if contains sat_runtime "poison" || contains sat_runtime "undef" then
    failwith "sat: undefined value in computed results";
  List.iter
    (fun (name, text, expected) ->
      match semantic_messages text with
      | [ message ] when message = expected -> ()
      | _ -> failwith ("sat reject: " ^ name))
    [
      ( "arity",
        "fn f(a u8) u8 { return add_sat(a) }\n",
        "builtin `add_sat` expects two arguments" );
      ( "bool-scalar",
        "fn f() bool { return add_sat(true, false) }\n",
        "builtin arguments must be integers or integer vectors" );
      ( "bool-vec",
        "fn f(m vec[2,bool]) vec[2,bool] { return add_sat(m, m) }\n",
        "builtin arguments must be integers or integer vectors" );
      ( "mixed-widths",
        "fn f(a u8, b u16) u16 { return add_sat(a, b) }\n",
        "builtin arguments must have the same type" );
      ( "mixed-shape",
        "fn f(a u8, v vec[2,u8]) vec[2,u8] { return add_sat(a, v) }\n",
        "builtin arguments must have the same type" );
      ( "const-mixed",
        "const A u8 = 1\nconst B u16 = 2\nconst X u16 = add_sat(A, B)\n",
        "builtin arguments must have the same type" );
      ( "const-mixed-lanes",
        "const AV vec[2,u8] = splat(1)\n\
         const BV vec[4,u8] = splat(2)\n\
         const X vec[2,u8] = add_sat(AV, BV)\n",
        "builtin arguments must have the same type" );
      ( "vec-const-scalar",
        "const AV vec[4,u8] = splat(1)\nconst X vec[4,u8] = add_sat(AV, 2)\n",
        "expression is not a compile-time vector constant" );
      ( "const-bool",
        "const X bool = add_sat(true, false)\n",
        "builtin arguments must be integers or integer vectors" );
      ( "vec-const-bool",
        "const M vec[2,bool] = splat(true)\nconst X vec[2,bool] = add_sat(M, M)\n",
        "builtin arguments must be integers or integer vectors" );
    ];
  List.iter
    (fun (name, ir) ->
      if contains ir " nuw " || contains ir " nsw " || contains ir " exact " then
        failwith (name ^ ": unexpected shift flags"))
    [
      ("shift-unsigned-right", shift_unsigned_right);
      ("shift-signed-right", shift_signed_right);
      ("shift-left", shift_left);
      ("shift-generic", shift_generic);
      ("shift-compound", shift_compound);
      ("shift-vector-broadcast", shift_vector_broadcast);
    ];

  let lane_paired_division_guard =
    llvm_of
      "fn div(left vec[2,i64], right vec[2,i64]) vec[2,i64] { return left / right }\n"
  in
  List.iter
    (fun marker ->
      if not (contains lane_paired_division_guard marker) then
        failwith ("lane-paired-division-guard: missing `" ^ marker ^ "`"))
    [
      "and <2 x i1>";
      "or <2 x i1>";
      "icmp eq <2 x i64>";
      "sdiv <2 x i64>";
      "call void @llvm.trap()";
    ];
  if
    List.length
      (positions lane_paired_division_guard "call i1 @llvm.vector.reduce.or.v2i1")
    <> 1
  then
    failwith
      "lane-paired-division-guard: overflow pairing must collapse to one reduction";
  let scan_indices =
    List.map
      (fun offset ->
        let rec line_end index =
          if lane_paired_division_guard.[index] = '\n' then index
          else line_end (index + 1)
        in
        lane_paired_division_guard.[line_end
                                      (offset + String.length "extractelement <2 x i1> ")
                                    - 1])
      (positions lane_paired_division_guard "extractelement <2 x i1> ")
  in
  if scan_indices <> [ '0'; '1' ] then
    failwith
      "lane-paired-division-guard: trap checks must scan lanes in scalar element order";
  let bool_one_lane_bitcasts =
    llvm_of
      "fn to_bool(mask vec[1,bool]) bool { return bitcast[bool](mask) }\n\
       fn to_mask(value bool) vec[1,bool] { return bitcast[vec[1,bool]](value) }\n"
  in
  List.iter
    (fun marker ->
      if not (contains bool_one_lane_bitcasts marker) then
        failwith ("bool-one-lane-bitcasts: missing `" ^ marker ^ "`"))
    [ "bitcast <1 x i1>"; "bitcast i1 " ];
  List.iter
    (fun forbidden ->
      if contains bool_one_lane_bitcasts forbidden then
        failwith ("bool-one-lane-bitcasts: unexpected `" ^ forbidden ^ "` lowering"))
    [ "trunc <1 x i1>"; "zext i1 "; "sext i1 " ];
  let cross_shape_value_bitcasts =
    llvm_of
      "fn one_to_int(value vec[1,u64]) u64 { return bitcast[u64](value) }\n\
       fn int_to_one(value u64) vec[1,u64] { return bitcast[vec[1,u64]](value) }\n\
       fn widen(value vec[4,u32]) vec[16,u8] { return bitcast[vec[16,u8]](value) }\n\
       fn narrow(value vec[16,u8]) vec[4,u32] { return bitcast[vec[4,u32]](value) }\n\
       fn odd_reshape(value vec[3,u16]) vec[6,u8] { return bitcast[vec[6,u8]](value) }\n\
       fn odd_restore(value vec[6,u8]) vec[3,u16] {\n\
      \       return bitcast[vec[3,u16]](value)\n\
      \ }\n\
       fn pack_bool(value vec[8,bool]) u8 { return bitcast[u8](value) }\n\
       fn unpack_bool(value u8) vec[8,bool] { return bitcast[vec[8,bool]](value) }\n"
  in
  List.iter
    (fun marker ->
      if not (contains cross_shape_value_bitcasts marker) then
        failwith ("cross-shape-value-bitcasts: missing `" ^ marker ^ "`"))
    [
      "bitcast <1 x i64>";
      "bitcast i64 ";
      "bitcast <4 x i32>";
      "bitcast <16 x i8>";
      "bitcast <3 x i16>";
      "bitcast <6 x i8>";
      "bitcast <8 x i1>";
      "bitcast i8 ";
    ];
  if List.length (positions cross_shape_value_bitcasts "bitcast ") <> 8 then
    failwith "cross-shape-value-bitcasts: expected eight mechanical LLVM bitcasts";
  let odd_lane_conversions =
    llvm_of
      "fn widen_bool(value vec[3,bool]) vec[3,u16] { return zext[vec[3,u16]](value) }\n\
       fn sign_widen(value vec[3,i8]) vec[3,i64] { return sext[vec[3,i64]](value) }\n\
       fn narrow(value vec[3,u64]) vec[3,u8] { return trunc[vec[3,u8]](value) }\n\
       fn truth_bits(value vec[3,u32]) vec[3,bool] { return trunc[vec[3,bool]](value) }\n\
       fn one_sign(value vec[1,bool]) vec[1,i64] { return sext[vec[1,i64]](value) }\n\
       fn one_zero(value vec[1,bool]) vec[1,u64] { return zext[vec[1,u64]](value) }\n\
       fn widen_unsigned(value vec[3,u8]) vec[3,u64] { return zext[vec[3,u64]](value) }\n\
       fn sign_from_unsigned(value vec[2,u8]) vec[2,u32] {\n\
      \       return sext[vec[2,u32]](value)\n\
      \ }\n"
  in
  List.iter
    (fun marker ->
      if not (contains odd_lane_conversions marker) then
        failwith ("odd-lane-conversions: missing `" ^ marker ^ "`"))
    [
      "zext <3 x i1>";
      "sext <3 x i8>";
      "trunc <3 x i64>";
      "trunc <3 x i32>";
      "sext <1 x i1>";
      "zext <1 x i1>";
      "zext <3 x i8>";
      "sext <2 x i8>";
    ];
  let scalar_width_conversions =
    llvm_of
      "fn widen(value u8) u64 { return zext[u64](value) }\n\
       fn sign_widen(value i8) i64 { return sext[i64](value) }\n\
       fn sign_from_unsigned(value u8) u32 { return sext[u32](value) }\n\
       fn low_bit(value u32) bool { return trunc[bool](value) }\n\
       fn narrow(value u64) u16 { return trunc[u16](value) }\n\
       fn widen_size(value u16) usize { return zext[usize](value) }\n\
       fn sign_size(value i16) isize { return sext[isize](value) }\n\
       fn bool_zero(value bool) u64 { return zext[u64](value) }\n\
       fn bool_sign(value bool) i64 { return sext[i64](value) }\n"
  in
  List.iter
    (fun marker ->
      if not (contains scalar_width_conversions marker) then
        failwith ("scalar-width-conversions: missing `" ^ marker ^ "`"))
    [
      "zext i8";
      "sext i8";
      "trunc i32";
      "trunc i64";
      "zext i16";
      "sext i16";
      "zext i1";
      "sext i1";
    ];
  semantic_error "vector-zext-equal-width"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,u8]) vec[4,u8] { return zext[vec[4,u8]](value) }\n";
  semantic_error "vector-sext-equal-width"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,i32]) vec[4,i32] { return sext[vec[4,i32]](value) }\n";
  semantic_error "vector-trunc-equal-width"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,u32]) vec[4,u32] { return trunc[vec[4,u32]](value) }\n";
  semantic_error "vector-zext-bool-equal-width"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,bool]) vec[4,bool] { return zext[vec[4,bool]](value) }\n";
  semantic_error "vector-sext-lane-count"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,i8]) vec[2,i16] { return sext[vec[2,i16]](value) }\n";
  semantic_error "vector-trunc-lane-count"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,u16]) vec[2,u8] { return trunc[vec[2,u8]](value) }\n";
  semantic_error "scalar-zext-bool-destination"
    "illegal cast for source and destination widths"
    "fn f(value u8) bool { return zext[bool](value) }\n";
  semantic_error "scalar-sext-bool-destination"
    "illegal cast for source and destination widths"
    "fn f(value u8) bool { return sext[bool](value) }\n";
  semantic_error "bitcast-bool-padding-source"
    "illegal cast for source and destination widths"
    "fn f(value vec[3,bool]) u8 { return bitcast[u8](value) }\n";
  semantic_error "bitcast-bool-padding-destination"
    "illegal cast for source and destination widths"
    "fn f(value u8) vec[3,bool] { return bitcast[vec[3,bool]](value) }\n";
  semantic_error "bitcast-unequal-scalar-widths"
    "illegal cast for source and destination widths"
    "fn f(value u64) u32 { return bitcast[u32](value) }\n";
  semantic_error "bitcast-array-destination"
    "illegal cast for source and destination widths"
    "fn f(value vec[2,u32]) arr[2,u32] { return bitcast[arr[2,u32]](value) }\n";
  semantic_error "bitcast-struct-destination"
    "illegal cast for source and destination widths"
    "struct Pair { left u32 right u32 }\n\
    \ fn f(value vec[2,u32]) Pair { return bitcast[Pair](value) }\n";
  semantic_error "cast-pointer-zext" "illegal cast for source and destination widths"
    "fn f(value ptr[u8]) u64 { return zext[u64](value) }\n";
  semantic_error "cast-pointer-sext" "illegal cast for source and destination widths"
    "fn f(value ptr[u8]) u64 { return sext[u64](value) }\n";
  semantic_error "cast-pointer-trunc" "illegal cast for source and destination widths"
    "fn f(value ptr[u8]) u32 { return trunc[u32](value) }\n";
  semantic_error "cast-pointer-bitcast-bool"
    "illegal cast for source and destination widths"
    "fn f(value ptr[u8]) bool { return bitcast[bool](value) }\n";
  semantic_error "cast-vector-bitcast-pointer"
    "illegal cast for source and destination widths"
    "fn f(value vec[1,u64]) ptr[u8] { return bitcast[ptr[u8]](value) }\n";
  semantic_error "cast-aggregate-zext" "illegal cast for source and destination widths"
    "struct Pair { left u32 right u32 }\n\
    \ fn f() u64 {\n\
    \ value Pair = (Pair){1, 2}\n\
    \ return zext[u64](value)\n\
    \ }\n";
  semantic_error "constant-vector-division-first-lane-overflow"
    "signed division overflow in constant expression"
    "const XA u64 = 6442450944\n\
     const XB u64 = 4294967295\n\
     const A vec[2,i32] = bitcast[vec[2,i32]](XA)\n\
     const B vec[2,i32] = bitcast[vec[2,i32]](XB)\n\
     const Q vec[2,i32] = A / B\n\
     fn main() i32 { return 0 }\n";
  semantic_error "constant-vector-division-first-lane-zero"
    "division by zero in constant expression"
    "const XC u64 = 9223372036854775809\n\
     const XD u64 = 18446744069414584320\n\
     const C vec[2,i32] = bitcast[vec[2,i32]](XC)\n\
     const D vec[2,i32] = bitcast[vec[2,i32]](XD)\n\
     const R vec[2,i32] = C / D\n\
     fn main() i32 { return 0 }\n";
  let constant_division_cross_pairs =
    llvm_of
      "const XE u64 = 19327352832\n\
       const XF u64 = 18446744069414584321\n\
       const E vec[2,i32] = bitcast[vec[2,i32]](XE)\n\
       const F vec[2,i32] = bitcast[vec[2,i32]](XF)\n\
       const S vec[2,i32] = E / F\n\
       fn low() i32 { return S[0] }\n"
  in
  if not (contains constant_division_cross_pairs "<i32 2147483648, i32 4294967292>")
  then failwith "constant-division-cross-pairs: expected paired lane result";
  let constant_remainder_edges =
    llvm_of
      "const XA u64 = 23622320128\n\
       const XB u64 = 12884901887\n\
       const A vec[2,i32] = bitcast[vec[2,i32]](XA)\n\
       const B vec[2,i32] = bitcast[vec[2,i32]](XB)\n\
       const R vec[2,i32] = A % B\n\
       fn low() i32 { return R[0] }\n"
  in
  if not (contains constant_remainder_edges "<i32 0, i32 1>") then
    failwith "constant-remainder-edges: MIN % -1 lane must yield zero";
  let constant_bool_pack =
    llvm_of
      "const A u8 = 204\n\
       const P vec[8,bool] = bitcast[vec[8,bool]](A)\n\
       const M vec[1,bool] = splat(true)\n\
       const B bool = bitcast[bool](M)\n\
       const M2 vec[1,bool] = bitcast[vec[1,bool]](true)\n\
       fn packed() vec[8,bool] { return P }\n\
       fn single() bool { return B }\n"
  in
  if
    not (contains constant_bool_pack "<i1 0, i1 0, i1 1, i1 1, i1 0, i1 0, i1 1, i1 1>")
  then failwith "constant-bool-pack: expected packed value bits";
  semantic_error "constant-bitcast-literal-parity"
    "integer literal is out of range for vec[8, bool]"
    "const P vec[8,bool] = bitcast[vec[8,bool]](204)\nfn main() i32 { return 0 }\n";
  semantic_error "runtime-bitcast-literal-parity"
    "integer literal is out of range for vec[8, bool]"
    "fn f() vec[8,bool] { return bitcast[vec[8,bool]](204) }\n";
  let generic_cast_path =
    llvm_of
      "fn unwrap[N const u64](mask vec[1,bool]) bool { return bitcast[bool](mask) }\n\
       fn widen_generic[N const u64](value u8) u64 { return zext[u64](value) + N }\n\
       fn use_it() bool { return unwrap[7](splat(true)) }\n\
       fn use_wide() u64 { return widen_generic[2](255) }\n"
  in
  List.iter
    (fun marker ->
      if not (contains generic_cast_path marker) then
        failwith ("generic-cast-path: missing `" ^ marker ^ "`"))
    [ "bitcast <1 x i1>"; "zext i8" ];

  Printf.printf "regression checks: %d passed\n" !checks_run
