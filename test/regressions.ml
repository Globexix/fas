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

let c_import_fixture file =
  let cwd = Sys.getcwd () in
  let candidates =
    [
      Filename.concat cwd ("test/c_import/" ^ file);
      Filename.concat cwd ("c_import/" ^ file);
    ]
  in
  let header =
    match List.find_opt Sys.file_exists candidates with
    | Some path -> path
    | None -> failwith ("missing C import fixture " ^ file)
  in
  let source = Filename.concat (Filename.dirname header) "probe.fas" in
  let request =
    C_import.
      { spelling = Ast.C_quoted (Filename.basename header); span = Span.synthetic }
  in
  let declarations, _, _ =
    expect_ok
      (C_import.import ~cc:"clang-22" ~debug:false ~keep:false source [ request ])
  in
  (source, C_import.map_declarations ~span:Span.synthetic declarations)

let c_semantic_result ((source, imported) : string * C_import.mapped) text =
  incr checks_run;
  let program =
    match Parser.parse (Source.create ~file:source ~text) with
    | Ok program -> program
    | Error diagnostics ->
        failwith (Diag.render_all ~source:None diagnostics ^ "\n" ^ text)
  in
  let program = { Ast.items = program.items @ imported.items } in
  Sema.check ~c_aliases:imported.aliases ~c_unsupported:imported.unsupported
    ~c_records:imported.record_types program

let c_semantic_accept name imported text =
  match c_semantic_result imported text with
  | Ok _ -> ()
  | Error diagnostics ->
      failwith
        (name ^ ": unexpected rejection: " ^ Diag.render_all ~source:None diagnostics)

let c_semantic_message name expected imported text =
  match c_semantic_result imported text with
  | Ok _ -> failwith (name ^ ": expected rejection")
  | Error diagnostics -> (
      match List.map (fun (diagnostic : Diag.t) -> diagnostic.message) diagnostics with
      | [ actual ] when actual = expected -> ()
      | actual ->
          failwith
            (name ^ ": expected [" ^ expected ^ "], got [" ^ String.concat "; " actual
           ^ "]"))

let c_semantic_error name fragment imported text =
  match c_semantic_result imported text with
  | Ok _ -> failwith (name ^ ": expected rejection")
  | Error diagnostics ->
      let rendered = Diag.render_all ~source:None diagnostics in
      if not (contains rendered fragment) then
        failwith (name ^ ": unexpected diagnostic: " ^ rendered)

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

let semantic_message name expected text =
  incr checks_run;
  match semantic_messages text with
  | [ actual ] when actual = expected -> ()
  | actual ->
      failwith
        (name ^ ": expected [" ^ expected ^ "], got [" ^ String.concat "; " actual ^ "]")

let semantic_error name fragment text =
  incr checks_run;
  let program = expect_ok (Parser.parse (source text)) in
  match Sema.check program with
  | Ok _ -> failwith (name ^ ": expected semantic rejection")
  | Error diagnostics ->
      let rendered = Diag.render_all ~source:None diagnostics in
      if not (contains rendered fragment) then
        failwith (name ^ ": unexpected diagnostic: " ^ rendered)

let semantic_accept name text =
  incr checks_run;
  let program = expect_ok (Parser.parse (source text)) in
  match Sema.check program with
  | Ok _ -> ()
  | Error diagnostics ->
      failwith
        (name ^ ": unexpected rejection: " ^ Diag.render_all ~source:None diagnostics)

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

let parse_message name expected text =
  incr checks_run;
  match parse_messages text with
  | [ actual ] when actual = expected -> ()
  | actual ->
      failwith
        (name ^ ": expected [" ^ expected ^ "], got [" ^ String.concat "; " actual ^ "]")

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
        globals = [];
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
        globals = [];
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
    "fn take(value usize) void { return }\n\
     fn check_case(value u64) void { take(value) }\n";
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
     fn check_case() void { take(Small) }\n";
  semantic_error "named-i8-constant-binary-keeps-type"
    "binary operands must have the same type"
    "const Small i8 = 7\nfn add(value i32) i32 { return Small + value }\n";
  semantic_error "named-i8-constant-generic-argument-keeps-type"
    "const argument type mismatch"
    "const Small i8 = 7\n\
     fn value[N const i32]() i32 { return N }\n\
     fn check_case() i32 { return value[Small]() }\n";
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
       const Values arr[3,u8] = {1, 2, 3}\n\
       fn to_u64(value usize) u64 { return bitcast[u64](value) }\n\
       fn to_i64(value isize) i64 { return bitcast[i64](value) }\n\
       fn unsigned_div(left usize, right usize) usize { return left / right }\n\
       fn signed_div(left isize, right isize) isize { return left / right }\n\
       fn sizes() usize {\n\
      \ return sizeof[Pair] + alignof[Pair] + offsetof[Pair,right] + len(Values)\n\
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
    "raw selection cannot load an aggregate value"
    "fn f(value addr) vec[2,u32] { return bitcast[vec[2,u32]](value[arr[2,u32]]) }\n";
  semantic_error "integer-vector-bitcast-struct"
    "raw selection cannot load an aggregate value"
    "struct Pair { left u32 right u32 }\n\
    \ fn f(value addr) vec[2,u32] { return bitcast[vec[2,u32]](value[Pair]) }\n";
  semantic_error "integer-vector-bitcast-pointer"
    "illegal cast for source and destination widths"
    "fn f(value addr) vec[1,u64] { return bitcast[vec[1,u64]](value) }\n";
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
    (not (contains integer_vector_comparisons "store i8"))
    || (not (contains integer_vector_comparisons "extractelement <4 x i1>"))
    || (not (contains integer_vector_comparisons "icmp eq <8 x i1>"))
    || not (contains integer_vector_comparisons "and <8 x i1>")
  then failwith "integer-vector-comparison: result vector was not preserved";
  if contains integer_vector_comparisons "store <4 x i1>" then
    failwith "integer-vector-comparison: mask store retained unused bits";
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
    "const Values arr[3,u8] = {1, 2, 3}\nfn size() u64 { return len(Values) }\n";
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
      "fn take(p addr) void { return }\n\
       fn main() i64 { a arr[2, i64]\n\
       take(&a[0])\n\
       return 0 }\n"
  in
  if not (contains uninitialized_array_address "call void @take(ptr") then
    failwith "place-init: address of an uninitialized array element failed";
  let uninitialized_address =
    llvm_of
      "fn take(p addr) void { return }\nfn main() i64 { x i64\n take(&x)\n return 0 }\n"
  in
  if not (contains uninitialized_address "call void @take(ptr") then
    failwith "place-init: taking the address of an uninitialized local failed";
  semantic_error "place-init-pointer-intermediate" "use of uninitialized local `p`"
    "fn main() i64 { p addr\n p[i64] = 1\n return 0 }\n";
  semantic_error "place-init-pointer-index-write" "use of uninitialized local `p`"
    "fn main() i64 { p addr\n p[i64] = 1\n return 0 }\n";
  semantic_error "place-init-pointer-index-read" "use of uninitialized local `p`"
    "fn main() i64 { p addr\n x i64 = p[i64]\n return x }\n";
  semantic_error "place-init-pointer-index-address" "use of uninitialized local `p`"
    "fn take(p addr) void { return }\n\
     fn main() i64 { p addr\n\
     take(&p[i64])\n\
     return 0 }\n";
  semantic_error "place-init-pointer-index-compound" "use of uninitialized local `p`"
    "fn main() i64 { p addr\n p[i64] += 1\n return 0 }\n";
  semantic_error "place-init-pointer-field-write" "use of uninitialized local `s`"
    "struct S { p addr }\nfn main() i64 { s S\n s.p[i64] = 1\n return 0 }\n";
  semantic_error "place-init-pointer-field-read" "use of uninitialized local `s`"
    "struct S { p addr }\nfn main() i64 { s S\n x i64 = s.p[i64]\n return x }\n";
  semantic_error "place-init-pointer-field-address" "use of uninitialized local `s`"
    "struct S { p addr }\n\
     fn take(p addr) void { return }\n\
     fn main() i64 { s S\n\
    \ take(&s.p[i64])\n\
    \ return 0 }\n";
  semantic_error "place-init-pointer-array-element" "use of uninitialized local `a`"
    "fn main() i64 { a arr[2,addr]\n a[0][i64] = 1\n return 0 }\n";
  ignore
    (lower_of
       "struct S { p addr }\n\
        fn main() i64 { x i64\n\
       \ s S\n\
       \ s.p = &x\n\
       \ s.p[i64] = 1\n\
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
    "fn take(p addr) void { return }\n\
     fn f() i64 { v vec[2,i64] = splat(1)\n\
     take(&v[0])\n\
     return 0 }\n";
  ignore
    (lower_of
       "fn read_value(v u32) u32 { return v }\n\
        fn f() u32 { x u32\n\
       \ view v = x\n\
       \ v = 5\n\
       \ v += 2\n\
       \ return read_value(v) }\n");
  ignore (lower_of "fn f(p addr) u32 { view v = p[u32]\n return v }\n");
  ignore
    (lower_of
       "fn f() u32 { a arr[2,u32]\n a[0] = 1\n view v = a[0]\n v = 3\n return v }\n");
  ignore
    (lower_of
       "fn f() u32 { x vec[2,u32] = splat(0)\n view v = x\n v[0] = 4\n return v[0] }\n");
  ignore
    (lower_of
       "struct Inner { value u32 }\n\
        fn f() u32 { s Inner\n\
       \ view outer = s\n\
       \ view inner = outer.value\n\
       \ inner = 8\n\
       \ return s.value }\n");
  ignore
    (lower_of
       "fn f() u32 { x u32 = 2\n\
       \ view v = x\n\
       \ { y u32 = 3\n\
       \ view v = y\n\
       \ v = 5 }\n\
       \ return v }\n");
  ignore
    (lower_of
       "fn at[N const u64](p addr) u32 { view v = p[u32, N]\n\
       \ return v }\n\
        fn f(p addr) u32 { return at[0](p) }\n");
  semantic_error "view-uninitialized-read" "use of uninitialized local `x`"
    "fn f() u32 { x u32\n view v = x\n return v }\n";
  semantic_error "view-element-facts-share" "use of uninitialized local `a`"
    "fn f() u32 { a arr[2,u32]\n view v = a[0]\n v = 1\n return a[1] }\n";
  semantic_error "view-call-rvalue" "view source must be an existing place"
    "fn make() u32 { return 1 }\nfn f() void { view v = make()\n return }\n";
  semantic_error "view-arithmetic-rvalue" "view source must be an existing place"
    "fn f(x u32) void { view v = x + 1\n return }\n";
  semantic_error "view-literal-rvalue" "view source must be an existing place"
    "fn f() void { view v = 1\n return }\n";
  semantic_error "view-simd-lane" "cannot create a view of a SIMD lane"
    "fn f() void { x vec[2,u32] = splat(0)\nview lane = x[0]\n return }\n";
  semantic_error "view-shadow-duplicate" "duplicate local `v`"
    "fn f(x u32) void { view v = x\n view v = x\n return }\n";
  semantic_error "view-name-reserved"
    "`view` is reserved and cannot be used as a binding"
    "fn f(x u32) void { view view = x\n return }\n";
  parse_error "view-top-level" "view x = 1\n";
  semantic_error "view-readonly-source" "cannot modify read-only pointer"
    "fn f() void { view v = c\"read only\"[u8]\n v = 2\n return }\n";
  ignore
    (lower_of
       "fn take(p addr) void { return }\n\
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
      "fn base() addr { return addr_from_bits(0) }\n\
       fn index() i64 { return 0 }\n\
       fn f() i64 { base()[i64, index()] = 1\n\
      \ return 0 }\n"
  in
  let base_calls = positions index_evaluation_order "call ptr @base" in
  let index_calls = positions index_evaluation_order "call i64 @index" in
  (match (base_calls, index_calls) with
  | base :: _, index :: _ when base < index -> ()
  | _ -> failwith "aggregate-index-order: index was evaluated before its base");
  let vector_assign_alias =
    llvm_of
      "fn mutate(p addr) i64 { p[vec[2,i64]] = splat(9)\n\
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
      "fn mutate(p addr) i64 { p[vec[2,i64]] = splat(9)\n\
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
    \ true || (&s != addr_from_bits(0))\n\
    \ t S\n\
    \ copy(t, s)\n\
    \ return 0 }\n";
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn take(p addr) void { return }\n\
        fn f() i64 { s S\n\
       \ take(&s)\n\
       \ true || false\n\
       \ t S\n\
       \ copy(t, s)\n\
       \ return 0 }\n");
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f(p bool) i64 { s S\n\
       \ q addr = p ? &s : addr_from_bits(0)\n\
        t S\n\
        copy(t, s)\n\
       \ return 0 }\n");
  semantic_error "place-init-ternary-cross-arm" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn choose(x i64) i64 { return x }\n\
     fn f(p bool) i64 { s S\n\
    \ t i64 = p ? choose(s.x) : s.x\n\
    \ return t }\n";
  semantic_error "place-init-binding-identity" "use of uninitialized local `x`"
    "fn main() i64 { { x i64 = 1 }\n { x i64\n y i64 = x\n }\n return 0 }\n";
  semantic_error "aggregate-whole-read" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\nfn f() i64 { s S\n t S\n copy(t, s)\n return 0 }\n";
  semantic_error "aggregate-partial-field" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\nfn f() i64 { s S\n s.x = 1\n return s.y }\n";
  semantic_error "aggregate-partial-whole" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn f() i64 { s S\n\
    \ s.x = 1\n\
    \ t S\n\
    \ copy(t, s)\n\
    \ return 0 }\n";
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f() i64 { s S\n\
       \ s.x = 1\n\
       \ s.y = 2\n\
       \ t S\n\
       \ copy(t, s)\n\
       \ return s.x }\n");
  semantic_error "aggregate-nested-field" "use of uninitialized local `o`"
    "struct I { x i64 y i64 }\n\
     struct O { i I z i64 }\n\
     fn f() i64 { o O\n\
    \ o.i.x = 1\n\
    \ return o.i.y }\n";
  semantic_error "aggregate-array-whole" "use of uninitialized local `a`"
    "fn f() i64 { a arr[2,i64]\n a[0] = 1\n t arr[2,i64]\n copy(t, a)\n return 0 }\n";
  ignore
    (lower_of
       "fn f() i64 { a arr[2,i64]\n\
       \ a[0] = 1\n\
       \ x i64 = a[0]\n\
       \ a[1] = 2\n\
       \ t arr[2,i64]\n\
       \ copy(t, a)\n\
       \ return x }\n");
  let array_field_copy =
    llvm_of
      "struct S { a arr[2,i64] }\n\
       fn f() i64 { s S\n\
      \ s.a[0] = 1\n\
      \ s.a[1] = 2\n\
      \ t arr[2,i64]\n\
      \ copy(t, s.a)\n\
      \ return t[0] }\n"
  in
  if contains array_field_copy "load [2 x i64], ptr" then
    failwith "aggregate-array-field-copy: aggregate was loaded as a value";
  let nested_array_copy =
    llvm_of
      "fn f() i64 { a arr[2,arr[2,i64]]\n\
      \ a[0][0] = 1\n\
      \ a[0][1] = 2\n\
      \ t arr[2,i64]\n\
      \ copy(t, a[0])\n\
      \ return t[0] }\n"
  in
  if contains nested_array_copy "load [2 x i64], ptr" then
    failwith "aggregate-nested-array-copy: aggregate was loaded as a value";
  semantic_error "aggregate-whole-array-uninitialized" "use of uninitialized local `a`"
    "fn f() i64 { a arr[2,i64]\n return a[0] }\n";
  ignore (lower_of "struct E { }\nfn f() i64 { e E\n t E\n copy(t, e)\n return 0 }\n");
  ignore
    (lower_of "fn f() i64 { a arr[0,i64]\n t arr[0,i64]\n copy(t, a)\n return 0 }\n");
  ignore
    (lower_of
       "struct E { }\n\
        struct S { e E x i64 }\n\
        fn f() i64 { s S\n\
       \ s.x = 1\n\
       \ t E\n\
       \ copy(t, s.e)\n\
       \ return s.x }\n");
  ignore
    (lower_of
       "struct E { }\n\
        struct S { e E }\n\
        fn f() i64 { s S\n\
       \ t S\n\
       \ copy(t, s)\n\
       \ return 0 }\n");
  ignore
    (lower_of
       "fn f() i64 { a arr[2,i64]\n\
       \ a[0] = 1\n\
       \ a[1] = 2\n\
       \ t arr[2,i64]\n\
       \ copy(t, a)\n\
       \ return t[0] }\n");
  semantic_error "aggregate-direct-return"
    "aggregate result cannot be returned by value; pass destination storage as `addr` \
     or `handle[T]`"
    "struct S { x i64 }\nfn f() S { s S\nreturn s }\n";
  semantic_error "aggregate-parameter"
    "aggregate parameter `value` cannot be passed by value; pass `&x` as `addr` or \
     `handle[T]`"
    "struct S { x i64 }\nfn f(value S) void { return }\n";
  semantic_error "aggregate-array-parameter"
    "aggregate parameter `value` cannot be passed by value; pass `&x` as `addr` or \
     `handle[T]`"
    "fn f(value arr[4,u32]) void { return }\n";
  semantic_error "aggregate-result"
    "aggregate result cannot be returned by value; pass destination storage as `addr` \
     or `handle[T]`"
    "struct S { x i64 }\nfn f() S { s S\nreturn s }\n";
  semantic_error "aggregate-array-result"
    "aggregate result cannot be returned by value; pass destination storage as `addr` \
     or `handle[T]`"
    "fn f() arr[4,u32] { value arr[4,u32]\nreturn value }\n";
  semantic_error "aggregate-declaration-copy"
    "aggregate value initialization is not supported; use `copy(dst, src)`"
    "struct S { x i64 }\nfn f() void { s S = (S){1}\n t S = s\nreturn }\n";
  semantic_error "aggregate-argument"
    "aggregate arguments cannot be passed by value; pass `&x` as `addr` or `handle[T]`"
    "struct S { x i64 }\n\
     fn take(p addr) void { return }\n\
     fn f() void { s S = (S){1}\n\
     take(s)\n\
     return }\n";
  semantic_accept "variadic-address-handle-arguments"
    "opaque Token\n\
     extern \"C\" { fn consume(marker u64, ...) u64 }\n\
     fn pass(pointer addr, token handle[Token]) u64 { return consume(1, pointer, \
     token) }\n";
  semantic_error "variadic-vector-argument"
    "aggregate arguments cannot be passed by value; pass `&x` as `addr` or `handle[T]`"
    "extern \"C\" { fn consume(marker u64, ...) void }\n\
     fn pass() void { value vec[2,u32] = splat(0)\n\
     consume(1, value)\n\
     return }\n";
  semantic_error "aggregate-assignment"
    "aggregate assignment is not supported; use `copy(dst, src)`"
    "struct S { x i64 }\n\
     fn f() void { source S = (S){1}\n\
     destination S = (S){2}\n\
     destination = source\n\
     return }\n";
  ignore
    (lower_of
       "struct S { x i64 }\nfn f(p bool) i64 { s S\nif p { s.x = 1 }\nreturn s.x }\n");
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
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f(n i64) i64 { s S\n\
       \ switch n {\n\
       \ case 0: { s.x = 1 }\n\
       \ default: { s.y = 2 }\n\
       \ }\n\
       \ return s.x }\n");
  ignore
    (lower_of
       "fn f(i i64) i64 { a arr[2,arr[2,i64]]\n\
       \ a[0][0] = 1\n\
       \ a[0][1] = 2\n\
       \ return a[0][i] }\n");
  ignore
    (lower_of "fn f(i i64) i64 { a arr[2,arr[2,i64]]\na[0][0] = 1\nreturn a[0][i] }\n");
  ignore
    (lower_of "fn f(i i64) i64 { v vec[2,i64]\n v[0] = 1\n v[1] = 2\n return v[i] }\n");
  semantic_error "aggregate-dynamic-index" "use of uninitialized local `a`"
    "fn f() i64 { a arr[2,i64]\n i i64 = 0\n return a[i] }\n";
  semantic_error "aggregate-dynamic-write-keeps-other-element-uninitialized"
    "use of uninitialized local `a`"
    "fn f() i64 { a arr[2,i64]\n a[0] = 1\n return a[1] }\n";
  semantic_error "aggregate-loop-read-before-write" "use of uninitialized local `a`"
    "fn f() i64 { a arr[2,i64]\n return a[0] }\n";
  ignore (lower_of "fn f(i i64) i64 { a arr[2,i64]\na[i] = 1\nreturn a[i] }\n");
  ignore
    (lower_of
       "fn f() u32 { a arr[4,u32]\n\
        for i u32 = 0; i < 4; i += 1 { a[i] = i }\n\
        return a[2] }\n");
  ignore
    (lower_of
       "struct S { a arr[4,u32] }\n\
        fn f() u32 { s S\n\
        for i u32 = 0; i < 4; i += 1 { s.a[i] = i }\n\
        return s.a[2] }\n");
  ignore
    (lower_of
       "fn take(p addr) void { return }\n\
        fn f() u32 { a arr[2,u32]\n\
       \ take(&a)\n\
       \ return a[1] }\n");
  ignore
    (lower_of "fn f(i i64) i64 { a arr[2,i64]\n a[0] = 1\n a[1] = 2\n return a[i] }\n");
  ignore
    (lower_of
       "struct S { a arr[2,i64] z i64 }\n\
        fn f(i i64) i64 { s S\n\
       \ s.a[0] = 1\n\
       \ s.a[1] = 2\n\
       \ return s.a[i] }\n");
  ignore
    (lower_of
       "fn f(i i64) i64 { x i64\n\
        a arr[2,addr]\n\
        a[0] = &x\n\
        a[i][i64,0] = 1\n\
        return x }\n");
  ignore
    (llvm_of
       "fn take(p addr) void { return }\nfn f() i64 { x i64\n take(&x)\n return x }\n");
  semantic_error "field-escape-preserves-sibling-initialization"
    "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn take(p addr) void { return }\n\
     fn f() i64 { s S\n\
     take(&s.x)\n\
     t S\n\
     copy(t, s)\n\
     return 0 }\n";
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f(p bool) i64 { s S\n\
        if p { s.x = 1 } else { s.y = 2 }\n\
       \ return s.x }\n");
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f(p bool) i64 { s S\n\
        if p { s.x = 1 } else { return 0 }\n\
       \ return s.x }\n");
  ignore
    (lower_of
       "struct S { x i64 }\n\
        fn f(p bool) i64 { s S\n\
       \ while p { s.x = 1 }\n\
       \ return s.x }\n");
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn take(p addr) void { return }\n\
        fn f(p bool) i64 { s S\n\
       \ if p { take(&s) } else { s.x = 1 }\n\
        return s.x }\n");
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn take(p addr) void { return }\n\
        fn f(p bool) i64 { s S\n\
       \ if p { take(&s) } else { s.x = 1 }\n\
        return s.y }\n");
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn take(p addr) void { return }\n\
        fn f(p bool) i64 { s S\n\
       \ if p { take(&s) } else { s.x = 1 }\n\
        t S\n\
        copy(t, s)\n\
       \ return 0 }\n");
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
  ignore
    (lower_of
       "fn take(value i64) void { return }\n\
        fn f(condition bool) void { for value i64; true; take(value) {\n\
       \ if condition { value = 1 } else { continue }\n\
       \ } }\n");
  ignore
    (lower_of
       "fn f() i64 { value i64\n while true { value = 1\n break }\n return value }\n");
  ignore
    (lower_of
       "fn f() i64 { value i64\n\
       \ while true { defer { value = 1 }\n\
       \ break }\n\
       \ return value }\n");
  ignore
    (lower_of
       "fn f(condition bool) i64 { value i64\n\
       \ while condition { value = 1\n\
       \ break }\n\
       \ return value }\n");
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
  ignore
    (lower_of
       "fn f(choice i64) i64 { value i64\n\
       \ while true { switch choice {\n\
       \ case 0: { value = 1\n\
       \ break }\n\
       \ default: { break }\n\
       \ } }\n\
       \ return value }\n");
  semantic_error "nested-defer" "nested defer is not allowed"
    "fn f() void { defer { defer { } } }\n";
  semantic_error "defer-return" "return is not allowed inside defer"
    "fn f() void { defer { return } }\n";
  semantic_error "defer-break" "break is not allowed inside defer"
    "fn f() void { while true { defer { break } break } }\n";
  semantic_error "defer-continue" "continue is not allowed inside defer"
    "fn f() void { while true { defer { continue } break } }\n";
  semantic_error "raw-uninitialized-array-read" "use of uninitialized local `x`"
    "fn f() u32 { x arr[4,u32]\n x[0] = 1\n return x[3] }\n";
  semantic_error "raw-uninitialized-address-read" "use of uninitialized local `p`"
    "fn f() i64 { p addr\n p[i64] = 1\n return p[i64] }\n";
  semantic_error "raw-uninitialized-array-copy" "use of uninitialized local `x`"
    "fn f() u32 { x arr[4,u32]\n t arr[4,u32]\n copy(t, x)\n return t[0] }\n";
  ignore
    (lower_of
       "fn f() u32 { x arr[4,u32]\n\
       \ for i u32 = 0; i < 4; i += 1 { x[i] = i }\n\
       \ return x[0] }\n");
  semantic_error "raw-branch-uninitialized-element" "use of uninitialized local `x`"
    "fn f(p bool) u32 { x arr[4,u32]\n if p { x[0] = 1 }\n return x[1] }\n";
  semantic_error "raw-defer-read-before-write" "use of uninitialized local `x`"
    "fn f() u64 { x u64\n defer { x = 1 }\n return x }\n";
  semantic_error "raw-uninitialized-struct-field" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\nfn f() i64 { s S\n s.x = 1\n return s.y }\n";
  semantic_error "raw-uninitialized-vector-lane" "use of uninitialized local `v`"
    "fn f() u32 { v vec[4,u32]\n v[0] = 1\n return v[3] }\n";
  semantic_error "raw-uninitialized-scalar-read" "use of uninitialized local `x`"
    "fn f() i64 { x i64\n return x }\n";
  ignore (lower_of "fn f() i64 { x i64\n x = 5\n return x }\n");
  let uninitialized_no_zero =
    llvm_of
      "fn f() u32 { x arr[4,u32]\n\
      \ for i u32 = 0; i < 4; i += 1 { x[i] = i }\n\
      \ return x[3] }\n"
  in
  if
    contains uninitialized_no_zero "memset"
    || contains uninitialized_no_zero "zeroinitializer"
  then failwith "plain declaration emitted implicit initialization";
  parse_error_message "raw-declaration-removed"
    "`= raw` is no longer supported; declare `x T` without an initializer"
    "fn f() i64 { x i64 = raw\n return 0 }\n";
  semantic_error "raw-not-a-value-return" "unknown name `raw`"
    "fn f() i64 { return raw }\n";
  semantic_error "raw-not-a-value-call" "unknown name `raw`"
    "fn g(x i64) i64 { return x }\nfn f() i64 { return g(raw) }\n";
  semantic_error "raw-not-a-value-assign" "unknown name `raw`"
    "fn f() i64 { x i64 = 1\n x = raw\n return x }\n";
  parse_error_message "raw-init-trailing"
    "`= raw` is no longer supported; declare `x T` without an initializer"
    "fn f() i64 { x i64 = raw + 1\n return x }\n";
  ignore (lower_of "fn f() i64 { raw i64 = 3\n return raw }\n");
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
       "fn delayed(out addr, value i64) void {\n\
       \ defer { out[i64] += value }\n\
       \ { value i64 = 100\n\
       \ out[i64] += value - 100 }\n\
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
    "fn target() i32 { return 1 }\n\
     fn check_case() i32 { target i32 = 2\n\
    \ return target() }\n";
  semantic_error "lexical-scope-generic-call-shadow" "value, not a function"
    "fn target[N const i32]() i32 { return N }\n\
     fn check_case() i32 { target i32 = 2\n\
    \ return target[1]() }\n";
  semantic_error "lexical-scope-type-shadow" "value, not a type"
    "struct Item { value i32 }\nfn check_case(Item i32) i32 { local Item\n return 0 }\n";
  semantic_error "lexical-scope-aggregate-length-shadow" "not a compile-time constant"
    "const Count usize = 2\n\
     fn check_case(Count usize) i32 { local arr[Count,i32]\n\
    \ return 0 }\n";
  ignore
    (lower_of
       "const Index i32 = 0\n\
        fn read(Index i32) i32 { values arr[2,i32]\n\
       \ values[0] = 7\n\
       \ return values[Index] }\n\
        fn main() i32 { return read(1) }\n");
  ignore
    (lower_of
       "const Index i32 = 0\n\
        fn read(Index i32) i32 { values arr[2,i32]\n\
       \ values[0] = 7\n\
       \ return values[Index + 0] }\n\
        fn main() i32 { return read(1) }\n");
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
  semantic_error "switch-duplicate-case" "duplicate case label `1`"
    "fn f(x i32) i32 { switch x { case 1: return 1; case 1: return 2 } }\n";
  semantic_error "switch-nonconst-case" "case label must be a compile-time constant"
    "fn f(x i32, y i32) i32 { switch x { case y: return 1 } return 0 }\n";
  semantic_error "switch-vec-scrutinee" "switch scrutinee must be an integer or bool"
    "fn f(v vec[2,i32]) i32 { switch v { case 1: return 1 } return 0 }\n";
  semantic_error "switch-bool-exhaustive-still-needs-return"
    "may reach the end without returning"
    "fn f(x bool) i32 { switch x { case true: return 1; case false: return 2 } }\n";
  semantic_error "addr-handle-nonopaque-target"
    "handle type argument must be an opaque type"
    "struct S { x u8 }\n\
     fn f(p addr) addr { return handle_addr(handle_from_addr[S](p)) }\n";
  semantic_error "addr-handle-nonopaque-wrapper"
    "handle type argument must be an opaque type"
    "opaque O\nfn f(p addr) addr { return handle_from_addr[handle[O]](p) }\n";
  semantic_error "addr-handle-wrong-arg" "handle_from_addr argument must be an addr"
    "opaque O\n\
     fn f(n usize) usize { return addr_bits(handle_addr(handle_from_addr[O](n))) }\n";
  semantic_error "addr-bits-wrong-arg" "addr_bits argument must be an addr"
    "fn f(n u32) usize { return addr_bits(n) }\n";
  semantic_error "addr-from-bits-wrong-arg" "addr_from_bits argument must be a usize"
    "fn f(n u32) addr { return addr_from_bits(n) }\n";
  semantic_error "handle-addr-wrong-arg" "handle_addr argument must be a handle"
    "fn f(p addr) addr { return handle_addr(p) }\n";
  semantic_error "handle-from-addr-bare"
    "builtin `handle_from_addr` expects a type argument"
    "fn f(p addr) addr { return handle_from_addr(p) }\n";
  semantic_error "addr-const-context" "expression is not compile-time constant"
    "opaque O\nconst X addr = handle_from_addr[O](addr_from_bits(0))\n";
  semantic_error "addr-vec-element"
    "vector element type must be a scalar (bool, integer, or pointer)"
    "fn f(v vec[2,addr]) usize { return 0 }\n";
  let addr_equality_path = llvm_of "fn f(a addr, b addr) bool { return a == b }\n" in
  List.iter
    (fun marker ->
      if not (contains addr_equality_path marker) then
        failwith ("addr-equality-path: missing `" ^ marker ^ "`"))
    [ "ptrtoint ptr"; "icmp eq i64" ];
  let condition_forms =
    llvm_of
      "fn f(x bool, y u32, z u32) void {\n\
      \  if !(x) { return }\n\
      \  if !(y < z) { return }\n\
      \  while !(y == z) { return }\n\
      \  return\n\
       }\n"
  in
  List.iter
    (fun marker ->
      if not (contains condition_forms marker) then
        failwith ("condition-forms: missing `" ^ marker ^ "`"))
    [ "icmp ult"; "icmp eq" ];
  let nested_struct_literal_path =
    llvm_of
      "struct S { a u32 }\n\
       fn is(x u32) bool { return x == 1 }\n\
       fn f(x u32) void {\n\
      \  literal S = (S) { x }\n\
      \  if is(literal.a) { return }\n\
      \  return\n\
       }\n"
  in
  List.iter
    (fun marker ->
      if not (contains nested_struct_literal_path marker) then
        failwith ("nested-struct-literal-path: missing `" ^ marker ^ "`"))
    [ "%struct.S"; "store i32" ];
  let null_contexts =
    llvm_of
      "opaque O\n\
       const null_addr addr = null\n\
       const null_handle handle[O] = null\n\
       const addr_is_null bool = null_addr == null\n\
       const handle_is_null bool = null == null_handle\n\
       fn return_addr() addr { return null }\n\
       fn return_handle() handle[O] { return null }\n\
       fn local_nulls() bool { p addr = null\n\
      \ h handle[O] = null\n\
      \ return p == null && h == null }\n\
       fn call_addr() bool { return addr_equal(null) }\n\
       fn addr_equal(a addr) bool { return null == a }\n\
       fn handle_null_left(a handle[O]) bool { return null != a }\n\
       fn handle_equal(a handle[O], b handle[O]) bool { return a == b }\n\
       fn addr_constant() bool { return addr_is_null }\n\
       fn handle_constant() bool { return handle_is_null }\n"
  in
  List.iter
    (fun marker ->
      if not (contains null_contexts marker) then
        failwith ("null-contexts: missing `" ^ marker ^ "`"))
    [ "ret ptr null"; "icmp eq ptr"; "ret i1 true" ];
  semantic_error "handle-order-reject" "ordered comparison requires integer operands"
    "opaque O\nfn f(a handle[O], b handle[O]) bool { return a < b }\n";
  semantic_error "handle-arithmetic-reject"
    "arithmetic requires integer or vector operands"
    "opaque O\nfn f(a handle[O], b handle[O]) handle[O] { return a + b }\n";
  semantic_error "cross-handle-equality-reject"
    "binary operands must have the same type"
    "opaque O\nopaque P\nfn f(a handle[O], b handle[P]) bool { return a == b }\n";
  semantic_error "handle-null-unconstrained" "null requires an addr or handle context"
    "opaque O\nfn f() bool { return null == null }\n";
  semantic_error "addr-index-reject" "raw selection requires a type argument"
    "fn f(p addr) u8 { return p[0] }\n";
  parse_error_message "removed-typed-pointer"
    "typed pointers are no longer supported; use addr or handle[T]"
    "fn f() void { p ptr[u8]\n return }\n";
  parse_error_message "removed-pointer-dereference"
    "pointer dereference is no longer supported; use raw selection"
    "fn f(p addr) addr { return p.* }\n";
  parse_error_message "removed-ptr-add"
    "ptr_add is no longer supported; use address arithmetic"
    "fn f(p addr, n usize) addr { return ptr_add(p, n) }\n";
  parse_error_message "removed-ptr-add-bytes"
    "ptr_add_bytes is no longer supported; use address arithmetic"
    "fn f(p addr, n usize) addr { return ptr_add_bytes(p, n) }\n";
  semantic_error "addr-bitcast-from-reject"
    "illegal cast for source and destination widths"
    "fn f(p addr) usize { return bitcast[usize](p) }\n";
  semantic_error "addr-bitcast-to-reject"
    "illegal cast for source and destination widths"
    "fn f(n usize) addr { return bitcast[addr](n) }\n";
  semantic_error "handle-field-reject" "field access requires a struct"
    "opaque O\nfn f(h handle[O]) usize { return h.x }\n";
  semantic_error "handle-index-reject" "cannot select through a handle"
    "opaque O\nfn f(h handle[O]) u8 { return h[0] }\n";
  semantic_error "comparison-chaining-reject" "binary operands must have the same type"
    "fn f(a i32, b i32, c i32) bool { return a < b < c }\n";
  semantic_error "conditional-defer-divergence" "may reach the end without returning"
    "fn finish(choice bool) i32 { defer { if choice { while true { } } } }\n";
  semantic_error "unreached-defer-does-not-consume-break"
    "may reach the end without returning"
    "fn spin() i32 { while true { break\n defer { while true { } } } }\n";
  semantic_error "extern-c-struct-parameter"
    "aggregate parameter `value` cannot be passed by value; pass `&x` as `addr` or \
     `handle[T]`"
    "struct S { x i64 }\nextern \"C\" { fn take(value S) void }\n";
  semantic_error "extern-c-array-parameter"
    "aggregate parameter `value` cannot be passed by value; pass `&x` as `addr` or \
     `handle[T]`"
    "extern \"C\" { fn take(value arr[2,i64]) void }\n";
  semantic_error "extern-c-struct-return"
    "aggregate result cannot be returned by value; pass destination storage as `addr` \
     or `handle[T]`"
    "struct S { x i64 }\nextern \"C\" { fn make() S }\n";
  semantic_error "extern-c-vector-parameter"
    "cannot use `vec[4, i32]` by value; use a pointer"
    "extern \"C\" { fn take(value vec[4,i32]) void }\n";
  semantic_error "extern-c-array-return"
    "aggregate result cannot be returned by value; pass destination storage as `addr` \
     or `handle[T]`"
    "extern \"C\" { fn make() arr[2,i64] }\n";
  semantic_error "extern-c-opaque-parameter"
    "opaque type `Handle` may only be used behind a pointer"
    "opaque Handle\nextern \"C\" { fn take(value Handle) void }\n";
  semantic_error "extern-c-definition-fallthrough" "may reach the end without returning"
    "extern \"C\" { fn value() i64 { } }\n";
  semantic_error "extern-c-definition-struct-parameter"
    "aggregate parameter `value` cannot be passed by value; pass `&x` as `addr` or \
     `handle[T]`"
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
       fn check_struct(p addr) bool { return p[S].x == 1 && p[S].y == 2 }\n\
       fn check_array(p addr) bool { return p[i64,0] == 1 && p[i64,1] == 2 }\n\
       fn pass_vector(value vec[3,i32]) vec[3,i32] { return value }\n\
       fn main() i32 {\n\
       literal S = (S){1, 2}\n\
       if !check_struct(&literal) { return 1 }\n\
       a arr[2,i64]\n\
       copy(a, A)\n\
       if !check_array(&a) { return 1 }\n\
       v vec[3,i32] = pass_vector(splat(3))\n\
       return zext[i32](v[0] == 3)\n\
       }\n"
  in
  List.iter
    (fun expected ->
      if not (contains internal_aggregate_abi expected) then
        failwith ("internal-aggregate-abi: missing `" ^ expected ^ "`"))
    [
      "call i1 @check_struct(ptr";
      "call i1 @check_array(ptr";
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
         (source "fn check_case(value handle[Handle]) i64 { return 0 }\nopaque Handle\n"))
  in
  (match neutral_named_type.Ast.items with
  | Ast.Func { params = [ { ty = Ast.Handle (Ast.Named_type "Handle"); _ } ]; _ } :: _
    ->
      ()
  | _ -> failwith "named-type-neutral-ast: parser classified a declaration name");
  ignore
    (lower_of "fn check_case(value handle[Handle]) i64 { return 0 }\nopaque Handle\n");
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
      "fn read(object handle[Handle]) i64 { value Pair = (Pair){11}\n\
      \ return value.x }\n" )
  in
  let declarations_file = ("types.fas", "opaque Handle\nstruct Pair { x i64 }\n") in
  List.iter
    (fun files -> ignore (expect_ok (check_files files) |> Lower.lower |> expect_ok))
    [ [ use_file; declarations_file ]; [ declarations_file; use_file ] ];
  semantic_error "unknown-named-type" "unknown type `Missing`"
    "fn check_case(value handle[Missing]) i64 { return 0 }\n";
  semantic_error "opaque-struct-literal" "opaque type `Handle` is not a struct"
    "opaque Handle\nfn check_case() i64 { (Handle){}\n return 0 }\n";
  ignore
    (lower_of
       "fn preserve(value handle[Handle]) handle[Handle] { local handle[Handle] = value\n\
       \ return local }\n\
        fn read_only(value handle[Handle]) addr { return handle_addr(value) }\n\
        fn pointer_size() usize { return sizeof[handle[Handle]] }\n\
        fn pointer_align() usize { return alignof[addr] }\n\
        opaque Handle\n");
  semantic_error "opaque-local-by-value"
    "opaque type `Handle` may only be used behind a pointer"
    "opaque Handle\nfn check_case() void { value Handle }\n";
  semantic_error "opaque-struct-field-by-value" "opaque type `Handle` has no layout"
    "opaque Handle\nstruct Wrapper { value Handle }\n";
  semantic_error "opaque-array-by-value"
    "opaque type `Handle` may only be used behind a pointer"
    "opaque Handle\nfn check_case() void { values arr[2,Handle] }\n";
  semantic_error "opaque-vector-by-value" "vector element type must be a scalar"
    "opaque Handle\nfn check_case() void { values vec[2,Handle] }\n";
  semantic_error "opaque-parameter-by-value"
    "opaque type `Handle` may only be used behind a pointer"
    "opaque Handle\nfn check_case(value Handle) void { return }\n";
  semantic_error "opaque-return-by-value"
    "opaque type `Handle` may only be used behind a pointer"
    "opaque Handle\nfn check_case() Handle { }\n";
  semantic_error "opaque-sizeof" "opaque type `Handle` has no layout"
    "opaque Handle\nfn check_case() usize { return sizeof[Handle] }\n";
  semantic_error "opaque-alignof" "opaque type `Handle` has no layout"
    "opaque Handle\nfn check_case() usize { return alignof[Handle] }\n";
  semantic_error "opaque-implicit-erasure"
    "type mismatch: expected addr, got handle[Handle]"
    "opaque Handle\nfn check_case(value handle[Handle]) addr { return value }\n";
  ignore
    (lower_of
       "fn accept(value addr) void { return }\n\
        fn return_read_only(value addr) addr { return value }\n\
        fn choose(flag bool, mutable addr, read_only addr) addr {\n\
       \ return flag ? mutable : read_only\n\
        }\n\
        fn check_case(value addr) bool {\n\
       \ read_only addr = value\n\
       \ accept(value)\n\
       \ read_only = value\n\
       \ return value == read_only\n\
        }\n");
  semantic_error "pointer-implicit-to-integer" "type mismatch: expected usize, got addr"
    "fn check_case(value addr) void { bits usize = value }\n";
  semantic_error "integer-implicit-to-pointer" "type mismatch: expected addr, got usize"
    "fn check_case(value usize) void { pointer addr = value }\n";
  semantic_error "pointer-bitcast-discards-const"
    "illegal cast for source and destination widths"
    "fn check_case(value addr) addr { return bitcast[addr](value) }\n";
  semantic_error "pointer-bitcast-u32-width"
    "illegal cast for source and destination widths"
    "fn check_case(value addr) u32 { return bitcast[u32](value) }\n";
  semantic_error "pointer-bitcast-i32-width"
    "illegal cast for source and destination widths"
    "fn check_case(value i32) addr { return bitcast[addr](value) }\n";
  semantic_error "const-pointer-bitcast-u32-width"
    "illegal cast for source and destination widths"
    "fn check_case(value addr) u32 { return bitcast[u32](value) }\n";
  semantic_error "integer-bitcast-const-pointer-u32-width"
    "illegal cast for source and destination widths"
    "fn check_case(value u32) addr { return bitcast[addr](value) }\n";
  let before_messages =
    semantic_messages
      "struct Stable { x i64 }\nfn check_case(value Missing) i64 { return 0 }\n"
  in
  let after_messages =
    semantic_messages
      "fn check_case(value Missing) i64 { return 0 }\nstruct Stable { x i64 }\n"
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
  parse_error "fas-006-noalias-parameter" "fn f(x noalias addr) i32 { return 0 }\n";
  parse_error "fas-007-aligned-parameter" "fn f(x aligned[16] addr) i32 { return 0 }\n";
  let explicit_pointer_cast =
    llvm_of
      "fn take(p addr) i32 { return 0 }\n\
       fn main() i32 { x i64 = 1\n\
      \ return take(addr_from_bits(addr_bits(&x))) }\n"
  in
  if not (contains explicit_pointer_cast "call i32 @take(ptr") then
    failwith "fas-028: explicit pointer conversion was rejected";
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
  cli_error "removed-ast-output" "unknown option: --emit-ast"
    [ "--emit-ast"; "profile.fas" ];
  cli_error "removed-release-option" "unknown option: -release"
    [ "-release"; "profile.fas" ];
  cli_error "removed-kernel-option" "unknown option: -kernel"
    [ "-kernel"; "profile.fas" ];
  cli_error "link-input-needs-executable"
    "C inputs and link flags require an executable output"
    [ "--emit-llvm"; "profile.fas"; "helper.c" ];
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
    "fn check_case() i64 { return identity[i64](7) }\n\
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
      output_string channel "fn main() i32 {\n  s addr = \"";
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
  let const_array_address =
    llvm_of
      "const K arr[2,u32] = {4, 5}\n\
       fn pointer() addr { return &K }\n\
       fn main() u32 { return K[0] }\n"
  in
  if
    (not
       (contains const_array_address
          "@K = private unnamed_addr constant [2 x i32] [i32 4, i32 5]"))
    || not (contains const_array_address "getelementptr [2 x i32], ptr @K")
  then failwith "named-aggregate-constant-address: storage is not static and readonly";
  semantic_error "named-aggregate-constant-view-write" "cannot modify constant"
    "const K arr[2,u32] = {4, 5}\n\
     fn main() void { view values = K\n\
    \ values[0] = 8\n\
    \ return }\n";
  semantic_error "scalar-constant-address" "constant `K` is not a place"
    "const K u32 = 4\nfn main() addr { return &K }\n";
  semantic_error "fas-029-string-literal-index" "cannot modify read-only pointer"
    "fn main() i32 { \"x\"[u8] = 9\n return 0 }\n";
  let string_literals =
    llvm_of
      "extern \"C\" { fn take(p addr) void }\n\
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
      "fn read(p addr) i32 { return zext[i32](p[u8]) }\n\
       fn main() i32 { return read(\"x\") }\n"
  in
  if not (contains string_pointer "call i32 @read(ptr") then
    failwith "fas-030-string-literals: read-only pointer call was rejected";
  let byte_literal_semantics =
    llvm_of
      "const ByteCount usize = len(\"a\\0b\") + len(\"\\n\") + len(\"é\")\n\
       const CByteCount usize = len(c\"fas\")\n\
       fn bytes() addr { return \"a\\0b\" }\n\
       fn cbytes() addr { return c\"fas\" }\n\
       fn main() usize { return ByteCount }\n"
  in
  if not (contains byte_literal_semantics "[3 x i8] c\"a\\00b\"") then
    failwith "fas-030-string-literals: ordinary embedded NUL was not preserved";
  if
    (not (contains byte_literal_semantics "[4 x i8] c\"fas\\00\""))
    || not (contains byte_literal_semantics "ret i64 6\n")
  then failwith "fas-030-string-literals: literal length did not count decoded bytes";
  let hexadecimal_byte_literals =
    llvm_of
      "const HexLen usize = len(\"\\x41\\x42\") + len(\"\\x414\")\n\
       fn hex_bytes() addr { return \"\\x41\\x00\\x42\" }\n\
       fn fixed_bytes() addr { return \"\\x414\" }\n\
       fn main() usize { return HexLen }\n"
  in
  if
    (not (contains hexadecimal_byte_literals "[3 x i8] c\"A\\00B\""))
    || (not (contains hexadecimal_byte_literals "[2 x i8] c\"A4\""))
    || not (contains hexadecimal_byte_literals "ret i64 4\n")
  then failwith "hex-byte-escape: decoded bytes or fixed width changed";
  let char_literal body = "'" ^ body ^ "'" in
  let slash = "\\" in
  let escaped_characters =
    List.map
      (fun (body, value) -> (char_literal body, value))
      [
        (slash ^ "n", 10);
        (slash ^ "r", 13);
        (slash ^ "t", 9);
        (slash ^ slash, 92);
        (slash ^ "'", 39);
        (slash ^ "\"", 34);
        (slash ^ "0", 0);
        (slash ^ "x41", 65);
        (slash ^ "xFF", 255);
      ]
  in
  let printable_characters =
    List.init 95 (fun index -> index + 32)
    |> List.filter (fun value -> value <> 39 && value <> 92)
    |> List.map (fun value -> (char_literal (String.make 1 (Char.chr value)), value))
  in
  let all_characters = escaped_characters @ printable_characters in
  let character_bytes =
    Printf.sprintf
      "const CharacterBytes arr[%d,u8] = {%s}\n\
       fn main() u8 { return CharacterBytes[0] }\n"
      (List.length all_characters)
      (all_characters |> List.map fst |> String.concat ", ")
  in
  semantic_accept "character-literal-all-values" character_bytes;
  let character_byte_llvm = llvm_of character_bytes in
  if
    not
      (contains character_byte_llvm
         (Printf.sprintf "[%d x i8] [%s]" (List.length all_characters)
            (String.concat ", "
               (List.map
                  (fun (_, value) -> Printf.sprintf "i8 %d" value)
                  all_characters))))
  then failwith "character-literal-values: byte values changed";
  let character_contexts =
    "const CharacterConst i32 = 'a'\n\
     fn compare_byte(value u8) bool { return value == 'a' }\n\
     fn case_byte(value u8) bool {\n\
     switch value {\n\
     case 'a': return true\n\
     default: return false\n\
     }\n\
     }\n\
     fn main() i32 {\n\
     a u8 = 'a'\n\
     b i8 = 'a'\n\
     c i32 = 'a'\n\
     d u64 = 'a'\n\
     e usize = 'a'\n\
     values arr[1,u8] = {'a'}\n\
     if compare_byte(values[0]) && case_byte(a) && CharacterConst == c && b == 97 && d \
     == 97 && e == 97 { return 0 }\n\
     return 1\n\
     }\n"
  in
  semantic_accept "character-literal-contexts" character_contexts;
  let negative_character = llvm_of "fn main() i32 { return -'a' }\n" in
  if not (contains negative_character "ret i32 4294967199\n") then
    failwith "character-literal-unary-minus: expected -97";
  semantic_error "character-literal-i8-overflow"
    "integer literal is out of range for i8"
    "const Invalid i8 = '\\xFF'\nfn main() i8 { return Invalid }\n";
  let apostrophe_escape_string =
    llvm_of
      "fn read(p addr) u8 { return p[u8,2] }\n\
       fn main() u8 { return read(\"it\\'s\") }\n"
  in
  if not (contains apostrophe_escape_string "[4 x i8] c\"it's\"") then
    failwith "string-apostrophe-escape: decoded bytes changed";
  semantic_error "fas-030-string-literal-fixed-array"
    "type mismatch: expected arr[3, u8], got addr"
    "fn main() i32 { bytes arr[3,u8] = \"abc\"\n return 0 }\n";
  semantic_error "fas-030-c-string-literal-nul"
    "C string literal cannot contain embedded NUL"
    "fn main() i32 { c\"a\\0b\"[0]\n return 0 }\n";
  semantic_error "fas-030-const-c-string-literal-nul"
    "C string literal cannot contain embedded NUL"
    "const N usize = len(c\"a\\0b\")\nfn main() usize { return N }\n";
  semantic_error "fas-030-runtime-len-c-string-literal-nul"
    "C string literal cannot contain embedded NUL"
    "fn main() usize { return len(c\"a\\0b\") }\n";
  semantic_error "fas-030-c-string-literal-hex-nul"
    "C string literal cannot contain embedded NUL"
    "fn main() i32 { c\"a\\x00b\"[0]\n return 0 }\n";
  semantic_error "fas-030-const-c-string-literal-hex-nul"
    "C string literal cannot contain embedded NUL"
    "const N usize = len(c\"a\\x00b\")\nfn main() usize { return N }\n";
  semantic_error "fas-030-runtime-len-c-string-literal-hex-nul"
    "C string literal cannot contain embedded NUL"
    "fn main() usize { return len(c\"a\\x00b\") }\n";
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
  let c_literal_embedded_payload_length =
    llvm_of "fn main() i64 { return bitcast[i64](len(c\"abc\")) }\n"
  in
  if not (contains c_literal_embedded_payload_length "ret i64 3\n") then
    failwith "fas-031-len: C literal payload length included a terminator";
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
    "fn main() i64 { p addr = \"abc\"\n return zext[i64](len(p)) }\n";
  let const_array_value =
    llvm_of
      "const G arr[2, i64] = {7, 8}\n\
       fn take(p addr) i64 { return p[i64,0] + p[i64,1] }\n\
       fn main() i64 { a arr[2, i64]\n\
      \ copy(a, G)\n\
      \ return take(&a) }\n"
  in
  if
    contains const_array_value "load [2 x i64], ptr"
    || contains const_array_value "store [2 x i64] ptr"
  then failwith "fas-013: const array was lowered as an implicit aggregate value";

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
    "const F i64 = 1\nfn check_case() i64 { return F() }\n";
  semantic_error "function-used-as-value" "function, not a value"
    "fn F() i64 { return 1 }\nfn check_case() i64 { return F }\n";
  semantic_error "type-used-as-value" "type, not a value"
    "opaque F\nfn check_case() i64 { return F }\n";
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

  ignore
    (lower_of "fn f() i64 { y i64\n for i i32 = 0; i < 0; y = 5 { }\n return y }\n");

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
  semantic_error "vec-element-address"
    "vector element type must be a scalar (bool, integer, or pointer)"
    "fn f() i64 { v vec[2,addr] = splat(addr_from_bits(0))\n\
    \                    return 0 }\n";
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

  (let ir =
     llvm_of
       "fn f() i64 { b vec[4,bool] = splat(true)\n\
        \032             d vec[4,bool] = b & b\n\
        \032             return 0 }\n"
   in
   if not (contains ir "and <4 x i1>") then
     failwith "bool-vec-arith: splat bitand drifted");

  List.iter
    (fun name ->
      semantic_error ("reserved-builtin-" ^ name)
        (Printf.sprintf "`%s` is reserved and cannot be used as a binding" name)
        (Printf.sprintf "fn %s(x i64) i64 { return x }\n" name))
    Names.operation_names;

  (match parse_messages "use \"C\"\n" with
  | [ message ] when message = "use \"C\" is not implemented until v0.2" -> ()
  | messages ->
      failwith ("use-c-rejection: unexpected diagnostics " ^ String.concat "; " messages));
  incr checks_run;
  (match (parse_file "quoted-use.fas" "use \"C\" \"local.h\"\n").Ast.items with
  | [ Ast.Use { path = "C"; c_header = Some (Ast.C_quoted "local.h"); _ } ] -> ()
  | _ -> failwith "use-c-quoted: parser did not preserve the quoted header");
  incr checks_run;
  (match (parse_file "angle-use.fas" "use \"C\" <system.h>\n").Ast.items with
  | [ Ast.Use { path = "C"; c_header = Some (Ast.C_system "system.h"); _ } ] -> ()
  | _ -> failwith "use-c-angle: parser did not preserve the system header");
  let raw_container =
    parse_file "raw-container.fas"
      "  use \"C\" <<END\n/* } { */\nconst char *s = \"# Fas text\";\nEND\n"
  in
  (match raw_container.Ast.items with
  | [ Ast.Use { c_header = Some (Ast.C_fragment fragment); span; _ } ]
    when fragment.tag = "END"
         && fragment.text = "/* } { */\nconst char *s = \"# Fas text\";\n"
         && span.Span.line = 1 ->
      ()
  | _ -> failwith "use-c-container: raw text or source line was not preserved");
  let raw_crlf =
    parse_file "raw-container-crlf.fas" "use \"C\" <<E\r\nint raw_fragment;\r\nE"
  in
  (match raw_crlf.Ast.items with
  | [ Ast.Use { c_header = Some (Ast.C_fragment fragment); span; _ } ]
    when fragment.text = "int raw_fragment;\r\n" && span.Span.line = 1 ->
      ()
  | _ -> failwith "use-c-container-crlf: CRLF or EOF terminator was not accepted");
  parse_message "use-c-container-missing-terminator"
    "C container is missing terminator `END`" "use \"C\" <<END\nint value;\n";
  parse_message "use-c-container-invalid-tag"
    "C container tag must be an ASCII identifier" "use \"C\" <<9END\n9END\n";
  parse_message "use-c-container-trailing-opener-text"
    "trailing text after C container tag" "use \"C\" <<END trailing\nEND\n";
  parse_message "use-c-container-nested" "C container must be at top level"
    "fn main() i32 {\nuse \"C\" <<END\nint value;\nEND\nreturn 0 }\n";
  parse_message "use-c-container-terminator-trailing-space"
    "C container is missing terminator `END`" "use \"C\" <<END\nint value;\nEND \n";
  (match parse_messages "fn use() i32 { return 0 }\n" with
  | [ message ] when message = "expected identifier, found `use`" -> ()
  | messages ->
      failwith
        ("use-keyword-rejection: unexpected diagnostics " ^ String.concat "; " messages));
  if not (Names.reserved_binding_name "use") then
    failwith "use-keyword: use is not registered as a reserved binding";
  (match Driver.use_path_error "/opt/lib.fas" with
  | Some message
    when message
         = "absolute Fas dependency paths are not supported; use a path relative to \
            this file" ->
      ()
  | _ -> failwith "use-absolute-path: unexpected validation result");
  (match Driver.use_path_error "lib\000.fas" with
  | Some message when message = "Fas dependency paths cannot contain NUL bytes" -> ()
  | _ -> failwith "use-nul-path: unexpected validation result");
  (match Driver.use_path_error "lib.FAS" with
  | Some message
    when message
         = "Fas dependency paths must end in lowercase `.fas`; C headers use `use \
            \"C\"` in v0.2" ->
      ()
  | _ -> failwith "use-extension-case: unexpected validation result");
  (match Driver.use_path_error "lib.h" with
  | Some message
    when message
         = "Fas dependency paths must end in lowercase `.fas`; C headers use `use \
            \"C\"` in v0.2" ->
      ()
  | _ -> failwith "use-c-header-path: unexpected validation result");
  if Option.is_some (Driver.use_path_error "lib/../ops.fas") then
    failwith "use-relative-parent-path: approved relative path rejected";
  let use_limit_directory =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "fas-use-regressions-%d" (Unix.getpid ()))
  in
  Unix.mkdir use_limit_directory 0o700;
  let use_limit_root = Filename.concat use_limit_directory "root.fas" in
  let use_limit_child = Filename.concat use_limit_directory "child.fas" in
  let use_missing_root = Filename.concat use_limit_directory "missing-root.fas" in
  let use_directory = Filename.concat use_limit_directory "directory.fas" in
  let use_directory_root = Filename.concat use_limit_directory "directory-root.fas" in
  let use_duplicate_root = Filename.concat use_limit_directory "duplicate-root.fas" in
  let use_duplicate_one = Filename.concat use_limit_directory "one.fas" in
  let use_duplicate_two = Filename.concat use_limit_directory "two.fas" in
  let write_use_test_file path contents =
    let channel = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () -> output_string channel contents)
  in
  let remove_use_test_path path = try Sys.remove path with Sys_error _ -> () in
  let remove_use_test_directory path =
    try Unix.rmdir path with Unix.Unix_error _ -> ()
  in
  let use_limit_root_text = "use \"child.fas\"\nfn main() i32 { return 0 }\n" in
  write_use_test_file use_limit_root use_limit_root_text;
  write_use_test_file use_limit_child "fn child() i32 { return 0 }\n";
  Fun.protect
    ~finally:(fun () ->
      List.iter remove_use_test_path
        [
          use_limit_root;
          use_limit_child;
          use_missing_root;
          Filename.concat use_limit_directory "absent.fas";
          use_directory_root;
          use_duplicate_root;
          use_duplicate_one;
          use_duplicate_two;
        ];
      remove_use_test_directory use_directory;
      remove_use_test_directory use_limit_directory)
    (fun () ->
      let dependency_limit_error limits =
        match Driver.load_program ~limits use_limit_root with
        | Error [ diagnostic ] -> diagnostic.Diag.message
        | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
        | Ok _ -> failwith "dependency-closure-limit: expected rejection"
      in
      let file_error =
        dependency_limit_error { Limits.default with max_use_files = 1 }
      in
      if
        file_error
        <> "dependency closure exceeds budget max_use_files of 1 (profile 0.15)"
      then failwith ("dependency-file-limit: unexpected diagnostic " ^ file_error);
      let byte_error =
        dependency_limit_error
          { Limits.default with max_use_bytes = String.length use_limit_root_text }
      in
      if
        byte_error
        <> "dependency closure exceeds budget max_use_bytes of "
           ^ string_of_int (String.length use_limit_root_text)
           ^ " (profile 0.15)"
      then failwith ("dependency-byte-limit: unexpected diagnostic " ^ byte_error);
      let driver_error path =
        match Driver.run (cli_run [ path ]) with
        | Error [ diagnostic ] -> diagnostic
        | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
        | Ok _ -> failwith "use-diagnostic-pins: expected driver rejection"
      in
      write_use_test_file use_missing_root "use \"absent.fas\"\n";
      let missing = driver_error use_missing_root in
      if
        missing.Diag.message
        <> "cannot read Fas dependency "
           ^ Filename.concat use_limit_directory "absent.fas"
           ^ ": No such file or directory"
        || missing.primary.Span.file <> use_missing_root
        || missing.primary.Span.line <> 1
        || missing.primary.Span.column <> 1
        || missing.notes
           <> [
                "include chain: " ^ use_missing_root ^ " -> "
                ^ Filename.concat use_limit_directory "absent.fas";
              ]
      then failwith "use-missing-chain: diagnostic or include chain changed";
      Unix.mkdir use_directory 0o700;
      write_use_test_file use_directory_root "use \"directory.fas\"\n";
      let directory = driver_error use_directory_root in
      if
        directory.Diag.message <> "Fas dependency is a directory: " ^ use_directory
        || directory.notes
           <> [ "include chain: " ^ use_directory_root ^ " -> " ^ use_directory ]
      then failwith "use-directory-chain: diagnostic or include chain changed";
      write_use_test_file use_duplicate_root "use \"one.fas\"\nuse \"two.fas\"\n";
      write_use_test_file use_duplicate_one "fn duplicate() i32 { return 1 }\n";
      write_use_test_file use_duplicate_two "fn duplicate() i64 { return 2 }\n";
      let duplicate = driver_error use_duplicate_root in
      if
        duplicate.Diag.message <> "duplicate function `duplicate`"
        || duplicate.primary.Span.file <> use_duplicate_two
        || duplicate.primary.Span.line <> 1
        || duplicate.primary.Span.column <> 1
        || duplicate.notes
           <> [
                "first definition is at " ^ use_duplicate_one ^ ":1:1";
                "include chain: " ^ use_duplicate_root ^ " -> " ^ use_duplicate_one;
                "include chain: " ^ use_duplicate_root ^ " -> " ^ use_duplicate_two;
              ]
      then failwith "use-duplicate-sites: diagnostic or include chains changed");

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

  let exact_semantic_error name expected text =
    match semantic_messages text with
    | [ message ] when message = expected -> ()
    | messages ->
        failwith
          (name ^ ": expected `" ^ expected ^ "`, got " ^ String.concat "; " messages)
  in
  let reserved_float_names = Names.reserved_float_names in
  if reserved_float_names <> [ "f32"; "f64"; "sqrt"; "fma"; "floor"; "ceil"; "round" ]
  then failwith "reserved-float-names: registry changed";
  List.iter
    (fun name ->
      exact_semantic_error
        ("reserved-float-type-" ^ name)
        "reserved for v0.5 floating point"
        (Printf.sprintf "fn main() %s { return 1 }\n" name);
      exact_semantic_error
        ("reserved-float-call-" ^ name)
        "reserved for v0.5 floating point"
        (Printf.sprintf "fn main() i32 { return %s() }\n" name))
    reserved_float_names;
  List.iter
    (fun name ->
      exact_semantic_error
        ("reserved-float-function-" ^ name)
        (Printf.sprintf "`%s` is reserved and cannot be used as a binding" name)
        (Printf.sprintf "fn %s() i32 { return 0 }\n" name);
      if not (Names.reserved_binding_name name) then
        failwith ("reserved-float-binding-registry: " ^ name))
    reserved_float_names;
  List.iter
    (fun (name, binding_name, text) ->
      exact_semantic_error name
        (Printf.sprintf "`%s` is reserved and cannot be used as a binding" binding_name)
        text)
    [
      ("reserved-float-local", "f32", "fn main() i32 { f32 i32 = 1\n return f32 }\n");
      ("reserved-float-parameter", "f64", "fn main(f64 i32) i32 { return f64 }\n");
      ("reserved-float-function", "sqrt", "fn sqrt() i32 { return 0 }\n");
      ("reserved-float-struct", "fma", "struct fma { value i32 }\n");
      ( "reserved-float-generic-parameter",
        "floor",
        "fn value[floor](x i32) i32 { return x }\n" );
      ( "reserved-float-const-parameter",
        "ceil",
        "fn value[ceil const u32](x i32) i32 { return x }\n" );
      ("reserved-float-const", "round", "const round i32 = 1\n");
    ];
  let released_unreserved_names =
    llvm_of
      "struct Members { len i32 i32 i32 true i32 }\n\
       fn main() i32 { value Members = (Members){1, 2, 3}\n\
       return value.len + value.i32 + value.true }\n"
  in
  if not (contains released_unreserved_names "%struct.Members = type") then
    failwith "released-unreserved-name: member labels stopped compiling";
  parse_error_message "floating-literal-unavailable" "expected identifier, found `5`"
    "fn main() i32 { return 1.5 }\n";
  parse_error "labeled-break-rejected" "fn f() void { while true { break outer } }\n";

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
    "fn id[N const addr](x u64) u64 { return x }\n";

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
      (Parser.parse
         (source "fn check_case(value Mixed[T, addr, 3]) i64 { return 0 }\n"))
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
                     Ast.Type_arg Ast.Addr;
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
    "fn check_case() i64 { return identity[i64](identity[i64](7)) }\n\
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
     fn inner[T](value T) T { result Box[T] = (Box[T]){value}\n\
     return result.value }\n\
     fn outer[T](value T) T { return inner[T](value) }\n\
     fn check_case() u8 { return outer[u8](3) }\n"
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
  let character_literal_messages =
    [
      ("character-empty", "''", "empty character literal");
      ("character-two-bytes", "'ab'", "character literal must contain exactly one byte");
      ( "character-four-bytes",
        "'abcd'",
        "character literal must contain exactly one byte" );
      ( "character-non-ascii",
        "'" ^ "\195\169" ^ "'",
        "character literal must be printable ASCII; use \\xNN" );
      ( "character-raw-tab",
        "'" ^ "\t" ^ "'",
        "character literal must be printable ASCII; use \\xNN" );
      ("character-unknown-escape", "'\\q'", "unknown string escape");
      ( "character-short-hex",
        "'\\x4'",
        "hex escape must be followed by exactly two hexadecimal digits" );
      ("character-unterminated-line", "'a\n", "unterminated character literal");
      ("character-unterminated-crlf", "'a\r\n", "unterminated character literal");
      ("character-unterminated-file", "'a", "unterminated character literal");
    ]
  in
  List.iter
    (fun (name, literal, expected) ->
      let messages = parse_messages ("fn main() i32 { return 0 }\n" ^ literal) in
      match messages with
      | [ message ] when message = expected -> ()
      | _ -> failwith (name ^ ": wrong message " ^ String.concat "; " messages))
    character_literal_messages;
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
  List.iter
    (fun (name, text) ->
      match parse_messages text with
      | [ message ]
        when message = "hex escape must be followed by exactly two hexadecimal digits"
        ->
          ()
      | messages -> failwith (name ^ ": wrong message " ^ String.concat "; " messages))
    [
      ("hex-escape-missing-digits", "fn main() i64 { return 0 }\n\"\\x\"\n");
      ("hex-escape-one-digit", "fn main() i64 { return 0 }\n\"\\xA\"\n");
      ("hex-escape-invalid-first", "fn main() i64 { return 0 }\n\"\\xG1\"\n");
      ("hex-escape-invalid-second", "fn main() i64 { return 0 }\n\"\\x0G\"\n");
    ];
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
  (let ir =
     llvm_of
       "fn f(a vec[2, bool], b vec[2, bool]) vec[2, bool] { return a & b }\n\
        fn main() i32 { return 0 }\n"
   in
   if not (contains ir "and <2 x i1>") then
     failwith "bool-mask-bitop: vec bitand did not lower");
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
     fn main(value addr) addr { return bad[addr](value) }\n"
  in
  (match semantic_diagnostics type_generic_failure with
  | [ diagnostic ] ->
      if diagnostic.message <> "address arithmetic requires a scalar integer offset"
      then failwith "generic-instantiation-type: root message changed";
      if
        diagnostic.primary.Span.file <> "regression.fas"
        || diagnostic.primary.Span.line <> 1
        || diagnostic.primary.Span.column <> 37
      then failwith "generic-instantiation-type: root span changed";
      if
        diagnostic.notes <> [ "while instantiating `bad[addr]` at regression.fas:2:38" ]
      then
        failwith
          ("generic-instantiation-type: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-type: expected one diagnostic");
  let const_generic_failure =
    "fn bad[N const u64](value addr) addr { return value + value }\n\
     fn main(value addr) addr {\n\
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
     fn main(value addr) addr {\n\
     return bad[addr, 18446744073709551615](value)\n\
     }\n"
  in
  (match semantic_diagnostics mixed_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes
        <> [
             "while instantiating `bad[addr, 18446744073709551615]` at \
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
     fn main(value addr, other addr) addr {\n\
     return bad[addr, 2, addr, -3](value, other)\n\
     }\n"
  in
  (match semantic_diagnostics interleaved_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes
        <> [ "while instantiating `bad[addr, 2, addr, -3]` at regression.fas:5:11" ]
      then
        failwith
          ("generic-instantiation-order: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-order: expected one diagnostic");
  let nested_generic_failure =
    "fn inner[T](value T) T { return value + value }\n\
     fn outer[T](value T) T { return inner[T](value) }\n\
     fn main(value addr) addr { return outer[addr](value) }\n"
  in
  (match semantic_diagnostics nested_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes
        <> [
             "while instantiating `outer[addr]` at regression.fas:3:40";
             "while instantiating `inner[addr]` at regression.fas:2:38";
           ]
      then
        failwith
          ("generic-instantiation-nested: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-nested: expected one diagnostic");
  let nested_const_generic_failure =
    "fn inner[N const usize](value addr) addr { return value + value }\n\
     fn outer[T](value T) T { return inner[4](value) }\n\
     fn main(value addr) addr { return outer[addr](value) }\n"
  in
  (match semantic_diagnostics nested_const_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes
        <> [
             "while instantiating `outer[addr]` at regression.fas:3:40";
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
     fn main() i64 { value Box[Box[u8]] = (Box[Box[u8]]){(Box[u8]){1}}\n\
     bad[Box[Box[u8]]](value)\n\
     return 0\n\
     }\n"
  in
  semantic_error "generic-instantiation-nested-struct"
    "aggregate parameter `value` cannot be passed by value; pass `&x` as `addr` or \
     `handle[T]`"
    nested_struct_argument_failure;
  let specialized_type_message_failure =
    "struct Box[T] { value T }\n\
     fn bad[T](value T) i64 {\n\
     local i64 = value\n\
     return 0\n\
     }\n\
     fn main() i64 { value u8 = 1\n\
    \ return bad[u8](value) }\n"
  in
  (match semantic_diagnostics specialized_type_message_failure with
  | [ diagnostic ] ->
      let rendered = Diag.render_all ~source:None [ diagnostic ] in
      if diagnostic.message <> "type mismatch: expected i64, got u8" then
        failwith
          ("generic-instantiation-specialized-type-message: unexpected message: "
         ^ diagnostic.message);
      if contains rendered "$spec$" then
        failwith "generic-instantiation-specialized-type-message: internal name leaked"
  | _ ->
      failwith "generic-instantiation-specialized-type-message: expected one diagnostic");
  let repeated_generic_failure =
    "fn bad[T](value T) T { return value + value }\n\
     fn main(value addr) addr {\n\
     first addr = bad[addr](value)\n\
     return bad[addr](first)\n\
     }\n"
  in
  (match semantic_diagnostics repeated_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes <> [ "while instantiating `bad[addr]` at regression.fas:3:17" ]
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
     fn main(value addr) addr { return root[addr](value) }\n"
  in
  (match semantic_diagnostics diamond_cache_failure with
  | [ diagnostic ] ->
      let rendered = Diag.render_all ~source:None [ diagnostic ] in
      if
        diagnostic.notes
        <> [
             "while instantiating `root[addr]` at regression.fas:8:39";
             "while instantiating `left[addr]` at regression.fas:5:15";
             "while instantiating `leaf[addr]` at regression.fas:2:36";
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
  let duplicate_across_files name expected_message first_file second_file first_text
      second_text =
    match check_files [ (first_file, first_text); (second_file, second_text) ] with
    | Error [ diagnostic ] ->
        if diagnostic.Diag.message <> expected_message then
          failwith (name ^ ": unexpected duplicate message: " ^ diagnostic.message);
        if diagnostic.primary.Span.file <> second_file then
          failwith (name ^ ": primary location does not name the second definition");
        let first_location = "first definition is at " ^ first_file ^ ":1:" in
        if not (List.exists (fun note -> contains note first_location) diagnostic.notes)
        then failwith (name ^ ": first definition location missing from notes")
    | Error diagnostics ->
        failwith
          (name ^ ": expected one duplicate diagnostic, got "
          ^ Diag.render_all ~source:None diagnostics)
    | Ok _ -> failwith (name ^ ": duplicate declaration was accepted")
  in
  duplicate_across_files "extern-duplicate-matching-signature"
    "duplicate function `shared`" "extern_first.fas" "extern_second.fas"
    "extern \"C\" { fn shared(value i32) i32 }\n"
    "extern \"C\" { fn shared(value i32) i32 }\n";
  duplicate_across_files "extern-duplicate-mismatching-signature"
    "duplicate function `shared`" "extern_mismatch_first.fas"
    "extern_mismatch_second.fas" "extern \"C\" { fn shared(value i32) i32 }\n"
    "extern \"C\" { fn shared(value i64) i32 }\n";
  duplicate_across_files "native-extern-collision" "duplicate function `shared`"
    "native_first.fas" "extern_collision.fas"
    "fn shared(value i32) i32 { return value }\n"
    "extern \"C\" { fn shared(value i32) i32 }\n";
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
     fn wrap[T, N const usize](value T) T { seen usize = N\n\
     result Box[T] = (Box[T]){stamp[T, N](value)}\n\
     return result.value }\n\
     fn main() i64 { first i64 = wrap[i64, THREE](7)\n\
     return wrap[i64, THREE](first) }\n"
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
     fn main(value addr) addr { return bad[addr, 1](value) }\n";
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
                "fn grow[T]() i64 { return grow[arr[2, T]]() }\n\
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
                  "fn identity[T](pointer addr) addr { return pointer }\n\
                   fn main() usize { right arr[01, u8] = {1}\n\
                   first addr = identity[arr[01, u8]](&right)\n\
                   second addr = identity[arr[1, u8]](first)\n\
                   return sizeof[arr[1, u8]] }\n"))))
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
    "struct Box[T] { value T pointer addr }\n\
     struct Pair[A, B] { first A second B }\n\
     struct Wrapper[T] { boxed Box[T] }\n\
     fn main() usize {\n\
    \ box Box[i64] = (Box[i64]){7, addr_from_bits(0)}\n\
    \ pair64 Pair[i64, u8] = (Pair[i64, u8]){9, 1}\n\
    \ pair32 Pair[u32, u8] = (Pair[u32, u8]){9, 1}\n\
    \ wrapped Wrapper[u8] = (Wrapper[u8]){(Box[u8]){1, addr_from_bits(0)}}\n\
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
           | [ { ty = Hir.Int Hir.I64; _ }; { ty = Hir.Addr; _ } ] -> true
           | _ -> false)
         box_specializations)
  then failwith "generic-struct-substitution: field substitution failed";
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
       fn main() usize { value Bytes[3]\n\
      \ return sizeof[Bytes[3]] }\n"
  in
  if not (contains issue33_llvm "%\"struct.Bytes$spec$c7:usize:3\" = type { [3 x i8] }")
  then failwith "const-generic-struct-llvm-name: specialization name was not quoted";
  if not (contains issue33_llvm "alloca %\"struct.Bytes$spec$c7:usize:3\"") then
    failwith "const-generic-struct-llvm-use: local specialization type was not quoted";
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
     fn fixed[T](pointer addr) void { result T\n\
     return }\n\
     fn main() usize { three Buffer[u8, THREE]\n\
     boxed Buffer[u8, BOX_BYTES]\n\
     unit Buffer[u8, sizeof[Unit]]\n\
     fixed[Buffer[u8, THREE]](&three)\n\
     return sizeof[Holder] + sizeof[Buffer[u8, BOX_BYTES]] + sizeof[Buffer[u8, \
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
     fn identity[T](pointer addr) addr { return pointer }\n\
     fn forward[T](pointer addr) addr { return identity[T](pointer) }\n\
     fn main() usize { three Buffer[u8, THREE]\n\
     four Buffer[u8, FOUR]\n\
     a addr = identity[Buffer[u8, THREE]](&three)\n\
     b addr = identity[Buffer[u8, FOUR]](&four)\n\
     forward[Buffer[u8, THREE]](a)\n\
     forward[Buffer[u8, FOUR]](b)\n\
     return sizeof[Buffer[u8, THREE]] + sizeof[Buffer[u8, FOUR]]\n\
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
     fn pass[T, N const usize](pointer addr) void {\n\
     value Outer[Inner[N]]\n\
     return\n\
     }\n\
     fn main() void { value Outer[Inner[1]]\n\
     pass[u8, 1](&value)\n\
     return\n\
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
     fn array_identity[T, N const usize](pointer addr) addr { return pointer }\n\
     fn array_outer[T, N const usize](pointer addr) addr {\n\
     return array_identity[T, N](pointer)\n\
     }\n\
     fn byte_identity[N const usize](pointer addr) addr { return pointer }\n\
     fn vector_identity[T, N const usize](value vec[N, T]) vec[N, T] {\n\
     return value\n\
     }\n\
     fn aggregate_metrics[T, N const usize]() usize {\n\
     return sizeof[arr[N, T]] + alignof[arr[N, T]] + sizeof[vec[N, T]]\n\
     }\n\
     fn main() usize { value arr[3, u8] = {1, 2, 3}\n\
     pointer addr = array_outer[u8, THREE](&value)\n\
     forwarded addr = array_outer[u8, 3](pointer)\n\
     bytes addr = byte_identity[THREE](&value)\n\
     lanes vec[4, u16] = splat(2)\n\
     same_lanes vec[4, u16] = vector_identity[u16, 4](lanes)\n\
     return len(value) + sizeof[vec[4, u16]] + aggregate_metrics[u16, 4]()\n\
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
           | [ { ty = Hir.Addr; _ } ], Hir.Addr -> true
           | _ -> false)
         function_specializations)
  then failwith "const-generic-function-type-signature: length was not substituted";
  if
    not
      (List.exists
         (fun (func : Hir.func) ->
           contains func.name "aggregate_metrics$spec$"
           && func.params = [] && func.ret = Hir.Int Hir.Usize)
         function_specializations)
  then
    failwith "const-generic-function-type-nesting: aggregate length was not substituted";
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
    "aggregate arguments cannot be passed by value; pass `&x` as `addr` or `handle[T]`"
    "fn identity[N const usize](value arr[N, u8]) arr[N, u8] { return value }\n\
     fn main() i64 { value arr[3,u8] = {1, 2, 3}\n\
    \ identity[4](value)\n\
    \ return 0 }\n";
  semantic_error "const-generic-function-negative-length" "negative aggregate length"
    "fn identity[N const isize](value arr[N, u8]) arr[N, u8] { return value }\n\
     fn main() i64 { value arr[1,u8] = {1}\n\
    \ identity[-1](value)\n\
    \ return 0 }\n";
  semantic_error "const-generic-function-machine-length"
    "aggregate length is not a machine integer"
    "fn identity[N const u64](value arr[N, u8]) arr[N, u8] { return value }\n\
     fn main() i64 { value arr[1,u8] = {1}\n\
    \ identity[18446744073709551615](value)\n\
    \ return 0 }\n";
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
     fn pass[T, N const usize](pointer addr) void {\n\
     value Buffer[T, N]\n\
     return\n\
     }\n\
     fn wrap[T, N const usize](pointer addr) void {\n\
     value Buffer[T, N]\n\
     pass[T, N](&value)\n\
     wrapped Wrapped[T, N]\n\
     return\n\
     }\n\
     fn sized(pointer addr) void {\n\
     pass[u8, sizeof[Unit]](pointer)\n\
     return\n\
     }\n\
     fn main() void { value Buffer[u8, 3]\n\
     wrap[u8, THREE](&value)\n\
     sized(&value)\n\
     return\n\
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
           match func.params with [ { ty = Hir.Addr; _ } ] -> true | _ -> false)
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
                   "struct Node[T] { next addr value T }\n\
                    fn main() i64 { value Node[u8]\n\
                   \ return 0 }\n")))));
  (match
     Sema.check ~limits:recursive_struct_limits
       (expect_ok
          (Parser.parse
             (source
                "struct Inner[T] { value T }\n\
                 struct Outer[T] { inner Inner[T] }\n\
                 fn main() i64 { value Outer[u8]\n\
                \ return 0 }\n")))
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
                    fn main() i64 { left Box[u8]\n\
                   \ right Box[u8]\n\
                   \ return 0 }\n")))));
  (match
     Sema.check ~limits:one_struct_limit
       (expect_ok
          (Parser.parse
             (source
                "struct Box[T] { value T }\n\
                 fn id[N const usize]() usize { return N }\n\
                 fn main() usize { value Box[u8]\n\
                \ return id[1]() }\n")))
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
         globals = [];
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
         globals = [];
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
         globals = [];
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
         globals = [];
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
            (Ast.Bit_not, Hir.Null (Hir.Addr, Span.synthetic), Hir.Addr, Span.synthetic),
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
         globals = [];
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
    "fn f(value addr) void { while value { break } }\n";
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
        fn pointer(value addr) bool {\n\
       \ while value != addr_from_bits(0) { break }\n\
       \ return value != addr_from_bits(0)\n\
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
        fn main() i64 { pair Pair[i64] = (Pair[i64]){12, 4}\n\
        return pair.left }\n");

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
  semantic_error "context-null-unconstrained" "null requires an addr or handle context"
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
       "fn f(p addr) bool { return p == addr_from_bits(0) }\n\
        fn g(p addr) bool { return addr_from_bits(0) == p }\n");
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
      ("umulh-u8-high", "u8", "255", "255", "mul_hi", "ret i8 254\n");
      ("umulh-u8-mid", "u8", "2", "3", "mul_hi", "ret i8 0\n");
      ("smulh-i8-high", "i8", "-128", "-128", "mul_hi", "ret i8 64\n");
      ("smulh-i8-negative", "i8", "-128", "127", "mul_hi", "ret i8 192\n");
      ("smulh-i8-one", "i8", "-1", "-1", "mul_hi", "ret i8 0\n");
      ( "umulh-u64-max",
        "u64",
        "18446744073709551615",
        "18446744073709551615",
        "mul_hi",
        "ret i64 -2\n" );
      ( "smulh-i64-min",
        "i64",
        "-9223372036854775808",
        "-9223372036854775808",
        "mul_hi",
        "ret i64 4611686018427387904\n" );
      ("umulh-usize", "usize", "4294967296", "4294967296", "mul_hi", "ret i64 1\n");
      ("smulh-isize", "isize", "-4294967296", "3", "mul_hi", "ret i64 -1\n");
      ( "umulh-u32-max",
        "u32",
        "4294967295",
        "4294967295",
        "mul_hi",
        "ret i32 4294967294\n" );
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
  let mul_vec =
    llvm_of
      "fn w[N const u32]() u32 { return N }\n\
       const A u32 = 197375\n\
       const B u32 = 157549567\n\
       const AV vec[4,u8] = bitcast[vec[4,u8]](A)\n\
       const BV vec[4,u8] = bitcast[vec[4,u8]](B)\n\
       const X vec[4,u8] = mul_hi(AV, BV)\n\
       fn main() u32 { return w[bitcast[u32](X)]() }\n"
  in
  if not (contains mul_vec "ret i32 65790\n") then failwith "mul_hi: vec lanes drifted";
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
  let mul_runtime =
    llvm_of
      "fn f(a vec[4,u8], b vec[4,u8]) vec[4,u8] { return mul_hi(a, b) }\n\
       fn g(a i32, b i32) i32 { return mul_hi(a, b) }\n\
       fn h(a u32, b u32) u32 { return mul_hi(a, b) }\n\
       fn i(a vec[3,i32], b vec[3,i32]) vec[3,i32] { return mul_hi(a, b) }\n\
       fn m(a u64, b u64) u64 { return mul_hi(a, b) }\n\
       fn n(a i64, b i64) i64 { return mul_hi(a, b) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains mul_runtime needle) then failwith ("mul_hi: missing " ^ needle))
    [
      "zext <4 x i8> %";
      "mul <4 x i16> %";
      "lshr <4 x i16> %";
      "<i16 8, i16 8, i16 8, i16 8>";
      "sext i32 %";
      "mul i64 %";
      "ashr i64 %";
      "trunc i64 %";
      "zext i32 %";
      "lshr i64 %";
      "sext <3 x i32> %";
      "mul <3 x i64> %";
      "ashr <3 x i64> %";
      "trunc <3 x i64> %";
      "zext i64 %";
      "sext i64 %";
      "mul i128 %";
      "lshr i128 %";
      "ashr i128 %";
      "trunc i128 %";
    ];
  if contains mul_runtime "poison" || contains mul_runtime "undef" then
    failwith "mul_hi: undefined value in computed results";
  List.iter
    (fun (name, op, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf
             "fn w[N const u32]() u32 { return N }\n\
              const A u32 = 197375\n\
              const AV vec[4,u8] = bitcast[vec[4,u8]](A)\n\
              const X vec[4,u8] = %s(AV)\n\
              fn main() u32 { return w[bitcast[u32](X)]() }\n"
             op)
      in
      if not (contains ir needle) then failwith ("bit-count: " ^ name ^ " drifted"))
    [
      ("vec-popcount", "popcount", "ret i32 131336\n");
      ("vec-clz", "clz", "ret i32 134612480\n");
      ("vec-ctz", "ctz", "ret i32 134217984\n");
    ];
  let bitcount_runtime =
    llvm_of
      "fn f(a vec[4,u8]) vec[4,u8] { return popcount(a) }\n\
       fn g(a vec[2,i32]) vec[2,i32] { return clz(a) }\n\
       fn h(a vec[3,i64]) vec[3,i64] { return ctz(a) }\n\
       fn i(a u32) u32 { return popcount(a) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains bitcount_runtime needle) then
        failwith ("bit-count: missing " ^ needle))
    [
      "@llvm.ctpop.v4i8("; "@llvm.ctlz.v2i32("; "@llvm.cttz.v3i64("; "@llvm.ctpop.i32(";
    ];
  if contains bitcount_runtime "poison" || contains bitcount_runtime "undef" then
    failwith "bit-count: undefined value in computed results";
  List.iter
    (fun (name, expr, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf
             "const K1 i64 = 8589934593\n\
              const K2 i64 = 8589934594\n\
              const A vec[2,i32] = bitcast[vec[2,i32]](K1)\n\
              const B vec[2,i32] = bitcast[vec[2,i32]](K2)\n\
              const M vec[2,bool] = A == B\n\
              const N vec[2,bool] = !M\n\
              const R vec[2,bool] = %s\n\
              fn f() vec[2,bool] { return R }\n"
             expr)
      in
      if not (contains ir needle) then failwith ("mask: " ^ name ^ " drifted"))
    [
      ("const-not", "!M", "ret <2 x i1> <i1 1, i1 0>\n");
      ("const-and", "M & N", "ret <2 x i1> <i1 0, i1 0>\n");
      ("const-or", "M | (A == A)", "ret <2 x i1> <i1 1, i1 1>\n");
      ("const-xor", "M ^ (A == A)", "ret <2 x i1> <i1 1, i1 0>\n");
    ];
  List.iter
    (fun (name, expr, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf
             "const K1 i64 = 8589934593\n\
              const K2 i64 = 8589934594\n\
              const A vec[2,i32] = bitcast[vec[2,i32]](K1)\n\
              const B vec[2,i32] = bitcast[vec[2,i32]](K2)\n\
              const M vec[2,bool] = A == B\n\
              const R %s = %s\n\
              fn f() %s { return R }\n"
             (if name = "const-any" || name = "const-all" then "bool" else "vec[2,i32]")
             expr
             (if name = "const-any" || name = "const-all" then "bool" else "vec[2,i32]"))
      in
      if not (contains ir needle) then failwith ("mask: " ^ name ^ " drifted"))
    [
      ("const-any", "any(M)", "ret i1 true\n");
      ("const-all", "all(M)", "ret i1 false\n");
      ("const-select", "select(M, A, B)", "ret <2 x i32> <i32 2, i32 2>\n");
    ];
  let mask_runtime =
    llvm_of
      "fn f(m vec[2,bool], n vec[2,bool]) vec[2,bool] {\n\
       \032 a vec[2,bool] = m & n\n\
       \032 b vec[2,bool] = m | n\n\
       \032 c vec[2,bool] = m ^ n\n\
       \032 d vec[2,bool] = !a\n\
       \032 return d\n\
       }\n\
       fn g(m vec[2,bool]) bool { return any(m) }\n\
       fn h(m vec[2,bool]) bool { return all(m) }\n\
       fn i(m vec[2,bool], a vec[2,i32], b vec[2,i32]) vec[2,i32] { return select(m, \
       a, b) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains mask_runtime needle) then failwith ("mask: missing " ^ needle))
    [ "and <2 x i1>"; "or <2 x i1>"; "xor <2 x i1>"; "select <2 x i1>" ];
  if contains mask_runtime "poison" || contains mask_runtime "undef" then
    failwith "mask: undefined value in computed results";
  let mask_reductions =
    llvm_of
      "fn g(m vec[2,bool]) bool { return any(m) }\n\
       fn h(m vec[2,bool]) bool { return all(m) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains mask_reductions needle) then failwith ("mask: missing " ^ needle))
    [ "extractelement <2 x i1>"; "or i1"; "and i1" ];
  let mask_eager =
    llvm_of
      "fn e(m vec[2,bool], a vec[2,i32], b vec[2,i32], c vec[2,i32]) vec[2,i32] {\n\
       \032 return select(m, a / b, c)\n\
       }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains mask_eager needle) then failwith ("mask: missing " ^ needle))
    [ "sdiv <2 x i32>"; "select <2 x i1>" ];
  List.iter
    (fun (name, expr, rty, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf
             "const KA u32 = 67305985\n\
              const KB u32 = 134678021\n\
              const A vec[4,u8] = bitcast[vec[4,u8]](KA)\n\
              const B vec[4,u8] = bitcast[vec[4,u8]](KB)\n\
              const K u32 = 117572096\n\
              const I vec[4,u8] = bitcast[vec[4,u8]](K)\n\
              const K2 u16 = 3\n\
              const I2 vec[2,u8] = bitcast[vec[2,u8]](K2)\n\
              const KS u16 = 773\n\
              const IS vec[2,i8] = bitcast[vec[2,i8]](KS)\n\
              const R %s = %s\n\
              fn f() %s { return R }\n"
             rty expr rty)
      in
      if not (contains ir needle) then failwith ("shuffle: " ^ name ^ " drifted"))
    [
      ( "const-same",
        "shuffle(A, B, I)",
        "vec[4,u8]",
        "ret <4 x i8> <i8 1, i8 3, i8 3, i8 8>\n" );
      ("const-fewer", "shuffle(A, B, I2)", "vec[2,u8]", "ret <2 x i8> <i8 4, i8 1>\n");
      ( "const-signed-positive",
        "shuffle(A, B, IS)",
        "vec[2,u8]",
        "ret <2 x i8> <i8 6, i8 4>\n" );
    ];
  let shuffle_runtime =
    llvm_of
      "const K u32 = 117572096\n\
       const I vec[4,u8] = bitcast[vec[4,u8]](K)\n\
       const K2 u16 = 3\n\
       const I2 vec[2,u8] = bitcast[vec[2,u8]](K2)\n\
       fn f(a vec[4,u8], b vec[4,u8]) vec[4,u8] { return shuffle(a, b, I) }\n\
       fn g(a vec[4,u8], b vec[4,u8]) vec[2,u8] { return shuffle(a, b, I2) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains shuffle_runtime needle) then
        failwith ("shuffle: missing " ^ needle))
    [
      "shufflevector <4 x i8>";
      "<4 x i32> <i32 0, i32 2, i32 2, i32 7>";
      "<2 x i32> <i32 3, i32 0>";
    ];
  if contains shuffle_runtime "poison" || contains shuffle_runtime "undef" then
    failwith "shuffle: undefined value in computed results";
  List.iter
    (fun (name, setup, expr, rty, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf "%sconst R %s = %s\nfn f() %s { return R }\n" setup rty expr
             rty)
      in
      if not (contains ir needle) then failwith ("permute: " ^ name ^ " drifted"))
    [
      ( "const-zero-on-invalid",
        "const KA u32 = 67305985\n\
         const A vec[4,u8] = bitcast[vec[4,u8]](KA)\n\
         const KI u32 = 117572096\n\
         const I vec[4,u8] = bitcast[vec[4,u8]](KI)\n",
        "permute(A, I)",
        "vec[4,u8]",
        "ret <4 x i8> <i8 1, i8 3, i8 3, i8 0>\n" );
      ( "const-no-truncation",
        "const KA u16 = 513\n\
         const A vec[2,u8] = bitcast[vec[2,u8]](KA)\n\
         const KI u32 = 65793\n\
         const I vec[2,u16] = bitcast[vec[2,u16]](KI)\n",
        "permute(A, I)",
        "vec[2,u8]",
        "ret <2 x i8> <i8 0, i8 2>\n" );
      ( "const-more-lanes",
        "const KA u16 = 513\n\
         const A vec[2,u8] = bitcast[vec[2,u8]](KA)\n\
         const KI u32 = 83952896\n\
         const I vec[4,u8] = bitcast[vec[4,u8]](KI)\n",
        "permute(A, I)",
        "vec[4,u8]",
        "ret <4 x i8> <i8 1, i8 0, i8 2, i8 0>\n" );
    ];
  let permute_runtime =
    llvm_of
      "fn f(a vec[4,u8], i vec[4,u8]) vec[4,u8] { return permute(a, i) }\n\
       fn g(a vec[4,u8], i vec[4,u64]) vec[4,u8] { return permute(a, i) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains permute_runtime needle) then
        failwith ("permute: missing " ^ needle))
    [
      "icmp ult i8"; "icmp ult i64"; "extractelement <4 x i8>"; "insertelement <4 x i8>";
    ];
  if contains permute_runtime "poison" || contains permute_runtime "undef" then
    failwith "permute: undefined value in computed results";
  List.iter
    (fun (name, setup, expr, rty, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf "%sconst R %s = %s\nfn f() %s { return R }\n" setup rty expr
             rty)
      in
      if not (contains ir needle) then failwith ("reduce: " ^ name ^ " drifted"))
    [
      ( "sum-u8",
        "const K u32 = 2348321930\nconst A vec[4,u8] = bitcast[vec[4,u8]](K)\n",
        "reduce_sum(A)",
        "u8",
        "ret i8 153\n" );
      ( "and-u8",
        "const K u32 = 2348321930\nconst A vec[4,u8] = bitcast[vec[4,u8]](K)\n",
        "reduce_and(A)",
        "u8",
        "ret i8 136\n" );
      ( "or-u8",
        "const K u32 = 2348321930\nconst A vec[4,u8] = bitcast[vec[4,u8]](K)\n",
        "reduce_or(A)",
        "u8",
        "ret i8 255\n" );
      ( "xor-u8",
        "const K u32 = 2348321930\nconst A vec[4,u8] = bitcast[vec[4,u8]](K)\n",
        "reduce_xor(A)",
        "u8",
        "ret i8 117\n" );
      ( "min-u8",
        "const K u32 = 2348321930\nconst A vec[4,u8] = bitcast[vec[4,u8]](K)\n",
        "reduce_min(A)",
        "u8",
        "ret i8 138\n" );
      ( "max-u8",
        "const K u32 = 2348321930\nconst A vec[4,u8] = bitcast[vec[4,u8]](K)\n",
        "reduce_max(A)",
        "u8",
        "ret i8 248\n" );
      ( "min-i8-signed",
        "const K u32 = 133250186\nconst A vec[4,i8] = bitcast[vec[4,i8]](K)\n",
        "reduce_min(A)",
        "i8",
        "ret i8 138\n" );
      ( "max-i8-signed",
        "const K u32 = 133250186\nconst A vec[4,i8] = bitcast[vec[4,i8]](K)\n",
        "reduce_max(A)",
        "i8",
        "ret i8 60\n" );
      ( "sum-i8-wrap",
        "const K u32 = 133250186\nconst A vec[4,i8] = bitcast[vec[4,i8]](K)\n",
        "reduce_sum(A)",
        "i8",
        "ret i8 190\n" );
      ( "one-lane",
        "const K i32 = -5\nconst A vec[1,i32] = bitcast[vec[1,i32]](K)\n",
        "reduce_sum(A)",
        "i32",
        "ret i32 4294967291\n" );
      ( "min-u32-high",
        "const K u64 = 9223372034707292160\n\
         const A vec[2,u32] = bitcast[vec[2,u32]](K)\n",
        "reduce_min(A)",
        "u32",
        "ret i32 2147483647\n" );
      ( "max-u32-high",
        "const K u64 = 9223372034707292160\n\
         const A vec[2,u32] = bitcast[vec[2,u32]](K)\n",
        "reduce_max(A)",
        "u32",
        "ret i32 2147483648\n" );
    ];
  let reduce_runtime =
    llvm_of
      "fn f(a vec[4,u8]) u8 { return reduce_sum(a) }\n\
       fn g(a vec[2,i32]) i32 { return reduce_min(a) }\n\
       fn h(a vec[2,i32]) i32 { return reduce_max(a) }\n\
       fn i(a vec[2,u64]) u64 { return reduce_and(a) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains reduce_runtime needle) then failwith ("reduce: missing " ^ needle))
    [
      "extractelement <4 x i8>";
      "add i8";
      "icmp slt i32";
      "icmp sgt i32";
      "and i64";
      "select i1";
    ];
  if contains reduce_runtime "poison" || contains reduce_runtime "undef" then
    failwith "reduce: undefined value in computed results";
  List.iter
    (fun (name, setup, expr, rty, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf "%sconst R %s = %s\nfn f() %s { return R }\n" setup rty expr
             rty)
      in
      if not (contains ir needle) then failwith ("compaction: " ^ name ^ " drifted"))
    [
      ( "compress-int",
        "const KV u32 = 67305985\n\
         const V vec[4,u8] = bitcast[vec[4,u8]](KV)\n\
         const KW u32 = 134416641\n\
         const W vec[4,u8] = bitcast[vec[4,u8]](KW)\n\
         const M vec[4,bool] = V == W\n",
        "compress(V, M)",
        "vec[4,u8]",
        "ret <4 x i8> <i8 1, i8 3, i8 0, i8 0>\n" );
      ( "expand-int",
        "const KV u32 = 67305985\n\
         const V vec[4,u8] = bitcast[vec[4,u8]](KV)\n\
         const KW u32 = 134416641\n\
         const W vec[4,u8] = bitcast[vec[4,u8]](KW)\n\
         const M vec[4,bool] = V == W\n",
        "expand(V, M)",
        "vec[4,u8]",
        "ret <4 x i8> <i8 1, i8 0, i8 2, i8 0>\n" );
      ( "compress-all-false",
        "const KV u32 = 67305985\n\
         const V vec[4,u8] = bitcast[vec[4,u8]](KV)\n\
         const KA u32 = 134678021\n\
         const A vec[4,u8] = bitcast[vec[4,u8]](KA)\n\
         const M vec[4,bool] = V == A\n",
        "compress(V, M)",
        "vec[4,u8]",
        "ret <4 x i8> <i8 0, i8 0, i8 0, i8 0>\n" );
      ( "compress-all-true",
        "const KV u32 = 67305985\n\
         const V vec[4,u8] = bitcast[vec[4,u8]](KV)\n\
         const M vec[4,bool] = V == V\n",
        "compress(V, M)",
        "vec[4,u8]",
        "ret <4 x i8> <i8 1, i8 2, i8 3, i8 4>\n" );
      ( "compress-bool-lanes",
        "const KV u32 = 67305985\n\
         const V vec[4,u8] = bitcast[vec[4,u8]](KV)\n\
         const KW4 u32 = 100991489\n\
         const W4 vec[4,u8] = bitcast[vec[4,u8]](KW4)\n\
         const KW5 u32 = 134416641\n\
         const W5 vec[4,u8] = bitcast[vec[4,u8]](KW5)\n\
         const M5 vec[4,bool] = V == W4\n\
         const M4 vec[4,bool] = V == W5\n",
        "compress(M5, M4)",
        "vec[4,bool]",
        "ret <4 x i1> <i1 1, i1 0, i1 0, i1 0>\n" );
      ( "expand-bool-lanes",
        "const KV u32 = 67305985\n\
         const V vec[4,u8] = bitcast[vec[4,u8]](KV)\n\
         const KW4 u32 = 100991489\n\
         const W4 vec[4,u8] = bitcast[vec[4,u8]](KW4)\n\
         const KW5 u32 = 134416641\n\
         const W5 vec[4,u8] = bitcast[vec[4,u8]](KW5)\n\
         const M5 vec[4,bool] = V == W4\n\
         const M4 vec[4,bool] = V == W5\n",
        "expand(M5, M4)",
        "vec[4,bool]",
        "ret <4 x i1> <i1 1, i1 0, i1 1, i1 0>\n" );
    ];
  let compaction_runtime =
    llvm_of
      "fn f(a vec[4,u8], m vec[4,bool]) vec[4,u8] { return compress(a, m) }\n\
       fn g(a vec[4,bool], m vec[4,bool]) vec[4,bool] { return expand(a, m) }\n\
       fn h(a vec[3,i32], m vec[3,bool]) vec[3,i32] { return compress(a, m) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains compaction_runtime needle) then
        failwith ("compaction: missing " ^ needle))
    [ "insertelement <4 x i8>"; "select i1"; "extractelement <4 x i8>" ];
  if contains compaction_runtime "poison" || contains compaction_runtime "undef" then
    failwith "compaction: undefined value in computed results";
  let lane_trap_runtime =
    llvm_of
      "fn f(a vec[4,u8], i usize) u8 { return a[i] }\n\
       fn g(a vec[4,u8], i usize) void { a[i] = 9 }\n\
       fn h(a vec[4,i32], i i64) void { a[i] += 1 }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains lane_trap_runtime needle) then
        failwith ("lane trap: missing " ^ needle))
    [
      "icmp uge i64";
      "icmp slt i64";
      "icmp sge i64";
      "llvm.trap";
      "extractelement <4 x i8>";
      "insertelement <4 x i8>";
    ];
  if contains lane_trap_runtime "poison" || contains lane_trap_runtime "undef" then
    failwith "lane trap: undefined value in computed results";
  List.iter
    (fun (name, text, expected) ->
      match semantic_messages text with
      | [ message ] when message = expected -> ()
      | _ -> failwith ("builtin reject: " ^ name))
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
      ( "mul-arity",
        "fn f(a u8) u8 { return mul_hi(a) }\n",
        "builtin `mul_hi` expects two arguments" );
      ( "mul-kind",
        "fn f(m vec[2,bool]) vec[2,bool] { return mul_hi(m, m) }\n",
        "builtin arguments must be integers or integer vectors" );
      ( "mul-mismatch",
        "fn f(a u8, b u16) u16 { return mul_hi(a, b) }\n",
        "builtin arguments must have the same type" );
      ( "ptr-add-sat",
        "fn f(p addr) u8 { return add_sat(p, p) }\n",
        "builtin arguments must be integers or integer vectors" );
      ( "ptr-mul-hi",
        "fn f(p addr) u8 { return mul_hi(p, p) }\n",
        "builtin arguments must be integers or integer vectors" );
      ( "bool-vec-mul-hi",
        "fn f(a vec[2,bool]) vec[2,bool] { return mul_hi(a, a) }\n",
        "builtin arguments must be integers or integer vectors" );
      ( "bitcount-bool-vec",
        "fn f(m vec[2,bool]) vec[2,bool] { return popcount(m) }\n",
        "builtin argument must be an integer or an integer vector" );
      ( "bitcount-ptr",
        "fn f(p addr) u8 { return clz(p) }\n",
        "builtin argument must be an integer or an integer vector" );
      ( "bitcount-bool-scalar",
        "fn f() bool { return popcount(true) }\n",
        "builtin argument must be an integer or an integer vector" );
      ( "bitcount-const-bool",
        "const X bool = clz(true)\nfn main() i32 { return 0 }\n",
        "builtin argument must be an integer or an integer vector" );
      ( "bitcount-const-popcount",
        "const X bool = popcount(false)\nfn main() i32 { return 0 }\n",
        "builtin argument must be an integer or an integer vector" );
      ( "bitcount-const-ctz-zero",
        "const X bool = ctz(false)\nfn main() i32 { return 0 }\n",
        "builtin argument must be an integer or an integer vector" );
      ( "bitcount-const-ctz",
        "const X bool = ctz(true)\nfn main() i32 { return 0 }\n",
        "builtin argument must be an integer or an integer vector" );
      ( "mask-compound-bool",
        "fn f() bool {\n  a bool = true\n  a &= false\n  return a\n}\n",
        "compound assignment requires an integer or vector" );
      ( "mask-compound-vec",
        "fn f(m vec[2,bool], n vec[2,bool]) vec[2,bool] {\n\
         \032 a vec[2,bool] = m\n\
         \032 a &= n\n\
         \032 return a\n\
         }\n",
        "compound assignment requires an integer or vector" );
      ( "mask-truthiness",
        "fn f(m vec[2,bool]) i32 {\n  if m { return 1 }\n  return 0\n}\n",
        "if condition must be bool" );
      ( "select-scalar-mask",
        "fn f(a vec[2,i32], b vec[2,i32]) vec[2,i32] { return select(true, a, b) }\n",
        "select mask must be a bool vector" );
      ( "select-mismatch",
        "fn f(m vec[2,bool], a vec[2,i32], b vec[2,i64]) vec[2,i32] { return select(m, \
         a, b) }\n",
        "builtin arguments must have the same type" );
      ( "select-lanes",
        "fn f(m vec[2,bool], a vec[4,i32], b vec[4,i32]) vec[4,i32] { return select(m, \
         a, b) }\n",
        "select values must be vectors with the mask lane count" );
      ( "select-scalar-values",
        "fn f(m vec[2,bool], a i32, b i32) i32 { return select(m, a, b) }\n",
        "select values must be vectors with the mask lane count" );
      ( "any-int-vec",
        "fn f(a vec[2,i32]) bool { return any(a) }\n",
        "builtin argument must be a bool vector" );
      ( "any-scalar",
        "fn f(a i32) bool { return any(a) }\n",
        "builtin argument must be a bool vector" );
      ( "select-arity",
        "fn f(m vec[2,bool], a vec[2,i32]) vec[2,i32] { return select(m, a) }\n",
        "builtin `select` expects three arguments" );
      ( "any-arity",
        "fn f(m vec[2,bool]) bool { return any(m, m) }\n",
        "builtin `any` expects one argument" );
      ( "shuffle-arity",
        "fn f(a vec[4,u8], b vec[4,u8]) vec[4,u8] { return shuffle(a, b) }\n",
        "builtin `shuffle` expects three arguments" );
      ( "shuffle-nonconst-indices",
        "fn f(a vec[4,u8], b vec[4,u8], i vec[4,u8]) vec[4,u8] { return shuffle(a, b, \
         i) }\n",
        "shuffle indices must be a compile-time constant integer vector" );
      ( "shuffle-bool-indices",
        "const I vec[4,bool] = splat(true)\n\
         fn f(a vec[4,u8], b vec[4,u8]) vec[4,u8] { return shuffle(a, b, I) }\n",
        "shuffle indices must be a compile-time constant integer vector" );
      ( "shuffle-index-range",
        "const K u32 = 255\n\
         const I vec[4,u8] = bitcast[vec[4,u8]](K)\n\
         fn f(a vec[4,u8], b vec[4,u8]) vec[4,u8] { return shuffle(a, b, I) }\n",
        "shuffle index out of range" );
      ( "shuffle-neg-selector",
        "const K u8 = 199\n\
         const I vec[1,i8] = bitcast[vec[1,i8]](K)\n\
         fn f(a vec[100,u8], b vec[100,u8]) vec[100,u8] { return shuffle(a, b, I) }\n",
        "shuffle index out of range" );
      ( "shuffle-signbit-selector",
        "const K u64 = 9223372036854775808\n\
         const I vec[1,u64] = bitcast[vec[1,u64]](K)\n\
         fn f(a vec[1,u8], b vec[1,u8]) vec[1,u8] { return shuffle(a, b, I) }\n",
        "shuffle index out of range" );
      ( "shuffle-non-vector-operands",
        "fn f(a u8, b u8, i vec[4,u8]) u8 { return shuffle(a, b, i) }\n",
        "shuffle operands must be vectors" );
      ( "shuffle-operand-mismatch",
        "const K u32 = 117572096\n\
         const I vec[4,u8] = bitcast[vec[4,u8]](K)\n\
         fn f(a vec[4,u8], b vec[2,u8]) vec[4,u8] { return shuffle(a, b, I) }\n",
        "builtin arguments must have the same type" );
      ( "permute-arity",
        "fn f(a vec[4,u8]) vec[4,u8] { return permute(a) }\n",
        "builtin `permute` expects two arguments" );
      ( "permute-signed-indices",
        "fn f(a vec[4,u8], i vec[4,i32]) vec[4,u8] { return permute(a, i) }\n",
        "permute indices must be an unsigned integer vector" );
      ( "permute-non-vector-value",
        "fn f(a u8, i vec[4,u8]) u8 { return permute(a, i) }\n",
        "permute value must be a vector" );
      ( "reduce-arity-two",
        "fn f(a vec[2,u8], b vec[2,u8]) u8 { return reduce_sum(a, b) }\n",
        "builtin `reduce_sum` expects one argument" );
      ( "reduce-arity-zero",
        "fn f() u8 { return reduce_sum() }\n",
        "builtin `reduce_sum` expects one argument" );
      ( "reduce-scalar-arg",
        "fn f(a u8) u8 { return reduce_sum(a) }\n",
        "reduction argument must be an integer vector" );
      ( "reduce-bool-vec-arg",
        "fn f(a vec[2,bool]) bool { return reduce_min(a) }\n",
        "reduction argument must be an integer vector" );
      ( "reduce-fold-scalar",
        "const K u8 = 5\nconst R u8 = reduce_max(K)\nfn f() u8 { return R }\n",
        "constant expression requires a known vector constant" );
      ( "reduce-fold-bool",
        "const K u32 = 67305985\n\
         const A vec[4,u8] = bitcast[vec[4,u8]](K)\n\
         const M vec[4,bool] = A == A\n\
         const R u8 = reduce_sum(M)\n\
         fn f() u8 { return R }\n",
        "reduction argument must be an integer vector" );
      ( "compress-arity",
        "fn f(a vec[4,u8], m vec[4,bool]) vec[4,u8] { return compress(a) }\n",
        "builtin `compress` expects two arguments" );
      ( "expand-arity",
        "fn f(a vec[4,u8]) vec[4,u8] { return expand(a) }\n",
        "builtin `expand` expects two arguments" );
      ( "compress-non-vector-values",
        "fn f(a u8, m vec[4,bool]) u8 { return compress(a, m) }\n",
        "compress values must be a vector" );
      ( "compress-mask-not-bool-vec",
        "fn f(a vec[4,u8], m vec[4,u8]) vec[4,u8] { return compress(a, m) }\n",
        "compress mask must be a bool vector" );
      ( "compress-lane-count",
        "fn f(a vec[4,u8], m vec[3,bool]) vec[4,u8] { return compress(a, m) }\n",
        "compress values and mask must have the same lane count" );
      ( "expand-lane-count",
        "fn f(a vec[4,u8], m vec[5,bool]) vec[4,u8] { return expand(a, m) }\n",
        "expand values and mask must have the same lane count" );
      ( "vec-lane-read-oob",
        "fn f(a vec[4,u8]) u8 { return a[7] }\n",
        "array index is out of bounds" );
      ( "vec-lane-write-oob",
        "fn f(a vec[4,u8]) void { a[7] = 1 }\n",
        "array index is out of bounds" );
      ( "vec-lane-compound-oob",
        "fn f(a vec[4,u8]) void { a[7] += 1 }\n",
        "array index is out of bounds" );
      ( "vec-lane-negative",
        "fn f(a vec[4,u8]) u8 { return a[-1] }\n",
        "array index is out of bounds" );
      ( "vec-lane-const-oob",
        "const K usize = 9\nfn f(a vec[4,u8]) u8 { return a[K] }\n",
        "array index is out of bounds" );
      ( "zext-equal-width",
        "fn f(a u32) u32 { return zext[u32](a) }\n",
        "illegal cast for source and destination widths" );
      ( "trunc-widening",
        "fn f(a u8) u32 { return trunc[u32](a) }\n",
        "illegal cast for source and destination widths" );
      ( "zext-to-bool",
        "fn f(a u8) bool { return zext[bool](a) }\n",
        "illegal cast for source and destination widths" );
    ];
  List.iter
    (fun (name, expr, rty, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf "const T %s = %s\nfn f() %s { return T }\n" rty expr rty)
      in
      if not (contains ir needle) then failwith ("div-edges: " ^ name ^ " drifted"))
    [
      ("trunc-negative-dividend", "-7 / 3", "i32", "ret i32 4294967294\n");
      ("trunc-negative-divisor", "7 / -3", "i32", "ret i32 4294967294\n");
      ("rem-sign-negative", "-7 % 3", "i32", "ret i32 4294967295\n");
      ("rem-sign-positive", "7 % -3", "i32", "ret i32 1\n");
      ("rem-min-minus-one", "-9223372036854775808 % -1", "i64", "ret i64 0\n");
      ("trunc-negative-i64", "-7 / 3", "i64", "ret i64 -2\n");
      ("cond-right-assoc", "false ? 2 : true ? 3 : 4", "i32", "ret i32 3\n");
      ("arith-left-assoc", "8 - 3 - 2", "i32", "ret i32 3\n");
      ("bitwise-above-comparison", "(6 & 3) == 2", "bool", "ret i1 true\n");
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
    "fn f(value vec[2,u32]) vec[2,u32] { return bitcast[arr[2,u32]](value) }\n";
  semantic_error "bitcast-struct-destination"
    "illegal cast for source and destination widths"
    "struct Pair { left u32 right u32 }\n\
    \ fn f(value vec[2,u32]) vec[2,u32] { return bitcast[Pair](value) }\n";
  semantic_error "cast-pointer-zext" "illegal cast for source and destination widths"
    "fn f(value addr) u64 { return zext[u64](value) }\n";
  semantic_error "cast-pointer-sext" "illegal cast for source and destination widths"
    "fn f(value addr) u64 { return sext[u64](value) }\n";
  semantic_error "cast-pointer-trunc" "illegal cast for source and destination widths"
    "fn f(value addr) u32 { return trunc[u32](value) }\n";
  semantic_error "cast-pointer-bitcast-bool"
    "illegal cast for source and destination widths"
    "fn f(value addr) bool { return bitcast[bool](value) }\n";
  semantic_error "cast-vector-bitcast-pointer"
    "illegal cast for source and destination widths"
    "fn f(value vec[1,u64]) addr { return bitcast[addr](value) }\n";
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
       fn check_case_it() bool { return unwrap[7](splat(true)) }\n\
       fn check_case_wide() u64 { return widen_generic[2](255) }\n"
  in
  List.iter
    (fun marker ->
      if not (contains generic_cast_path marker) then
        failwith ("generic-cast-path: missing `" ^ marker ^ "`"))
    [ "bitcast <1 x i1>"; "zext i8" ];
  semantic_error "raw-select-addr-add-both"
    "address arithmetic requires a scalar integer offset"
    "fn f(a addr, b addr) addr { return a + b }\n";
  semantic_error "raw-select-addr-sub-both"
    "address arithmetic requires a scalar integer offset"
    "fn f(a addr, b addr) addr { return a - b }\n";
  semantic_error "raw-select-int-addr-sub" "binary operands must have the same type"
    "fn f(n usize, p addr) usize { return n - p }\n";
  semantic_error "raw-select-handle-select" "cannot select through a handle"
    "opaque O\nfn f(h handle[O]) u8 { return h[u8] }\n";
  semantic_error "raw-select-aggregate-load"
    "raw selection cannot load an aggregate value"
    "struct S { a u8 }\nfn f(p addr) void { value S = p[S]\nreturn }\n";
  semantic_error "raw-select-aggregate-store"
    "raw selection cannot store an aggregate value"
    "struct S { a u8 }\nfn f(p addr) void { s S = (S){1}\np[S] = s\nreturn }\n";
  semantic_error "raw-select-lane" "raw vector lane selection is not yet supported"
    "fn f(p addr) u32 { return p[vec[2,u32]][0] }\n";
  semantic_error "raw-select-lane-store"
    "raw vector lane selection is not yet supported"
    "fn f(p addr) void { p[vec[2,u32]][0] = 1 }\n";
  semantic_error "raw-select-index-nonint"
    "raw selection index must be a scalar integer"
    "fn f(p addr, b bool) u32 { return p[u32, b] }\n";
  semantic_error "raw-select-three-payloads"
    "raw selection takes a type and an optional index"
    "fn f(p addr, i usize) u32 { return p[u32, i, 3] }\n";
  semantic_error "raw-select-value-payload" "unknown type `x`"
    "fn f(p addr, x u32) u32 { return p[x] }\n";
  semantic_error "raw-select-const-payload" "raw selection requires a type argument"
    "fn f(p addr) u32 { return p[3] }\n";
  semantic_error "raw-select-void" "raw selection requires a concrete type"
    "fn f(p addr) void { p[void] = p[void] }\n";
  semantic_error "raw-select-compound-mul-addr"
    "compound assignment requires an integer or vector"
    "fn f(p addr) void { p[addr] *= 2 }\n";
  semantic_error "raw-select-compound-addr-bad"
    "address arithmetic requires a scalar integer offset"
    "fn f(p addr, b bool) void { p[addr] += b }\n";
  semantic_error "raw-select-field-nonstruct" "field access requires a struct"
    "fn f(p addr) u32 { return p[u32].x }\n";
  semantic_error "raw-select-unknown-field" "unknown field `x`"
    "struct S { a u8 }\nfn f(p addr) u8 { return p[S].x }\n";
  let raw_load_shape = llvm_of "fn f(p addr) u32 { return p[u32] }\n" in
  List.iter
    (fun marker ->
      if not (contains raw_load_shape marker) then
        failwith ("raw-load-shape: missing `" ^ marker ^ "`"))
    [ "load i32, ptr %"; ", align 1" ];
  let raw_bool_shape = llvm_of "fn f(p addr) bool { return p[bool] }\n" in
  List.iter
    (fun marker ->
      if not (contains raw_bool_shape marker) then
        failwith ("raw-bool-shape: missing `" ^ marker ^ "`"))
    [ "load i8, ptr %"; "icmp ne i8" ];
  let raw_mask_shape = llvm_of "fn f(p addr) vec[3,bool] { return p[vec[3,bool]] }\n" in
  List.iter
    (fun marker ->
      if not (contains raw_mask_shape marker) then
        failwith ("raw-mask-shape: missing `" ^ marker ^ "`"))
    [ "load i8, ptr %"; "bitcast i8"; "extractelement"; "insertelement" ];
  let raw_addr_shape = llvm_of "fn f(p addr, n usize) addr { return p + n }\n" in
  List.iter
    (fun marker ->
      if not (contains raw_addr_shape marker) then
        failwith ("raw-addr-shape: missing `" ^ marker ^ "`"))
    [ "ptrtoint ptr"; "add i64"; "inttoptr i64" ];
  if contains raw_addr_shape "getelementptr i8, ptr" then
    failwith "raw-addr-shape: address arithmetic retained a GEP";
  let raw_normalize_shape = llvm_of "fn f(p addr, n u16) addr { return p - n }\n" in
  List.iter
    (fun marker ->
      if not (contains raw_normalize_shape marker) then
        failwith ("raw-normalize-shape: missing `" ^ marker ^ "`"))
    [ "zext i16"; "ptrtoint ptr"; "sub i64"; "inttoptr i64" ];
  let volatile_shapes =
    llvm_of
      "opaque Token\n\
       fn load_bool(p addr) bool { return volatile_load[bool](p) }\n\
       fn load_integer(p addr) u32 { return volatile_load[u32](p) }\n\
       fn load_address(p addr) addr { return volatile_load[addr](p) }\n\
       fn generic_load[T](p addr) T { return volatile_load[T](p) }\n\
       fn check_case_generic(p addr) u32 { return generic_load[u32](p) }\n\
       fn store_handle(p addr, value handle[Token]) void {\n\
       volatile_store[handle[Token]](p, value)\n\
       return\n\
       }\n\
       fn store_bool(p addr, value bool) void {\n\
       volatile_store[bool](p, value)\n\
       return\n\
       }\n\
       fn store_only(p addr) void {\n\
       volatile_store[u32](p, 7)\n\
       return\n\
       }\n"
  in
  List.iter
    (fun marker ->
      if not (contains volatile_shapes marker) then
        failwith ("volatile-shape: missing `" ^ marker ^ "`"))
    [
      "load volatile i8, ptr";
      "load volatile i32, ptr";
      "load volatile ptr, ptr";
      "store volatile i8";
      "store volatile ptr";
      "icmp ne i8";
      "zext i1";
      ", align 1";
    ];
  let volatile_store_only =
    llvm_of "fn store_only(p addr) void {\nvolatile_store[u32](p, 7)\nreturn\n}\n"
  in
  if not (contains volatile_store_only "store volatile i32") then
    failwith "volatile-store-no-read: missing volatile store";
  if contains volatile_store_only "load i32" then
    failwith "volatile-store-no-read: emitted a destination load";
  let volatile_vector_shapes =
    llvm_of
      "fn load_int(p addr) vec[3,u32] { return volatile_load[vec[3,u32]](p) }\n\
       fn store_int(p addr, v vec[3,u32]) void { volatile_store[vec[3,u32]](p, v)\n\
       return }\n\
       fn load_bool(p addr) vec[12,bool] { return volatile_load[vec[12,bool]](p) }\n\
       fn store_bool(p addr, v vec[12,bool]) void { volatile_store[vec[12,bool]](p, v)\n\
       return }\n"
  in
  List.iter
    (fun marker ->
      if not (contains volatile_vector_shapes marker) then
        failwith ("volatile-vector-shape: missing `" ^ marker ^ "`"))
    [
      "load volatile i32";
      "store volatile i32";
      "load volatile i16";
      "store volatile i16";
    ];
  List.iter
    (fun (needle, expected) ->
      if List.length (positions volatile_vector_shapes needle) <> expected then
        failwith ("volatile-vector-shape: wrong count for `" ^ needle ^ "`"))
    [
      ("load volatile i32", 3);
      ("store volatile i32", 3);
      ("load volatile i16", 1);
      ("store volatile i16", 1);
    ];
  let volatile_type_error =
    "volatile access type must be a scalar integer, bool, addr, handle[T], vec[N, \
     integer], or vec[N, bool]"
  in
  semantic_error "volatile-array" volatile_type_error
    "fn f(p addr) u32 { return volatile_load[arr[2,u32]](p) }\n";
  semantic_error "volatile-struct" volatile_type_error
    "struct S { value u32 }\nfn f(p addr) void { volatile_load[S](p)\nreturn }\n";
  semantic_error "volatile-store-expression" "volatile_store is statement-only"
    "fn f(p addr) u32 { return volatile_store[u32](p, 1) }\n";
  semantic_error "volatile-load-type-argument" "expects one type argument"
    "fn f(p addr) u32 { return volatile_load(p) }\n";
  semantic_error "volatile-store-arity" "expects two arguments"
    "fn f(p addr) void {\nvolatile_store[u32](p)\nreturn\n}\n";
  semantic_error "volatile-value-type" "type mismatch: expected u32, got bool"
    "fn f(p addr) void {\nvolatile_store[u32](p, true)\nreturn\n}\n";

  semantic_accept "place-init-ternary-escape-unknown"
    "struct S { x i64 y i64 }\n\
     fn f(p bool) i64 { s S\n\
     q addr = p ? &s : addr_from_bits(0)\n\
     t S\n\
     copy(t, s)\n\
     return 0 }\n";
  semantic_accept "aggregate-branch-no-else-unknown"
    "struct S { x i64 }\nfn f(p bool) i64 { s S\nif p { s.x = 1 }\nreturn s.x }\n";
  semantic_accept "aggregate-switch-partial-merge-unknown"
    "struct S { x i64 y i64 }\n\
     fn f(n i64) i64 { s S\n\
     switch n {\n\
     case 0: { s.x = 1 }\n\
     default: { s.y = 2 }\n\
     }\n\
     return s.x }\n";
  semantic_accept "aggregate-nested-dynamic-prefix-unknown"
    "fn f(i i64) i64 { a arr[2,arr[2,i64]]\n\
     a[0][0] = 1\n\
     a[0][1] = 2\n\
     return a[0][i] }\n";
  semantic_accept "aggregate-dynamic-write-read-unknown"
    "fn f(i i64) i64 { a arr[2,i64]\na[i] = 1\nreturn a[i] }\n";
  semantic_accept "aggregate-dynamic-write-preserves-subtree"
    "struct S { values arr[2,u32] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     value.values[i] = 7\n\
     return value.values[0] }\n";
  semantic_accept "aggregate-dynamic-compound-preserves-subtree"
    "struct S { values arr[2,u32] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     value.values[0] = 3\n\
     value.values[1] = 5\n\
     value.values[i] += 1\n\
     return value.values[0] }\n";
  semantic_accept "aggregate-dynamic-copy-preserves-subtree"
    "struct S { values arr[2,arr[2,u32]] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     source arr[2,u32] = {7, 11}\n\
     copy(value.values[i], source)\n\
     return value.values[0][0] }\n";
  semantic_accept "aggregate-dynamic-nested-loop-write"
    "fn f() u32 { values arr[2,arr[2,u32]]\n\
     for j usize = 0; j < 2; j += 1 { for i usize = 0; i < 2; i += 1 {\n\
     values[j][i] = trunc[u32](j * 2 + i) } }\n\
     return values[1][1] }\n";
  semantic_accept "aggregate-dynamic-pointer-element-unknown"
    "fn f(i i64) i64 { x i64\na arr[2,addr]\na[0] = &x\na[i][i64,0] = 1\nreturn x }\n";
  semantic_accept "aggregate-address-root-remains-unknown"
    "struct S { left u32 right u32 }\n\
     fn f() u32 { value S\n\
     pointer addr = &value\n\
     pointer[S].left = 13\n\
     return value.right }\n";
  semantic_accept "aggregate-address-field-preserves-target"
    "struct S { left u32 right u32 }\n\
     fn f() u32 { value S\n\
     pointer addr = &value.left\n\
     return value.left }\n";
  semantic_accept "aggregate-view-dynamic-write-preserves-subtree"
    "struct S { values arr[2,u32] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     view values = value.values\n\
     values[i] = 17\n\
     return value.values[0] }\n";
  semantic_accept "aggregate-branch-partial-unknown"
    "struct S { x i64 y i64 }\n\
     fn f(p bool) i64 { s S\n\
     if p { s.x = 1 } else { s.y = 2 }\n\
     return s.x }\n";
  semantic_accept "aggregate-loop-only-unknown"
    "struct S { x i64 }\nfn f(p bool) i64 { s S\nwhile p { s.x = 1 }\nreturn s.x }\n";
  semantic_accept "aggregate-branch-raw-missing-field-unknown"
    "struct S { x i64 y i64 }\n\
     fn take(p addr) void { return }\n\
     fn f(p bool) i64 { s S\n\
     if p { take(&s) } else { s.x = 1 }\n\
     return s.y }\n";
  semantic_accept "aggregate-branch-raw-whole-unknown"
    "struct S { x i64 y i64 }\n\
     fn take(p addr) void { return }\n\
     fn f(p bool) i64 { s S\n\
     if p { take(&s) } else { s.x = 1 }\n\
     t S\n\
     copy(t, s)\n\
     return 0 }\n";
  semantic_accept "for-step-path-merge-unknown"
    "fn take(value i64) void { return }\n\
     fn f(condition bool) void { for value i64; true; take(value) {\n\
     if condition { value = 1 } else { continue }\n\
     } }\n";
  semantic_accept "conditional-loop-initialization-unknown"
    "fn f(condition bool) i64 { value i64\n\
     while condition { value = 1\n\
     break }\n\
     return value }\n";
  semantic_accept "switch-loop-exit-initialization-unknown"
    "fn f(choice i64) i64 { value i64\n\
     while true { switch choice {\n\
     case 0: { value = 1\n\
     break }\n\
     default: { break }\n\
     } }\n\
     return value }\n";
  semantic_accept "for-step-discard-unknown"
    "fn f() i64 { y i64\nfor i i32 = 0; i < 0; y = 5 { }\nreturn y }\n";
  semantic_accept "static-index-parameter-constant-shadow-unknown"
    "const Index i32 = 0\n\
     fn read(Index i32) i32 { values arr[2,i32]\n\
     values[0] = 7\n\
     return values[Index] }\n\
     fn main() i32 { return read(1) }\n";
  semantic_accept "static-index-nested-parameter-constant-shadow-unknown"
    "const Index i32 = 0\n\
     fn read(Index i32) i32 { values arr[2,i32]\n\
     values[0] = 7\n\
     return values[Index + 0] }\n\
     fn main() i32 { return read(1) }\n";

  semantic_error "aggregate-branch-no-else-established-uninitialized"
    "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\nfn f(p bool) i64 { s S\nif p { s.x = 1 }\nreturn s.y }\n";
  semantic_error "aggregate-dynamic-write-preserves-sibling"
    "use of uninitialized local `value`"
    "struct S { values arr[2,u32] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     value.values[i] = 7\n\
     return value.other }\n";
  semantic_error "aggregate-dynamic-compound-preserves-sibling"
    "use of uninitialized local `value`"
    "struct S { values arr[2,u32] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     value.values[0] = 3\n\
     value.values[1] = 5\n\
     value.values[i] += 1\n\
     return value.other }\n";
  semantic_error "aggregate-dynamic-copy-preserves-sibling"
    "use of uninitialized local `value`"
    "struct S { values arr[2,arr[2,u32]] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     source arr[2,u32] = {7, 11}\n\
     copy(value.values[i], source)\n\
     return value.other }\n";
  semantic_error "aggregate-address-field-preserves-sibling"
    "use of uninitialized local `value`"
    "struct S { left u32 right u32 }\n\
     fn f() u32 { value S\n\
     pointer addr = &value.left\n\
     return value.right }\n";
  semantic_error "aggregate-view-dynamic-write-preserves-sibling"
    "use of uninitialized local `value`"
    "struct S { values arr[2,u32] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     view values = value.values\n\
     values[i] = 17\n\
     return value.other }\n";
  semantic_error "aggregate-switch-established-uninitialized"
    "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn f(n i64) i64 { s S\n\
     switch n { case 0: { s.x = 1 } default: { s.x = 2 } }\n\
     return s.y }\n";
  semantic_error "aggregate-loop-established-uninitialized"
    "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn f(p bool) i64 { s S\n\
     while p { s.x = 1 }\n\
     return s.y }\n";

  semantic_accept "constant-if-true-flow-pruning"
    "fn f() i64 { x i64\nif true { x = 1 } else { }\nreturn x }\n";
  semantic_error "constant-if-true-flow-pruning-rejects-selected-uninit"
    "use of uninitialized local `x`"
    "fn f() i64 { x i64\nif true { } else { x = 1 }\nreturn x }\n";
  semantic_accept "constant-if-false-flow-pruning"
    "fn f() i64 { x i64\nif false { } else { x = 1 }\nreturn x }\n";
  semantic_error "constant-if-false-flow-pruning-rejects-selected-uninit"
    "use of uninitialized local `x`"
    "fn f() i64 { x i64\nif false { x = 1 } else { }\nreturn x }\n";
  semantic_accept "constant-while-false-flow-pruning"
    "fn f() i64 { x i64 = 1\nwhile false { x = 2 }\nreturn x }\n";
  semantic_error "constant-while-false-flow-pruning-rejects-body-only-init"
    "use of uninitialized local `x`"
    "fn f() i64 { x i64\nwhile false { x = 1 }\nreturn x }\n";
  semantic_accept "constant-for-false-flow-pruning"
    "fn f() i64 { x i64 = 1\nfor i i32 = 0; false; i += 1 { x = 2 }\nreturn x }\n";
  semantic_error "constant-for-false-flow-pruning-rejects-body-only-init"
    "use of uninitialized local `x`"
    "fn f() i64 { x i64\nfor i i32 = 0; false; i += 1 { x = 1 }\nreturn x }\n";
  semantic_accept "constant-switch-matching-case-flow-pruning"
    "fn f() i64 { x i64\nswitch 1 { case 1: { x = 1 } default: { } }\nreturn x }\n";
  semantic_error "constant-switch-matching-case-flow-pruning-rejects-default"
    "use of uninitialized local `x`"
    "fn f() i64 { x i64\nswitch 1 { case 1: { } default: { x = 1 } }\nreturn x }\n";
  semantic_accept "constant-switch-default-flow-pruning"
    "fn f() i64 { x i64\nswitch 9 { case 1: { } default: { x = 1 } }\nreturn x }\n";
  semantic_error "constant-switch-default-flow-pruning-rejects-case"
    "use of uninitialized local `x`"
    "fn f() i64 { x i64\nswitch 9 { case 1: { x = 1 } default: { } }\nreturn x }\n";
  semantic_accept "constant-switch-no-match-flow-pruning-preserves-fact"
    "fn f() i64 { x i64 = 7\nswitch 9 { case 1: { x = 1 } }\nreturn x }\n";
  semantic_error "constant-switch-no-match-flow-pruning"
    "use of uninitialized local `x`"
    "fn f() i64 { x i64\nswitch 9 { case 1: { x = 1 } }\nreturn x }\n";
  semantic_accept "constant-and-short-circuit-flow-pruning"
    "fn f() bool { return false && (1 / 0 == 0) }\n";
  semantic_error "constant-and-short-circuit-reaches-rhs"
    "division by zero is not a defined runtime operation"
    "fn f() bool { return true && (1 / 0 == 0) }\n";
  semantic_accept "constant-or-short-circuit-flow-pruning"
    "fn f() bool { return true || (1 / 0 == 0) }\n";
  semantic_error "constant-or-short-circuit-reaches-rhs"
    "division by zero is not a defined runtime operation"
    "fn f() bool { return false || (1 / 0 == 0) }\n";

  semantic_accept "copy-local-struct-array-facts"
    "struct Leaf { value i32 }\n\
     struct Outer { entries arr[2,Leaf] }\n\
     fn f() i32 { source Outer\n\
     source.entries[0].value = 11\n\
     source.entries[1].value = 12\n\
     destination Outer\n\
     copy(destination, source)\n\
     copy(destination, destination)\n\
     return destination.entries[1].value }\n";
  semantic_accept "copy-field-element-place"
    "struct Cell { value i64 }\n\
     struct Box { items arr[2,Cell] }\n\
     fn f() i64 { source Box\n\
     source.items[0].value = 19\n\
     destination Box\n\
     copy(destination.items[0], source.items[0])\n\
     return destination.items[0].value }\n";
  semantic_accept "copy-view-places"
    "struct S { value i64 }\n\
     fn f() i64 { source S\n\
     source.value = 23\n\
     destination S\n\
     view source_view = source\n\
     view destination_view = destination\n\
     copy(destination_view, source_view)\n\
     return destination.value }\n";
  semantic_accept "copy-raw-struct-and-array-places"
    "struct S { value i64 }\n\
     fn f(destination addr, source addr) void {\n\
     copy(destination[S], source[S])\n\
     copy(destination[arr[2,u32]], source[arr[2,u32]])\n\
     return }\n";
  semantic_accept "copy-constant-array-source"
    "const Values arr[2,u32] = {7, 9}\n\
     fn f() u32 { destination arr[2,u32]\n\
     copy(destination, Values)\n\
     return destination[1] }\n";
  semantic_accept "copy-unknown-raw-source-marks-destination-unknown"
    "struct S { x i64 y i64 }\n\
     fn f(source addr) i64 { destination S\n\
     copy(destination, source[S])\n\
     return destination.y }\n";
  semantic_accept "copy-transfers-unknown-local-facts"
    "struct S { x i64 y i64 }\n\
     fn forget(p addr) void { return }\n\
     fn f() i64 { source S\n\
     source.x = 1\n\
     forget(&source)\n\
     destination S\n\
     copy(destination, source)\n\
     return destination.y }\n";
  semantic_error "copy-expression-is-statement-only" "copy is statement-only"
    "fn f() i32 { return copy(1, 2) }\n";
  semantic_error "copy-generic-form-rejected" "copy takes no type arguments"
    "fn f(destination addr, source addr) void {\n\
     copy[arr[2,u8]](destination, source)\n\
     return }\n";
  semantic_error "copy-non-aggregate-operands" "copy requires array or struct places"
    "fn f() void { destination i32 = 1\n\
     source i32 = 2\n\
     copy(destination, source)\n\
     return }\n";
  semantic_error "copy-vector-operands" "copy requires array or struct places"
    "fn f() void { destination vec[2,u32] = splat(1)\n\
     source vec[2,u32] = splat(2)\n\
     copy(destination, source)\n\
     return }\n";
  semantic_error "copy-mismatched-aggregate-types"
    "copy operands must have identical types"
    "fn f() void { destination arr[2,u32]\n\
     source arr[3,u32]\n\
     copy(destination, source)\n\
     return }\n";
  semantic_error "copy-rvalue-destination" "copy operands must be existing places"
    "struct S { value i64 }\n\
     fn f(condition bool) void { source S = {1}\n\
     copy(condition ? source : source, source)\n\
     return }\n";
  semantic_error "copy-rvalue-source" "copy operands must be existing places"
    "struct S { value i64 }\n\
     fn f(condition bool) void { destination S = {1}\n\
     copy(destination, condition ? destination : destination)\n\
     return }\n";
  semantic_error "copy-constant-destination" "cannot modify constant"
    "const Values arr[2,u32] = {7, 9}\n\
     fn f() void { source arr[2,u32]\n\
     copy(Values, source)\n\
     return }\n";
  semantic_error "copy-readonly-destination" "cannot modify read-only pointer"
    "fn f(source addr) void {\ncopy(c\"ab\"[arr[2,u8]], source[arr[2,u8]])\nreturn }\n";
  semantic_error "copy-uninitialized-source" "use of uninitialized local `source`"
    "struct S { value i64 }\n\
     fn f() void { destination S\n\
     source S\n\
     copy(destination, source)\n\
     return }\n";

  semantic_accept "construction-struct-brace-initializer"
    "struct Pair { left i32 right bool }\n\
     fn f() i32 { value Pair = {13, true}\n\
     if value.right { return value.left } else { return 0 } }\n";
  semantic_accept "construction-explicit-struct-initializer"
    "struct Pair { left i32 right i32 }\n\
     fn f() i32 { value Pair = (Pair){5, 8}\n\
     return value.left + value.right }\n";
  semantic_accept "construction-array-initializer"
    "fn f() i32 { values arr[3,i32] = {2, 3, 5}\n\
     return values[0] + values[1] + values[2] }\n";
  semantic_accept "construction-nested-array-of-struct"
    "struct Cell { x i32 y i32 }\n\
     struct Board { cells arr[2,Cell] }\n\
     fn f() i32 { board Board = {{{7, 11}, {13, 17}}}\n\
     return board.cells[1].y }\n";
  semantic_accept "construction-explicit-nested-entry"
    "struct Cell { x i32 y i32 }\n\
     struct Board { cell Cell }\n\
     fn f() i32 { board Board = {(Cell){19, 23}}\n\
     return board.cell.y }\n";
  semantic_accept "construction-explicit-array-initializer"
    "fn f() i32 { values arr[2,i32] = (arr[2,i32]){19, 23}\nreturn values[1] }\n";
  semantic_accept "construction-vector-literal-shuffle-selector"
    "fn f() u32 { a vec[4,u32] = (vec[4,u32]){11, 13, 17, 19}\n\
     b vec[4,u32] = shuffle(a, a, (vec[4,u32]){3, 2, 1, 0})\n\
     return b[0] + b[1] + b[2] + b[3] }\n";
  semantic_accept "construction-local-facts-are-initialized"
    "struct S { left i32 right i32 }\n\
     fn f() i32 { value S = {29, 31}\n\
     return value.left + value.right }\n";
  semantic_accept "construction-vector-value"
    "fn f() u32 { values vec[2,u32] = (vec[2,u32]){37, 41}\n\
     return values[0] + values[1] }\n";
  semantic_accept "construction-contextual-vector"
    "fn f() u32 { values vec[4,u32] = {1, 2, 3, 4}\nreturn values[3] }\n";
  semantic_accept "construction-contextual-vector-in-struct"
    "struct S { values vec[2,u32] tag u32 }\n\
     fn f() u32 { value S = {{5, 7}, 11}\n\
     return value.values[1] + value.tag }\n";
  semantic_accept "construction-contextual-array-of-vectors"
    "fn f() u32 { values arr[2,vec[2,u32]] = {{13, 17}, {19, 23}}\n\
     return values[1][0] + values[1][1] }\n";
  semantic_error "construction-array-entry-count"
    "wrong number of array literal elements"
    "fn f() void { values arr[2,i32] = {1}\nreturn }\n";
  semantic_error "construction-struct-entry-count"
    "wrong number of struct literal fields"
    "struct Pair { left i32 right i32 }\nfn f() void { value Pair = {1}\nreturn }\n";
  semantic_error "construction-vector-entry-count"
    "wrong number of vector literal lanes"
    "fn f() void { value vec[2,i32] = (vec[2,i32]){1}\nreturn }\n";
  semantic_error "construction-scalar-literal-destination"
    "construction needs an array, struct or vector type"
    "fn f() void { value u32 = {1}\nreturn }\n";
  semantic_error "construction-entry-type-mismatch"
    "type mismatch: expected i32, got bool"
    "struct S { flag i32 }\nfn f() void { value S = {true}\nreturn }\n";
  semantic_error "construction-explicit-type-mismatch"
    "aggregate construction type does not match destination"
    "struct A { value i32 }\n\
     struct B { value i32 }\n\
     fn f() void { value A = (B){1}\n\
     return }\n";
  semantic_error "construction-brace-needs-destination"
    "aggregate construction needs a destination"
    "fn consume(pointer addr) i32 { return 0 }\nfn f() i32 { return consume({43}) }\n";
  semantic_error "construction-aggregate-expression-needs-destination"
    "aggregate construction needs a destination"
    "struct S { value i32 }\n\
     fn consume(pointer addr) i32 { return 0 }\n\
     fn f() i32 { return consume((S){47}) }\n";
  semantic_error "construction-new-name-not-in-scope" "unknown name `value`"
    "fn f() i32 { value arr[1,i32] = {value[0]}\nreturn 0 }\n";

  let copy_direct_locals =
    lower_of
      "fn f() i64 { source arr[2,i64]\n\
       source[0] = 3\n\
       source[1] = 5\n\
       destination arr[2,i64]\n\
       copy(destination, source)\n\
       return destination[1] }\n"
  in
  let copy_direct_fn =
    List.find (fun (func : Ir.func) -> func.name = "f") copy_direct_locals.Ir.funcs
  in
  let copy_array_allocas =
    List.concat_map
      (fun (block : Ir.block) ->
        List.filter_map
          (function
            | Ir.Alloca (_, Ir.Array (2, Ir.I64), _) -> Some block.id | _ -> None)
          block.instrs)
      copy_direct_fn.blocks
  in
  if
    List.length copy_array_allocas <> 2
    || not (List.for_all (( = ) 0) copy_array_allocas)
  then failwith "copy-distinct-locals: expected two local allocas and no scratch";

  semantic_accept "simd-memory-six-forms"
    "fn ml(p addr, m vec[3,bool], f vec[3,u32]) vec[3,u32] { return \
     masked_load[u32](p, m, f) }\n\
     fn ms(p addr, m vec[3,bool], v vec[3,bool]) void { masked_store[bool](p, m, v)\n\
     return }\n\
     fn ga(p addr, i vec[3,i8], m vec[3,bool], f vec[3,bool]) vec[3,bool] { return \
     gather[bool](p, i, m, f) }\n\
     fn sc(p addr, i vec[3,u64], m vec[3,bool], v vec[3,u16]) void { scatter[u16](p, \
     i, m, v)\n\
     return }\n\
     fn gb(p addr, i vec[3,u64], m vec[3,bool], f vec[3,u8]) vec[3,u8] { return \
     gather_bytes[u8](p, i, m, f) }\n\
     fn sb(p addr, i vec[3,i8], m vec[3,bool], v vec[3,u32]) void { \
     scatter_bytes[u32](p, i, m, v)\n\
     return }\n";
  semantic_accept "simd-memory-generic-specialization"
    "fn load[T](p addr, m vec[3,bool], f vec[3,T]) vec[3,T] { return masked_load[T](p, \
     m, f) }\n\
     fn check_case(p addr, m vec[3,bool], f vec[3,u32]) vec[3,u32] { return \
     load[u32](p, m, f) }\n";
  semantic_accept "simd-memory-generic-store-specialization"
    "fn store[T](p addr, i vec[3,i8], m vec[3,bool], v vec[3,T]) void { \
     scatter_bytes[T](p, i, m, v)\n\
     return }\n\
     fn check_case(p addr, i vec[3,i8], m vec[3,bool], v vec[3,u16]) void { \
     store[u16](p, i, m, v)\n\
     return }\n";
  let simd_memory_type_error =
    "SIMD memory element type must be a scalar integer or bool"
  in
  semantic_error "simd-memory-aggregate-element" simd_memory_type_error
    "fn f(p addr, m vec[2,bool], v vec[2,u32]) vec[2,u32] { return \
     masked_load[arr[2,u32]](p, m, v) }\n";
  semantic_error "simd-memory-address-element" simd_memory_type_error
    "fn f(p addr, m vec[2,bool], v vec[2,u32]) vec[2,u32] { return \
     masked_load[addr](p, m, v) }\n";
  semantic_error "simd-memory-vector-element" simd_memory_type_error
    "fn f(p addr, m vec[2,bool], v vec[2,u32]) vec[2,u32] { return \
     masked_load[vec[2,u32]](p, m, v) }\n";
  semantic_error "simd-memory-handle-element" simd_memory_type_error
    "opaque Token\n\
     fn f(p addr, i vec[2,i8], m vec[2,bool], v vec[2,u32]) vec[2,u32] { return \
     gather[handle[Token]](p, i, m, v) }\n";
  semantic_error "simd-memory-mask-type" "masked_load mask must be a bool vector"
    "fn f(p addr, m vec[2,u32], v vec[2,u32]) vec[2,u32] { return masked_load[u32](p, \
     m, v) }\n";
  semantic_error "simd-memory-index-type" "gather indices must be an integer vector"
    "fn f(p addr, i vec[2,bool], m vec[2,bool], v vec[2,u32]) vec[2,u32] { return \
     gather[u32](p, i, m, v) }\n";
  semantic_error "simd-memory-lane-count"
    "type mismatch: expected vec[2, bool], got vec[3, bool]"
    "fn f(p addr, i vec[2,i8], m vec[3,bool], v vec[2,u32]) vec[2,u32] { return \
     gather[u32](p, i, m, v) }\n";
  semantic_error "simd-memory-fallback-type"
    "type mismatch: expected vec[2, u32], got vec[3, u32]"
    "fn f(p addr, m vec[2,bool], v vec[3,u32]) vec[2,u32] { return masked_load[u32](p, \
     m, v) }\n";
  semantic_error "simd-memory-base-type" "type mismatch: expected addr, got u32"
    "fn f(p u32, m vec[2,bool], v vec[2,u32]) vec[2,u32] { return masked_load[u32](p, \
     m, v) }\n";
  semantic_error "simd-memory-load-arity" "builtin `masked_load` expects 3 arguments"
    "fn f(p addr, m vec[2,bool]) vec[2,u32] { return masked_load[u32](p, m) }\n";
  semantic_error "simd-memory-gather-arity" "builtin `gather` expects 4 arguments"
    "fn f(p addr, i vec[2,i8], m vec[2,bool]) vec[2,u32] { return gather[u32](p, i, m) }\n";
  semantic_error "simd-memory-byte-gather-arity"
    "builtin `gather_bytes` expects 4 arguments"
    "fn f(p addr, i vec[2,i8], m vec[2,bool]) vec[2,u32] { return gather_bytes[u32](p, \
     i, m) }\n";
  semantic_error "simd-memory-store-arity" "builtin `masked_store` expects 3 arguments"
    "fn f(p addr, m vec[2,bool]) void { masked_store[u32](p, m)\nreturn }\n";
  semantic_error "simd-memory-scatter-arity" "builtin `scatter` expects 4 arguments"
    "fn f(p addr, i vec[2,i8], m vec[2,bool]) void { scatter[u32](p, i, m)\nreturn }\n";
  semantic_error "simd-memory-byte-scatter-arity"
    "builtin `scatter_bytes` expects 4 arguments"
    "fn f(p addr, i vec[2,i8], m vec[2,bool]) void { scatter_bytes[u32](p, i, m)\n\
     return }\n";
  semantic_error "simd-memory-missing-type"
    "builtin `masked_load` expects one type argument"
    "fn f(p addr, m vec[2,bool], v vec[2,u32]) vec[2,u32] { return masked_load(p, m, \
     v) }\n";
  semantic_error "simd-memory-store-missing-type"
    "builtin `masked_store` expects one type argument"
    "fn f(p addr, m vec[2,bool], v vec[2,u32]) void { masked_store(p, m, v)\nreturn }\n";
  semantic_error "simd-memory-masked-store-expression" "masked_store is statement-only"
    "fn f(p addr, m vec[2,bool], v vec[2,u32]) u32 { return masked_store[u32](p, m, v) }\n";
  semantic_error "simd-memory-scatter-expression" "scatter is statement-only"
    "fn f(p addr, i vec[2,i8], m vec[2,bool], v vec[2,u32]) u32 { return \
     scatter[u32](p, i, m, v) }\n";
  semantic_error "simd-memory-scatter-bytes-expression"
    "scatter_bytes is statement-only"
    "fn f(p addr, i vec[2,i8], m vec[2,bool], v vec[2,u32]) u32 { return \
     scatter_bytes[u32](p, i, m, v) }\n";
  semantic_error "simd-memory-store-constant" "cannot modify constant"
    "const C arr[2,u32] = {3, 5}\n\
     fn f(m vec[2,bool], v vec[2,u32]) void { masked_store[u32](&C, m, v)\n\
     return }\n";
  semantic_error "simd-memory-store-readonly" "cannot modify read-only pointer"
    "fn f(m vec[4,bool], v vec[4,u8]) void { masked_store[u8](c\"data\", m, v)\n\
     return }\n";
  semantic_error "simd-memory-constant-context"
    "expression is not compile-time constant"
    "const M vec[1,bool] = {true}\n\
     const F vec[1,u32] = {0}\n\
     const X u32 = masked_load[u32](null, M, F)[0]\n";
  semantic_error "simd-memory-gather-constant-context"
    "expression is not compile-time constant"
    "const I vec[1,i8] = {0}\n\
     const M vec[1,bool] = {true}\n\
     const F vec[1,u32] = {0}\n\
     const X u32 = gather[u32](null, I, M, F)[0]\n";
  semantic_error "simd-memory-gather-bytes-constant-context"
    "expression is not compile-time constant"
    "const I vec[1,i8] = {0}\n\
     const M vec[1,bool] = {true}\n\
     const F vec[1,u32] = {0}\n\
     const X u32 = gather_bytes[u32](null, I, M, F)[0]\n";

  semantic_accept "global-declaration-forms"
    "struct Pair { x i32\n\
     y i32 }\n\
     var Zero i32\n\
     var Number i32 = 7\n\
     var Values arr[2,i32] = {3, 5}\n\
     var Item Pair = {11, 13}\n\
     extern \"C\" {\n\
     var Exported i32 = 17\n\
     var Imported i32\n\
     }\n\
     fn update() i32 {\n\
     Zero = Number\n\
     Zero += Values[0]\n\
     Item.y = Zero\n\
     return Imported + Exported + Item.y}\n";
  semantic_accept "global-address-place"
    "var Number i32 = 7\nfn address() addr { return &Number }\n";
  semantic_accept "global-inner-shadow"
    "var Value i32 = 7\nfn local() i32 { Value i32 = 11\n return Value }\n";
  semantic_accept "global-handle-type"
    "opaque Token\n\
     var Current handle[Token] = null\n\
     fn current_token() handle[Token] { return Current }\n";
  let cross_file_global =
    check_files
      [
        ("globals.fas", "var Shared i32 = 19\n");
        ("reader.fas", "fn read() i32 { return Shared }\n");
      ]
  in
  ignore cross_file_global;
  semantic_message "global-duplicate" "duplicate global `Value`"
    "var Value i32\nvar Value u32\n";
  semantic_message "global-function-name-collision" "duplicate declaration `Value`"
    "var Value i32\nfn Value() i32 { return 0 }\n";
  parse_message "global-reserved-var" "expected identifier, found `var`" "var var i32\n";
  semantic_message "global-initializer-not-constant"
    "global initializer must be a constant expression"
    "fn dynamic() i32 { return 1 }\nvar Value i32 = dynamic()\n";
  semantic_message "global-initializer-global-read" "global `Source` is not a constant"
    "var Source i32 = 1\nvar Value i32 = Source\n";
  semantic_accept "global-address-initializer"
    "var Target u32\nvar Pointer addr = &Target\n";
  semantic_accept "address-constants-forward-cycle" "var A addr = &B\nvar B addr = &A\n";
  semantic_accept "address-constants-c-string-table"
    "const Names arr[2,addr] = {c\"TROO\", c\"SARG\"}\n\
     fn get() addr { return Names[1] }\n";
  semantic_accept "address-constants-nested-selectors"
    "struct Pair { x i32\n\
     y arr[3,i32] }\n\
     var G Pair\n\
     const T arr[1,arr[2,addr]] = {{&G.x, &G.y[1+1]}}\n";
  semantic_accept "address-constants-forward-const-target"
    "const Links arr[1,addr] = {&Later[0]}\nconst Later arr[1,i32] = {7}\n";
  semantic_accept "address-constants-imported-target"
    "extern \"C\" { var Imported i32 }\nvar P addr = &Imported\n";
  semantic_accept "address-constants-handle-slot"
    "opaque Token\nvar G i32\nvar P handle[Token] = handle_from_addr[Token](&G)\n";
  semantic_accept "address-constants-null-table"
    "const P arr[1,addr] = {null}\nfn f() addr { return P[0] }\n";
  semantic_accept "address-constants-nested-struct-table"
    "struct Link { p addr }\n\
     struct Menu { n i32\n\
     link Link }\n\
     var G i32\n\
     const M arr[1,Menu] = {{7,{&G}}}\n\
     fn f() addr { return M[0].link.p }\n";
  semantic_accept "address-constants-specialization"
    "var G i32\n\
     const P arr[1,addr] = {&G}\n\
     fn get[T](x T) addr { return P[0] }\n\
     fn f() addr { return get[i32](0) }\n";
  semantic_accept "address-constants-c-string-length-context"
    "const N usize = len(c\"abc\")\nfn f() usize { local arr[N,i32]\nreturn N }\n";
  semantic_accept "address-constants-length-query-specialization"
    "var G i32\n\
     const P arr[2,addr] = {&G,null}\n\
     const N usize = len(P)\n\
     const A arr[1,usize] = {len(P)}\n\
     fn get[T](x T) i32 { switch N { case len(P): return 7 }\n\
     return 1 }\n\
     fn f() i32 { return get[i32](0) }\n";
  let address_length =
    llvm_of
      "var G i32\n\
       const P arr[2,addr] = {&G,null}\n\
       const N usize = len(P)\n\
       fn f() usize { return N }\n"
  in
  if not (contains address_length "ret i64 2") then
    failwith "address-constants-length-query: incorrect fixed length";
  semantic_message "address-constants-oob" "array index is out of bounds"
    "var G arr[2,i32]\nvar P addr = &G[2]\n";
  semantic_message "address-constants-scalar-slot"
    "global initializer must be a constant expression" "var G i32\nvar P i32 = &G\n";
  semantic_message "address-constants-arithmetic" "address constants are storable only"
    "var G i32\nvar P addr = &G + 1\n";
  semantic_message "address-constants-comparison"
    "global initializer must be a constant expression"
    "var G i32\nconst P bool = &G == &G\n";
  semantic_message "address-constants-integer-conversion"
    "global initializer must be a constant expression"
    "var G i32\nconst P usize = zext[usize](&G)\n";
  semantic_message "address-constants-bitcast"
    "global initializer must be a constant expression"
    "var G i32\nconst P usize = bitcast[usize](&G)\n";
  semantic_message "address-constants-array-length"
    "aggregate length is not a machine integer"
    "var G i32\nconst P addr = &G\nvar A arr[P,i32]\n";
  semantic_message "address-constants-switch-case" "global `P` is not a constant"
    "var G i32\n\
     const P addr = &G\n\
     fn f() i32 { switch 0 { case P: return 1 }\n\
     return 0 }\n";
  parse_message "address-constants-sizeof-value" "expected a type, found `&`"
    "var G i32\nconst P usize = sizeof[&G]\n";
  semantic_message "address-constants-table-copy" "address constants are storable only"
    "var G i32\nconst P arr[1,addr] = {&G}\nconst Q arr[1,addr] = {P[0]}\n";
  semantic_message "address-constants-function-target" "function `f` is not a place"
    "fn f() void { return }\nvar P addr = &f\n";
  semantic_message "address-constants-scalar-constant-target"
    "constant `G` is not a place" "const G i32 = 1\nvar P addr = &G\n";
  semantic_message "address-constants-vector-constant-target"
    "cannot take the address of this expression"
    "const G vec[2,i32] = splat(1)\nvar P addr = &G\n";
  semantic_message "address-constants-ordinary-string"
    "address constants require a C string literal" "var P addr = \"x\"\n";
  semantic_message "address-constants-local-target" "unknown name `Local`"
    "fn f() void { Local i32 = 1\nreturn }\nvar P addr = &Local\n";
  semantic_message "address-constants-handle-ordinary-string"
    "address constants require a C string literal"
    "opaque Token\nvar P handle[Token] = handle_from_addr[Token](\"x\")\n";
  semantic_message "address-constants-readonly-table" "cannot modify read-only pointer"
    "var G i32\nconst P arr[1,addr] = {&G}\nfn f() void { P[0] = null\nreturn }\n";
  let relocatable =
    llvm_of
      "struct Pair { x i32\n\
       p addr\n\
       y i32 }\n\
       var G arr[3,i32]\n\
       var T Pair = {7,&G[1+1],9}\n"
  in
  if
    (not (contains relocatable "@T = internal global <{ [8 x i8], ptr, [8 x i8] }>"))
    || (not (contains relocatable "getelementptr (i8, ptr @G, i64 8)"))
    || contains relocatable "inbounds"
  then failwith "address-constants-mixed-layout: invalid relocation or layout";
  semantic_message "const-global-read" "global `Value` is not a constant"
    "var Value i32 = 1\nconst Copy i32 = Value\n";
  semantic_message "global-array-length-read" "global `Count` is not a constant"
    "var Count usize = 2\nfn run() void { local arr[Count,i32]\nreturn }\n";
  semantic_message "global-generic-argument-read" "global `Count` is not a constant"
    "var Count i32 = 2\n\
     fn get[N const i32]() i32 { return N }\n\
     fn run() i32 { return get[Count]() }\n";
  semantic_message "global-case-label-read" "global `Choice` is not a constant"
    "var Choice i32 = 2\n\
     fn run() i32 { switch 0 { case Choice: return 1 }\n\
     return 0 }\n";
  semantic_message "global-array-initializer-arity"
    "wrong number of array literal elements" "var Values arr[2,i32] = {1}\n";
  semantic_message "global-struct-initializer-arity"
    "wrong number of struct literal fields"
    "struct Pair { x i32\ny i32 }\nvar Item Pair = {1}\n";
  semantic_message "global-opaque-object"
    "opaque type `Token` may only be used behind a pointer"
    "opaque Token\nvar Value Token\n";
  parse_message "global-local-var" "expected an expression, found `var`"
    "fn run() void { var Value i32\nreturn }\n";

  let c_matrix = c_import_fixture "matrix.h" in
  let c_matrix_source, c_matrix_imported = c_matrix in
  let stdio_declarations, _, _ =
    expect_ok
      (C_import.import ~cc:"clang-22" ~debug:false ~keep:false c_matrix_source
         [ C_import.{ spelling = Ast.C_system "stdio.h"; span = Span.synthetic } ])
  in
  let stdio_import =
    C_import.map_declarations ~span:Span.synthetic stdio_declarations
  in
  incr checks_run;
  if
    List.length
      (List.filter
         (function Ast.Func function_ -> function_.name = "printf" | _ -> false)
         stdio_import.items)
    <> 1
    || List.mem_assoc "printf" stdio_import.unsupported
  then failwith "implicit C builtin declaration conflicted with stdio.h printf";
  let restrict_manifest =
    C_import.manifest_text c_matrix_imported
    |> String.split_on_char '\n'
    |> List.find_opt (fun line ->
        String.starts_with ~prefix:"fas_restrict_pointer\t" line)
  in
  incr checks_run;
  (match restrict_manifest with
  | Some line -> (
      match String.split_on_char '\t' line with
      | [ "fas_restrict_pointer"; spelling; signature; qualifiers; location ]
        when spelling = "void (int *restrict) fas_restrict_pointer"
             && signature = "fn(addr)->void" && qualifiers = "restrict"
             && contains location "matrix.h:" ->
          ()
      | columns ->
          failwith ("C manifest columns changed: " ^ String.concat " | " columns))
  | None -> failwith "C manifest omitted the restrict function");
  let provenance_dir = Filename.dirname c_matrix_source in
  let provenance_headers =
    List.map
      (fun (name, line) ->
        C_import.
          {
            spelling = Ast.C_quoted name;
            span =
              Span.make ~file:c_matrix_source ~start_offset:0 ~end_offset:0 ~line
                ~column:1;
          })
      [ ("provenance_first.h", 1); ("provenance_second.h", 2) ]
  in
  let provenance_declarations, _, _ =
    expect_ok
      (C_import.import ~cc:"clang-22" ~debug:false ~keep:false c_matrix_source
         provenance_headers)
  in
  let provenance_import =
    C_import.map_declarations ~span:Span.synthetic provenance_declarations
  in
  List.iter
    (fun (name, header, line) ->
      incr checks_run;
      let expected_file = Filename.concat provenance_dir header ^ ":" ^ line in
      match
        List.find_opt
          (fun entry -> String.starts_with ~prefix:(name ^ "\t") entry)
          provenance_import.manifest
      with
      | Some entry when contains entry expected_file -> ()
      | Some entry ->
          failwith
            (Printf.sprintf "C provenance %s: expected %s, got %s" name expected_file
               entry)
      | None -> failwith ("C provenance declaration missing: " ^ name))
    [
      ("FasProvenanceFirstType", "provenance_first.h", "1");
      ("fas_provenance_first", "provenance_first.h", "2");
      ("fas_provenance_macro", "provenance_first.h", "4");
      ("FasProvenanceSecondType", "provenance_second.h", "1");
      ("fas_provenance_second", "provenance_second.h", "2");
    ];
  incr checks_run;
  let container_source_path = Filename.temp_file "fas-container-import-" ".fas" in
  let container_local_header = container_source_path ^ ".h" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [ container_source_path; container_local_header ])
    (fun () ->
      let header_channel = open_out_bin container_local_header in
      output_string header_channel "#define FAS_CONTAINER_LOCAL 2\n";
      close_out header_channel;
      let container_source =
        Printf.sprintf
          "use \"C\" <stddef.h>\n\
           use \"C\" <<FIRST\n\
           #define FAS_CONTAINER_MACRO 40\n\
           FIRST\n\
           use \"C\" <limits.h>\n\
           use \"C\" <<SECOND\n\
           #include \"%s\"\n\
           int fas_fragment_value(void) { return FAS_CONTAINER_MACRO + CHAR_BIT + \
           FAS_CONTAINER_LOCAL; }\n\
           static int fas_fragment_static(int value) { return value + 1; }\n\
           static int fas_unused_fragment_static(void) { return 3; }\n\
           SECOND\n"
          (Filename.basename container_local_header)
      in
      let parsed =
        match
          Parser.parse
            (Source.create ~file:container_source_path ~text:container_source)
        with
        | Ok parsed -> parsed
        | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
      in
      let requests =
        List.filter_map
          (function
            | Ast.Use { c_header = Some spelling; span; _ } ->
                Some C_import.{ spelling; span }
            | _ -> None)
          parsed.items
      in
      let declarations, kept, _ =
        expect_ok
          (C_import.import ~cc:"clang-22" ~debug:false ~keep:true container_source_path
             requests)
      in
      let kept = Option.value ~default:[] kept in
      Fun.protect
        ~finally:(fun () ->
          List.iter (fun path -> try Sys.remove path with Sys_error _ -> ()) kept)
        (fun () ->
          match kept with
          | [ unit_path; first_fragment; second_fragment ] -> (
              let read path =
                let channel = open_in_bin path in
                Fun.protect
                  ~finally:(fun () -> close_in_noerr channel)
                  (fun () -> really_input_string channel (in_channel_length channel))
              in
              let unit = read unit_path in
              let ordered =
                [
                  C_import.find_text unit "#include <stddef.h>" 0;
                  C_import.find_text unit ("#include \"" ^ first_fragment ^ "\"") 0;
                  C_import.find_text unit "#include <limits.h>" 0;
                  C_import.find_text unit ("#include \"" ^ second_fragment ^ "\"") 0;
                ]
              in
              (match ordered with
              | [ Some first; Some second; Some third; Some fourth ]
                when first < second && second < third && third < fourth ->
                  ()
              | _ -> failwith "C unit did not preserve header and fragment source order");
              let first_line = (List.nth requests 1).C_import.span.Span.line + 1
              and second_line = (List.nth requests 3).C_import.span.Span.line + 1 in
              if
                (not
                   (String.starts_with
                      ~prefix:
                        (Printf.sprintf "#line %d %S\n" first_line container_source_path)
                      (read first_fragment)))
                || not
                     (String.starts_with
                        ~prefix:
                          (Printf.sprintf "#line %d %S\n" second_line
                             container_source_path)
                        (read second_fragment))
              then failwith "C fragment files did not start at their Fas source lines";
              let imported =
                C_import.map_declarations ~span:Span.synthetic declarations
              in
              if
                not
                  (List.exists
                     (String.starts_with ~prefix:"fas_fragment_value\t")
                     imported.manifest)
              then failwith "C function definition in a fragment was not imported";
              incr checks_run;
              List.iter
                (fun name ->
                  if
                    not
                      (List.exists
                         (fun (static : C_import.static_function) -> static.name = name)
                         imported.static_functions)
                  then
                    failwith ("static C function in fragment was not recorded: " ^ name))
                [ "fas_fragment_static"; "fas_unused_fragment_static" ];
              incr checks_run;
              match
                List.find_opt
                  (fun (static : C_import.static_function) ->
                    static.name = "fas_fragment_static")
                  imported.static_functions
              with
              | Some static -> (
                  match
                    C_import.make_adapter ~occupied:[] container_source_path static
                  with
                  | Ok adapter
                    when contains adapter.code "return fas_fragment_static(fas_arg0);"
                    ->
                      ()
                  | Ok _ ->
                      failwith "static C fragment adapter did not forward its call"
                  | Error message -> failwith message)
              | None -> failwith "static C fragment function was not recorded")
          | _ -> failwith "--keep omitted generated C units or fragment files"));
  List.iter
    (fun (name, expected) ->
      incr checks_run;
      match List.assoc_opt name c_matrix_imported.aliases with
      | Some actual when actual = expected -> ()
      | Some actual ->
          failwith
            (Printf.sprintf "C typedef %s: expected %s, got %s" name
               (Ast.type_name expected) (Ast.type_name actual))
      | None -> failwith ("C typedef was not imported: " ^ name))
    [
      ("fas_i8", Ast.Int Ast.I8);
      ("fas_u8", Ast.Int Ast.U8);
      ("fas_i16", Ast.Int Ast.I16);
      ("fas_u16", Ast.Int Ast.U16);
      ("fas_i32", Ast.Int Ast.I32);
      ("fas_u32", Ast.Int Ast.U32);
      ("fas_i64", Ast.Int Ast.I64);
      ("fas_u64", Ast.Int Ast.U64);
      ("fas_bool", Ast.Bool);
    ];
  let repeated_import =
    C_import.merge_imports [ c_matrix_imported; c_matrix_imported ]
  in
  let named_item name items =
    List.filter
      (function
        | Ast.Opaque { name = item_name; _ }
        | Ast.Const { name = item_name; _ }
        | Ast.Global { name = item_name; _ }
        | Ast.Func { name = item_name; _ } ->
            item_name = name
        | _ -> false)
      items
  in
  let nested_dir = Filename.concat (Filename.dirname c_matrix_source) "nested" in
  Unix.mkdir nested_dir 0o700;
  Fun.protect
    ~finally:(fun () -> Unix.rmdir nested_dir)
    (fun () ->
      let relative_source = Filename.concat nested_dir "probe.fas" in
      let relative_span =
        Span.make ~file:relative_source ~start_offset:0 ~end_offset:0 ~line:3 ~column:1
      in
      let relative_header =
        C_import.{ spelling = Ast.C_quoted "../matrix.h"; span = relative_span }
      in
      let relative_declarations, _, _ =
        expect_ok
          (C_import.import ~cc:"clang-22" ~debug:false ~keep:false relative_source
             [ relative_header ])
      in
      let relative_import =
        C_import.map_declarations ~span:relative_span relative_declarations
      in
      incr checks_run;
      if named_item "fas_i32_echo" relative_import.items = [] then
        failwith "quoted C header path was not resolved from a nested Fas file";
      let missing_span =
        Span.make ~file:relative_source ~start_offset:0 ~end_offset:0 ~line:9 ~column:4
      in
      let missing_header =
        C_import.{ spelling = Ast.C_quoted "../absent.h"; span = missing_span }
      in
      incr checks_run;
      match
        C_import.import ~cc:"clang-22" ~debug:false ~keep:false relative_source
          [ missing_header ]
      with
      | Error [ diagnostic ]
        when diagnostic.primary = missing_span
             && contains diagnostic.message "file not found" ->
          ()
      | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
      | Ok _ -> failwith "missing C header was accepted");
  incr checks_run;
  if List.length (named_item "fas_i32_echo" repeated_import.items) <> 1 then
    failwith "repeated C imports did not merge to one function binding";
  incr checks_run;
  if
    List.length
      (List.filter (fun (name, _) -> name = "fas_i32") repeated_import.aliases)
    <> 1
  then failwith "repeated C imports did not merge to one typedef binding";
  let matching =
    parse_file c_matrix_source
      "use \"C\" \"matrix.h\"\nextern \"C\" { fn fas_i32_echo(value i32) i32 }\n"
  in
  let matching_import =
    expect_ok (C_import.reconcile_source matching.items c_matrix_imported)
  in
  incr checks_run;
  if named_item "fas_i32_echo" matching_import.items <> [] then
    failwith "matching extern C declaration did not confirm its import";
  incr checks_run;
  (match
     Sema.check ~c_aliases:matching_import.aliases
       ~c_unsupported:matching_import.unsupported
       ~c_records:matching_import.record_types
       { Ast.items = matching.items @ matching_import.items }
   with
  | Ok _ -> ()
  | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics));
  let mismatching =
    parse_file c_matrix_source
      "use \"C\" \"matrix.h\"\nextern \"C\" { fn fas_i32_echo(value u32) i32 }\n"
  in
  incr checks_run;
  (match C_import.reconcile_source mismatching.items c_matrix_imported with
  | Error [ diagnostic ]
    when diagnostic.message
         = "C declaration `fas_i32_echo` has type `fn(i32)->i32`, but Fas declares \
            `fn(u32)->i32`" ->
      ()
  | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  | Ok _ -> failwith "mismatching extern C declaration was accepted");
  let c_conflict = snd (c_import_fixture "conflict.h") in
  let conflicted = C_import.merge_imports [ c_matrix_imported; c_conflict ] in
  c_semantic_accept "c-import-unused-cross-file-conflict" (c_matrix_source, conflicted)
    "fn main() i32 { return 0 }\n";
  c_semantic_message "c-import-cross-file-conflict"
    "C declaration `fas_i32_echo` is not supported: conflicting C declarations"
    (c_matrix_source, conflicted) "fn main() i32 { return fas_i32_echo(7) }\n";
  c_semantic_accept "c-import-unused-unsupported" c_matrix
    "fn main() i32 { return 0 }\n";
  c_semantic_accept "c-import-integer-alias-matrix" c_matrix
    "fn integers(a fas_i8, b fas_u8, c fas_i16, d fas_u16, e fas_i32, f fas_u32, g \
     fas_i64, h fas_u64, flag fas_bool) void {\n\
     fas_char_echo(a)\n\
     fas_i8_echo(a)\n\
     fas_u8_echo(b)\n\
     fas_i16_echo(c)\n\
     fas_u16_echo(d)\n\
     fas_i32_echo(e)\n\
     fas_u32_echo(f)\n\
     fas_i64_echo(g)\n\
     fas_u64_echo(h)\n\
     fas_bool_echo(flag)\n\
     return }\n";
  c_semantic_accept "c-import-typedef-chain" c_matrix
    "fn chain(value fas_i8_chain) fas_i8 { return value }\n";
  c_semantic_accept "c-import-enum-values-and-abi" c_matrix
    "fn enum_values() i64 {\n\
     fas_enum_arg(FAS_ENUM_NEG)\n\
     fas_enum_arg(FAS_ENUM_LARGE)\n\
     return FAS_ENUM_NEG }\n";
  c_semantic_accept "c-import-anonymous-enum-typedef-abi" c_matrix
    "fn anonymous_enum(value FasAnonymousEnum) FasAnonymousEnum {\n\
     return fas_anonymous_enum_echo(value) }\n";
  let enum_program =
    match
      c_semantic_result c_matrix "fn enum_values() i64 { return FAS_ENUM_LARGE }\n"
    with
    | Ok program -> program
    | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  in
  List.iter
    (fun (name, value) ->
      match
        List.find_opt
          (fun (constant : Hir.const_def) -> constant.name = name)
          enum_program.consts
      with
      | Some { ty = Hir.Int Hir.I64; bits; _ } when bits = value -> ()
      | Some { ty; bits; _ } ->
          failwith
            (Printf.sprintf "C enum %s: expected i64 %Ld, got %s %Ld" name value
               (Hir.ty_name ty) bits)
      | None -> failwith ("C enum constant not imported: " ^ name))
    [ ("FAS_ENUM_NEG", -3L); ("FAS_ENUM_LARGE", 0xffffffffL) ];
  let implicit_enum = c_import_fixture "enums.h" in
  let implicit_enum_program =
    match
      c_semantic_result implicit_enum
        "fn enum_values() i32 { return FAS_IMPLICIT_INT_MAX }\n"
    with
    | Ok program -> program
    | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  in
  List.iter
    (fun (name, value) ->
      match
        List.find_opt
          (fun (constant : Hir.const_def) -> constant.name = name)
          implicit_enum_program.consts
      with
      | Some { ty = Hir.Int Hir.I32; bits; _ } when bits = value -> ()
      | Some { ty; bits; _ } ->
          failwith
            (Printf.sprintf "C implicit enum %s: expected i32 %Ld, got %s %Ld" name
               value (Hir.ty_name ty) bits)
      | None -> failwith ("C implicit enum constant not imported: " ^ name))
    [
      ("FAS_IMPLICIT_FIRST", 0L);
      ("FAS_IMPLICIT_EXPLICIT", 7L);
      ("FAS_IMPLICIT_AFTER_EXPLICIT", 8L);
      ("FAS_IMPLICIT_NEGATIVE", 4294967291L);
      ("FAS_IMPLICIT_AFTER_NEGATIVE", 4294967292L);
      ("FAS_IMPLICIT_NEAR_MAX", 2147483646L);
      ("FAS_IMPLICIT_INT_MAX", 2147483647L);
    ];
  let c_records = c_import_fixture "records.h" in
  c_semantic_accept "c-import-typedef-anonymous-record-handle" c_records
    "fn probe(value handle[FasAnonymousRecord]) handle[FasAnonymousRecord] {\n\
    \     return fas_anonymous_record(value) }\n";
  c_semantic_message "c-import-record-without-typedef"
    "C declaration `fas_untyped_record` is not supported: struct and union values are \
     not supported"
    c_records "fn probe() i32 { return fas_untyped_record }\n";
  c_semantic_accept "c-import-pointer-matrix" c_matrix
    "fn pointers(p addr, bytes addr, record handle[FasRecord], other \
     handle[FasOtherRecord], same handle[FasSameRecord]) void {\n\
     fas_void_pointer(p)\n\
     fas_scalar_pointer(bytes)\n\
     fas_record_pointer(record)\n\
     fas_record_alias_pointer(record)\n\
     fas_other_pointer(other)\n\
     fas_same_record(same)\n\
     fas_pointer_output(p)\n\
     fas_variadic(1, true, 2)\n\
     return }\n";
  c_semantic_error "c-import-distinct-record-identities"
    "expected handle[FasOtherRecord], got handle[FasRecord]" c_matrix
    "fn wrong(record handle[FasRecord]) void {\nfas_other_pointer(record)\nreturn }\n";
  c_semantic_accept "c-import-global-places" c_matrix
    "fn globals() i32 {\n\
     value i32 = fas_mutable_global\n\
     fas_mutable_global = value\n\
     fas_mutable_pointer_global = null\n\
     fas_mutable_typedef_pointer_global = null\n\
     return fas_readonly_global }\n";
  c_semantic_message "c-import-readonly-global"
    "global `fas_readonly_global` is read-only" c_matrix
    "fn write() void { fas_readonly_global = 1\nreturn }\n";
  c_semantic_message "c-import-readonly-pointer-global"
    "global `fas_readonly_pointer_global` is read-only" c_matrix
    "fn write() void { fas_readonly_pointer_global = null\nreturn }\n";
  c_semantic_message "c-import-readonly-typedef-global"
    "global `fas_readonly_alias_global` is read-only" c_matrix
    "fn write() void { fas_readonly_alias_global = 1\nreturn }\n";
  c_semantic_message "c-import-readonly-typedef-pointer-global"
    "global `fas_readonly_typedef_pointer_global` is read-only" c_matrix
    "fn write() void { fas_readonly_typedef_pointer_global = null\nreturn }\n";
  let unsupported name reason expression =
    c_semantic_message
      ("c-import-unsupported-" ^ name)
      (Printf.sprintf "C declaration `%s` is not supported: %s" name reason)
      c_matrix
      ("fn probe() i32 {\n" ^ expression ^ "\nreturn 0 }\n")
  in
  unsupported "fas_float_value" "floating-point types are not supported"
    "fas_float_value(1)";
  unsupported "fas_double_value" "floating-point types are not supported"
    "fas_double_value(1)";
  unsupported "fas_long_double_value" "floating-point types are not supported"
    "fas_long_double_value(1)";
  unsupported "fas_int128_value" "`__int128` has no Fas type" "fas_int128_value(1)";
  unsupported "fas_uint128_value" "`__int128` has no Fas type" "fas_uint128_value(1)";
  unsupported "fas_bitint_value" "`_BitInt` has no Fas type" "fas_bitint_value(1)";
  unsupported "fas_struct_by_value" "struct and union values are not supported"
    "fas_struct_by_value(null)";
  unsupported "fas_union_by_value" "struct and union values are not supported"
    "fas_union_by_value(null)";
  unsupported "fas_bitfield_global" "bit-fields are not supported"
    "fas_bitfield_global = null";
  c_semantic_accept "c-import-array-global" c_matrix
    "fn probe() i32 { fas_array_global[3] = 9; return fas_array_global[0] }\n";
  c_semantic_accept "c-import-array-nested" c_matrix
    "fn probe() i8 { fas_array_names[3][7] = 65; return fas_array_names[0][0] }\n";
  c_semantic_accept "c-import-array-parameter-adjustment" c_matrix
    "fn probe() i32 { return fas_array_parameter(&fas_array_global) }\n";
  c_semantic_accept "c-import-array-mutable-pointer-elements" c_matrix
    "fn probe() void { fas_array_pointer_elements[0] = null }\n";
  c_semantic_accept "c-import-record-array-global" c_matrix
    "fn probe() i32 { return fas_direct_record_array[0].field }\n";
  c_semantic_accept "c-import-typedef-record-array-global" c_matrix
    "fn probe() i32 { return fas_alias_record_array[0].field }\n";
  unsupported "fas_array_unknown" "arrays of unknown size are not supported"
    "fas_array_unknown[0]";
  unsupported "fas_array_float" "floating-point types are not supported"
    "fas_array_float[0]";
  List.iter
    (fun name ->
      c_semantic_message
        ("c-import-array-readonly-" ^ name)
        "cannot modify read-only pointer" c_matrix
        ("fn probe() void { " ^ name ^ "[0] = null }\n"))
    [ "fas_array_readonly_pointer_elements" ];
  List.iter
    (fun name ->
      c_semantic_message
        ("c-import-array-readonly-" ^ name)
        "cannot modify read-only pointer" c_matrix
        ("fn probe() void { " ^ name ^ "[0] = 1 }\n"))
    [ "fas_array_readonly"; "fas_array_readonly_alias" ];
  c_semantic_message "c-import-array-bounds" "array index is out of bounds" c_matrix
    "fn probe() i32 { return fas_array_global[4] }\n";
  c_semantic_message "c-import-array-nested-bounds" "array index is out of bounds"
    c_matrix "fn probe() i8 { return fas_array_names[0][8] }\n";
  c_semantic_message "c-import-array-readonly-view" "cannot modify read-only pointer"
    c_matrix "fn probe() void { view values = fas_array_readonly; values[0] = 1 }\n";
  c_semantic_message "c-import-array-readonly-copy" "cannot modify read-only pointer"
    c_matrix "fn probe() void { copy(fas_array_readonly, fas_array_global) }\n";
  c_semantic_accept "c-import-array-readonly-pointer-target" c_matrix
    "fn probe() void { fas_array_readonly_pointer_elements[0][i32] = 1 }\n";
  unsupported "fas_function_pointer" "function pointers are not supported"
    "fas_function_pointer(1)";
  unsupported "fas_function_pointer_arg" "function pointers are not supported"
    "fas_function_pointer_arg(null)";
  unsupported "fas_function_pointer_nested" "function pointers are not supported"
    "fas_function_pointer_nested(null)";
  unsupported "fas_vector_value" "vector types are not supported by value"
    "fas_vector_value(null)";
  unsupported "fas_address_space" "C address spaces are not supported"
    "fas_address_space(null)";
  unsupported "fas_nondefault_abi" "non-default calling conventions are not supported"
    "fas_nondefault_abi(1)";
  c_semantic_accept "c-import-static-inline" c_matrix
    "fn static_inline_call() i32 { return fas_static_inline(4) }\n";
  if
    not
      (List.exists
         (fun (static : C_import.static_function) ->
           static.name = "fas_static_inline" && not static.variadic)
         c_matrix_imported.static_functions)
  then failwith "static inline function was not recorded for adapter generation";
  c_semantic_accept "c-import-static-inline-handle" c_matrix
    "fn static_rect_call(rect handle[FasRect]) i32 { return fas_rect_empty(rect) }\n";
  let rect_adapter =
    match
      List.find_opt
        (fun (static : C_import.static_function) -> static.name = "fas_rect_empty")
        c_matrix_imported.static_functions
    with
    | Some static -> (
        match C_import.make_adapter ~occupied:[] c_matrix_source static with
        | Ok adapter -> adapter
        | Error message -> failwith message)
    | None -> failwith "static inline handle function was not recorded"
  in
  if
    not
      (contains rect_adapter.code "const struct FasRect * fas_arg0"
      && contains rect_adapter.code "return fas_rect_empty(fas_arg0);")
  then failwith "static inline handle adapter did not preserve its C signature";
  incr checks_run;
  if not (String.starts_with ~prefix:"__fas_c_adapter_" rect_adapter.symbol) then
    failwith "static adapter symbol omitted its fixed prefix";
  incr checks_run;
  (match
     List.find_opt
       (fun (static : C_import.static_function) -> static.name = "fas_rect_empty")
       c_matrix_imported.static_functions
   with
  | Some static -> (
      match
        C_import.make_adapter ~occupied:[ rect_adapter.symbol ] c_matrix_source static
      with
      | Ok adapter when adapter.symbol <> rect_adapter.symbol -> ()
      | Ok _ -> failwith "static adapter symbol collision was not avoided"
      | Error message -> failwith message)
  | None -> failwith "static inline handle function was not recorded");
  unsupported "fas_static_float" "floating-point types are not supported"
    "fas_static_float(1)";
  let static_collision_header = Filename.temp_file "fas-static-collision-" ".h" in
  Fun.protect
    ~finally:(fun () -> Sys.remove static_collision_header)
    (fun () ->
      let channel = open_out_bin static_collision_header in
      output_string channel
        "static int fas_shared_static(int value) { return value; }\n";
      close_out channel;
      let import_static source =
        let span =
          Span.make ~file:source ~start_offset:0 ~end_offset:0 ~line:1 ~column:1
        in
        let declarations, _, _ =
          expect_ok
            (C_import.import ~cc:"clang-22" ~debug:false ~keep:false source
               [ C_import.{ spelling = Ast.C_quoted static_collision_header; span } ])
        in
        C_import.map_declarations ~span declarations
      in
      let static_left =
        Filename.concat (Filename.dirname static_collision_header) "left.fas"
      and static_right =
        Filename.concat (Filename.dirname static_collision_header) "right.fas"
      in
      let collided =
        C_import.merge_imports [ import_static static_left; import_static static_right ]
      in
      c_semantic_message "c-import-static-name-collision"
        "C declaration `fas_shared_static` is not supported: conflicting C declarations"
        (static_left, collided) "fn call_static() i32 { return fas_shared_static(1) }\n");
  c_semantic_accept "c-import-anonymous-record-typedef-by-value" c_matrix
    "fn anonymous() i32 { value FasAnonymous = (FasAnonymous){ 1 }\n\
     return value.field }\n";
  let phase21_records = c_import_fixture "phase21_records.h" in
  let phase21_imported = snd phase21_records in
  let imported_struct name =
    List.find_map
      (function
        | Ast.Struct { name = found; fields; align; _ } when found = name ->
            Some (fields, align)
        | _ -> None)
      phase21_imported.items
  in
  let require_struct name =
    match imported_struct name with
    | Some result -> result
    | None -> failwith ("missing admitted C record " ^ name)
  in
  let tag_fields, _ = require_struct "FasTagRecord" in
  if
    List.map (fun (field : Ast.field) -> (field.name, field.ty)) tag_fields
    <> [ ("first", Ast.Int Ast.I32); ("second", Ast.Int Ast.U8) ]
  then failwith "C tag record fields were not imported in order";
  let const_pointer_fields, _ = require_struct "FasConstPointerRecord" in
  if
    List.map (fun (field : Ast.field) -> (field.name, field.ty)) const_pointer_fields
    <> [ ("value", Ast.Addr) ]
  then failwith "const pointee field was not admitted";
  let anonymous_fields, _ = require_struct "FasAnonymousRecord" in
  if
    List.map (fun (field : Ast.field) -> (field.name, field.ty)) anonymous_fields
    <> [ ("byte", Ast.Int Ast.U8); ("word", Ast.Int Ast.I32) ]
  then failwith "anonymous C typedef record fields were not imported";
  let alias_fields, _ = require_struct "FasAliasRecord" in
  if
    List.length alias_fields <> 1
    || List.length
         (List.filter
            (function Ast.Struct { name = "FasAliasRecord"; _ } -> true | _ -> false)
            phase21_imported.items)
       <> 1
    || List.assoc_opt "FasAlias" phase21_imported.aliases
       <> Some (Ast.Named_type "FasAliasRecord")
  then failwith "tag and typedef did not preserve one C record identity";
  let nested_fields, _ = require_struct "FasNestedRecord" in
  if
    List.map (fun (field : Ast.field) -> (field.name, field.ty)) nested_fields
    <> [
         ("inner", Ast.Named_type "FasInnerRecord");
         ("values", Ast.Array ("2", Ast.Int Ast.I32));
       ]
  then failwith "nested record or array field type was not imported";
  let self_fields, _ = require_struct "FasSelfRecord" in
  if
    List.map (fun (field : Ast.field) -> (field.name, field.ty)) self_fields
    <> [
         ("next", Ast.Handle (Ast.Named_type "FasSelfRecord"));
         ("value", Ast.Int Ast.I32);
       ]
  then failwith "self-referential record pointer did not map to a handle";
  let _, aligned = require_struct "FasAlignedRecord" in
  if aligned <> Some 16 then failwith "aligned C record did not map to @align(16)";
  List.iter
    (fun (name, reason) ->
      if
        imported_struct name <> None
        || (not (List.mem_assoc name phase21_imported.unsupported))
        || List.assoc name phase21_imported.unsupported <> reason
      then failwith ("unsupported C record reason was not retained for " ^ name))
    [
      ("FasPackedRecord", "record layout differs from C");
      ("FasUnionRecord", "unions are not supported");
      ("FasBitfieldRecord", "bit-fields are not supported");
      ("FasFlexibleRecord", "flexible array members are not supported");
      ("FasAnonymousMemberRecord", "anonymous members are not supported");
      ("FasFloatRecord", "floating-point fields are not supported");
      ("FasFunctionPointerRecord", "function-pointer fields are not supported");
      ("FasKeywordFieldRecord", "field name is a Fas keyword");
      ("FasConstFieldRecord", "const fields are not supported");
      ("FasNestedConstFieldRecord", "const fields are not supported");
    ];
  (match c_semantic_result phase21_records "fn noop() void { return }\n" with
  | Ok program ->
      let layout name =
        match
          List.find_opt
            (fun (record : Hir.struct_def) -> record.name = name)
            program.structs
        with
        | Some record -> record
        | None -> failwith ("missing checked record layout " ^ name)
      in
      List.iter
        (fun (name, size, align, offsets) ->
          let record = layout name in
          if
            record.size <> size || record.align <> align
            || List.map
                 (fun (field : Hir.field) -> (field.name, field.offset))
                 record.fields
               <> offsets
          then failwith ("unexpected imported record layout for " ^ name))
        [
          ("FasTagRecord", 8, 4, [ ("first", 0); ("second", 4) ]);
          ("FasAnonymousRecord", 8, 4, [ ("byte", 0); ("word", 4) ]);
          ("FasNestedRecord", 16, 4, [ ("inner", 0); ("values", 8) ]);
          ("FasSelfRecord", 16, 8, [ ("next", 0); ("value", 8) ]);
          ("FasAlignedRecord", 16, 16, [ ("value", 0) ]);
        ]
  | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics));
  let phase21_manifest = C_import.manifest_text phase21_imported in
  if
    not
      (contains phase21_manifest
         "FasNestedRecord\tstruct FasNestedRecord\tstruct FasNestedRecord {inner \
          FasInnerRecord, values arr[2, i32]}")
  then failwith "C record manifest did not list admitted fields";
  c_semantic_accept "c-import-record-handle-and-field-access" phase21_records
    "fn read(value handle[FasSelfRecord]) i32 {\n\
     return handle_addr(value)[FasSelfRecord].value }\n\
     fn cast(pointer addr) handle[FasTagRecord] {\n\
     return handle_from_addr[FasTagRecord](pointer) }\n";
  c_semantic_accept "c-import-record-fas-field-names" phase21_records
    "fn fields(value handle[FasFieldNamesRecord]) i32 {\n\
     raw addr = handle_addr(value)\n\
     raw[FasFieldNamesRecord].handle = 1\n\
     raw[FasFieldNamesRecord].len = 2\n\
     raw[FasFieldNamesRecord].view = 3\n\
     raw[FasFieldNamesRecord].i32 = 4\n\
     return raw[FasFieldNamesRecord].handle + raw[FasFieldNamesRecord].len + \
     raw[FasFieldNamesRecord].view + raw[FasFieldNamesRecord].i32 }\n";
  c_semantic_accept "address-constants-imported-record-handles" phase21_records
    "var Direct handle[FasSelfRecord] = &fas_address_self\n\
     const Nested arr[1,arr[1,handle[FasInnerRecord]]] = {{&fas_address_nested.inner}}\n\
     var Indexed arr[1,handle[FasSelfRecord]] = {&fas_address_self_array[1]}\n";
  c_semantic_message "address-constants-different-imported-record"
    "constant initializer type mismatch" phase21_records
    "var P handle[FasTagRecord] = &fas_address_self\n";
  c_semantic_message "address-constants-native-record-handle"
    "constant initializer type mismatch" phase21_records
    "struct NativeAddressRecord { value i32 }\n\
     var G NativeAddressRecord = {1}\n\
     var P handle[FasSelfRecord] = &G\n";
  c_semantic_message "address-constants-scalar-record-handle"
    "constant initializer type mismatch" phase21_records
    "var G i32 = 1\nvar P handle[FasSelfRecord] = &G\n";
  let imported_record_addresses =
    match
      c_semantic_result phase21_records
        "var Direct handle[FasSelfRecord] = &fas_address_self\n\
         const Nested arr[1,arr[1,handle[FasInnerRecord]]] = \
         {{&fas_address_nested.inner}}\n\
         var Indexed arr[1,handle[FasSelfRecord]] = {&fas_address_self_array[1]}\n"
    with
    | Ok program -> Ir.render (expect_ok (Lower.lower program))
    | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  in
  if
    (not (contains imported_record_addresses "@fas_address_self"))
    || (not (contains imported_record_addresses "@fas_address_nested"))
    || (not (contains imported_record_addresses "@fas_address_self_array"))
    || (not
          (contains imported_record_addresses
             "getelementptr (i8, ptr @fas_address_self_array, i64 16)"))
    || contains imported_record_addresses "inbounds"
  then failwith "address-constants-imported-record-handles: invalid relocation";
  c_semantic_accept "c-import-unsupported-record-remains-handle" phase21_records
    "fn retain(value handle[FasUnionRecord]) handle[FasUnionRecord] { return value }\n";
  c_semantic_message "c-import-packed-record-layout"
    "C declaration `FasPackedRecord` is not supported: record layout differs from C"
    phase21_records "fn read(value FasPackedRecord) i32 { return value.word }\n";
  c_semantic_message "c-import-union-record-reason"
    "C declaration `FasUnionRecord` is not supported: unions are not supported"
    phase21_records "fn read(value FasUnionRecord) i32 { return value.value }\n";
  c_semantic_message "c-import-bitfield-record-reason"
    "C declaration `FasBitfieldRecord` is not supported: bit-fields are not supported"
    phase21_records "fn read(value FasBitfieldRecord) i32 { return value.value }\n";
  c_semantic_message "c-import-flexible-record-reason"
    "C declaration `FasFlexibleRecord` is not supported: flexible array members are \
     not supported"
    phase21_records "fn read(value FasFlexibleRecord) i32 { return value.length }\n";
  c_semantic_message "c-import-anonymous-member-record-reason"
    "C declaration `FasAnonymousMemberRecord` is not supported: anonymous members are \
     not supported"
    phase21_records
    "fn read(value FasAnonymousMemberRecord) i32 { return value.integer }\n";
  c_semantic_message "c-import-float-field-record-reason"
    "C declaration `FasFloatRecord` is not supported: floating-point fields are not \
     supported"
    phase21_records "fn read(value FasFloatRecord) i32 { return value.value }\n";
  c_semantic_message "c-import-function-pointer-field-record-reason"
    "C declaration `FasFunctionPointerRecord` is not supported: function-pointer \
     fields are not supported"
    phase21_records
    "fn read(value FasFunctionPointerRecord) i32 { return value.callback }\n";
  c_semantic_accept "c-import-keyword-field-record-remains-handle" phase21_records
    "fn retain(value handle[FasKeywordFieldRecord]) handle[FasKeywordFieldRecord] { \
     return value }\n";
  c_semantic_accept "c-import-const-field-record-remains-handle" phase21_records
    "fn retain(value handle[FasConstFieldRecord]) handle[FasConstFieldRecord] { return \
     value }\n\
     fn retain_nested(value handle[FasNestedConstFieldRecord]) \
     handle[FasNestedConstFieldRecord] { return value }\n";
  c_semantic_message "c-import-const-field-record-reason"
    "C declaration `FasConstFieldRecord` is not supported: const fields are not \
     supported"
    phase21_records "fn read(value FasConstFieldRecord) i32 { return value.value }\n";
  c_semantic_message "c-import-nested-const-field-record-reason"
    "C declaration `FasNestedConstFieldRecord` is not supported: const fields are not \
     supported"
    phase21_records
    "fn read(value FasNestedConstFieldRecord) i32 { return value.inner.value }\n";
  semantic_error "addr-handle-c-record-native-still-rejected"
    "handle type argument must be an opaque type"
    "struct NativeRecord { value i32 }\n\
     fn f(pointer addr) handle[NativeRecord] {\n\
     return handle_from_addr[NativeRecord](pointer) }\n";
  let phase21_definition_header = Filename.temp_file "fas-phase21-definition-" ".h" in
  let phase21_definition_source =
    Filename.concat
      (Filename.dirname phase21_definition_header)
      "phase21-definition.fas"
  in
  Fun.protect
    ~finally:(fun () -> Sys.remove phase21_definition_header)
    (fun () ->
      let output = open_out_bin phase21_definition_header in
      output_string output
        "extern int fas_phase21_imported_global;\n\
         int fas_phase21_imported_function(int value);\n\
         extern int fas_phase21_incomplete[];\n";
      close_out output;
      let imported =
        let span =
          Span.make ~file:phase21_definition_source ~start_offset:0 ~end_offset:0
            ~line:1 ~column:1
        in
        let declarations, _, _ =
          expect_ok
            (C_import.import ~cc:"clang-22" ~debug:false ~keep:false
               phase21_definition_source
               [ C_import.{ spelling = Ast.C_quoted phase21_definition_header; span } ])
        in
        C_import.map_declarations ~span declarations
      in
      let definitions =
        parse_file phase21_definition_source
          "extern \"C\" { var fas_phase21_imported_global i32 = 7\n\
           fn fas_phase21_imported_function(value i32) i32 { return value }\n\
           var fas_phase21_incomplete arr[3,i32] = {1,2,3} }\n"
      in
      let reconciled =
        expect_ok (C_import.reconcile_source definitions.items imported)
      in
      ignore
        (expect_ok (Sema.check { Ast.items = definitions.items @ reconciled.items }));
      if
        List.exists
          (function
            | Ast.Global { name; _ } | Ast.Func { name; _ } ->
                List.mem name
                  [ "fas_phase21_imported_global"; "fas_phase21_imported_function" ]
            | _ -> false)
          reconciled.items
        || List.mem_assoc "fas_phase21_incomplete" reconciled.unsupported
      then failwith "matching C definitions retained conflicting imports";
      let mismatching =
        parse_file phase21_definition_source
          "extern \"C\" { fn fas_phase21_imported_function(value u32) i32 {return 0 } }\n"
      in
      (match C_import.reconcile_source mismatching.items imported with
      | Error [ diagnostic ]
        when diagnostic.message
             = "C declaration `fas_phase21_imported_function` has type `fn(i32)->i32`, \
                but Fas declares `fn(u32)->i32`" ->
          ()
      | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
      | Ok _ -> failwith "mismatching imported function definition was accepted");
      let wrong_element =
        parse_file phase21_definition_source
          "extern \"C\" { var fas_phase21_incomplete arr[3,u32] = {1,2,3} }\n"
      in
      match C_import.reconcile_source wrong_element.items imported with
      | Error [ diagnostic ]
        when diagnostic.message
             = "C declaration `fas_phase21_incomplete` has type `arr[?, i32]`, but Fas \
                declares `arr[?, u32]`" ->
          ()
      | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
      | Ok _ -> failwith "incomplete array with the wrong element type was accepted");
  c_semantic_message "c-import-reserved-name"
    "C declaration `addr` is not supported: name is reserved in Fas" c_matrix
    "fn probe() i32 { return addr(1) }\n";
  c_semantic_message "c-import-macro-is-foreign-only" "unknown name `FAS_MACRO_ONLY`"
    c_matrix "fn probe() i32 { return FAS_MACRO_ONLY }\n";
  List.iter
    (fun (name, output, file, line, column, notes) ->
      incr checks_run;
      let fallback =
        Span.make ~file:"import.fas" ~start_offset:0 ~end_offset:0 ~line:7 ~column:1
      in
      let diagnostic = C_import.compilation_error fallback output in
      if
        diagnostic.message <> "C compilation failed: expected expression"
        || diagnostic.primary.file <> file
        || diagnostic.primary.line <> line
        || diagnostic.primary.column <> column
        || diagnostic.notes <> notes
      then failwith (name ^ ": " ^ Diag.render_all ~source:None [ diagnostic ]))
    [
      ( "c-diagnostic-fas",
        "m.fas:2:12: error: expected expression\n",
        "m.fas",
        2,
        12,
        [] );
      ( "c-diagnostic-fatal",
        "header.h:3:9: fatal error: expected expression\n",
        "import.fas",
        7,
        1,
        [ "header.h:3:9" ] );
      ( "c-diagnostic-header",
        "header.h:3:9: error: expected expression\n",
        "import.fas",
        7,
        1,
        [ "header.h:3:9" ] );
    ];
  semantic_accept "static-named-sizes"
    "const N usize = 3\n\
     var V arr[N,i32] = {1,2,3}\n\
     const T arr[N,i32] = {1,2,3}\n\
     struct P { d arr[N,u8] }\n\
     const S arr[3,u8] = {1,2,3}\n\
     const K usize = len(S)\n\
     const L usize = len(c\"abc\")\n\
     var W arr[L,u8]\n\
     var X arr[K,u8]\n\
     var Q vec[N,u8]\n";
  semantic_accept "static-forward-named-sizes"
    "var V arr[N,i32]\nstruct P { d arr[N,u8] }\nconst N usize = 3\n";
  semantic_message "static-size-self-cycle" "cyclic constant dependency involving `N`"
    "const N usize = len(A)\nconst A arr[N,u8] = {1}\n";
  semantic_message "static-size-mutual-cycle" "cyclic constant dependency involving `N`"
    "const N usize = len(B)\n\
     const M usize = len(A)\n\
     const A arr[N,u8] = {1}\n\
     const B arr[M,u8] = {2}\n";
  parse_message "static-size-direct-len-cycle" "expected `,`, found `(`"
    "const A arr[len(B),u8] = {1}\nconst B arr[len(A),u8] = {2}\n";
  Printf.printf "regression checks: %d passed\n" !checks_run

let () =
  let hir =
    expect_ok
      (Sema.check
         (parse_file "exports.fas"
            "opaque Token\n\
             struct Inner { x u16 }\n\
             struct Outer @align(16) { inner Inner, lanes vec[3,u8], mask vec[8,bool], \
             ptr handle[Token], data arr[2,arr[3,i32]] }\n\
             extern \"C\" {\n\
             var state Outer = {{2},splat(1),splat(true),null,{{1,2,3},{4,5,6}}}\n\
             var a i8 = 1\n\
             fn z(p addr, t handle[Token], n usize, s isize, b bool) i32 { return 2 }\n\
             fn imported() i32\n\
             }\n\
             fn private() i32 { return 1 }\n"))
  in
  let actual, headers, errors = C_exports.declarations [] hir in
  let expected =
    "struct Inner {\n\
    \  alignas(2) uint16_t x;\n\
     };\n\
     static_assert(sizeof(struct Inner) == 2, \"struct Inner size\");\n\
     static_assert(alignof(struct Inner) == 2, \"struct Inner alignment\");\n\
     static_assert(offsetof(struct Inner, x) == 0, \"Inner.x offset\");\n\
     typedef uint8_t fas_vec_3_u8 __attribute__((ext_vector_type(3)));\n\
     static_assert(sizeof(fas_vec_3_u8) == 4, \"fas_vec_3_u8 size\");\n\
     static_assert(alignof(fas_vec_3_u8) == 4, \"fas_vec_3_u8 alignment\");\n\
     typedef bool fas_vec_8_bool __attribute__((ext_vector_type(8)));\n\
     static_assert(sizeof(fas_vec_8_bool) == 1, \"fas_vec_8_bool size\");\n\
     static_assert(alignof(fas_vec_8_bool) == 1, \"fas_vec_8_bool alignment\");\n\
     struct Token;\n\
     struct Outer {\n\
    \  alignas(16) struct Inner inner;\n\
    \  fas_vec_3_u8 lanes;\n\
    \  fas_vec_8_bool mask;\n\
    \  struct Token * ptr;\n\
    \  int32_t data[2][3];\n\
     };\n\
     static_assert(sizeof(struct Outer) == 48, \"struct Outer size\");\n\
     static_assert(alignof(struct Outer) == 16, \"struct Outer alignment\");\n\
     static_assert(offsetof(struct Outer, inner) == 0, \"Outer.inner offset\");\n\
     static_assert(offsetof(struct Outer, lanes) == 4, \"Outer.lanes offset\");\n\
     static_assert(offsetof(struct Outer, mask) == 8, \"Outer.mask offset\");\n\
     static_assert(offsetof(struct Outer, ptr) == 16, \"Outer.ptr offset\");\n\
     static_assert(offsetof(struct Outer, data) == 24, \"Outer.data offset\");\n\
     extern int8_t a;\n\
     extern struct Outer state;\n\
     int32_t z(void * p, struct Token * t, size_t n, ptrdiff_t s, bool b);\n"
  in
  if actual <> expected then failwith ("c-export-matrix:\n" ^ actual);
  assert (headers = [] && errors = []);
  print_endline "C export declaration matrix: passed"

let () =
  let generate text =
    C_exports.declarations [] (expect_ok (Sema.check (parse_file "exports.fas" text)))
  in
  let widths =
    [
      ("i8", "int8_t");
      ("u8", "uint8_t");
      ("i16", "int16_t");
      ("u16", "uint16_t");
      ("i32", "int32_t");
      ("u32", "uint32_t");
      ("i64", "int64_t");
      ("u64", "uint64_t");
      ("isize", "ptrdiff_t");
      ("usize", "size_t");
      ("bool", "bool");
      ("addr", "void *");
    ]
  in
  List.iter
    (fun (fas, c) ->
      incr checks_run;
      let value =
        if fas = "bool" then "true" else if fas = "addr" then "null" else "1"
      in
      let text, _, errors =
        generate
          (Printf.sprintf
             "extern \"C\" {\nvar value %s = %s\nfn echo(x %s) %s { return x }\n}\n" fas
             value fas fas)
      in
      assert (errors = []);
      assert (text = Printf.sprintf "%s echo(%s x);\nextern %s value;\n" c c c))
    widths;
  let text, _, errors = generate "extern \"C\" { fn done() void {} }\n" in
  assert (text = "void done(void);\n" && errors = []);
  List.iter
    (fun (source, expected) ->
      incr checks_run;
      let text, _, errors = generate source in
      assert (text = "extern int32_t good;\n" && errors = [ expected ]))
    [
      ( "struct Empty {}\n\
         extern \"C\" {\n\
         var bad arr[2,Empty] = {{},{}}\n\
         var good i32 = 1\n\
         }\n",
        "export `bad` has no C declaration: arr[2, Empty] contains an empty struct" );
      ( "struct Zero { x arr[0,u8] }\n\
         extern \"C\" {\n\
         var bad Zero = {{}}\n\
         var good i32 = 1\n\
         }\n",
        "export `bad` has no C declaration: Zero contains a zero-length array" );
    ];
  let _, _, errors =
    generate
      "struct Empty {}\nextern \"C\" {\nvar z Empty = {}\nvar a arr[0,u8] = {}\n}\n"
  in
  assert (
    List.hd errors
    = "export `a` has no C declaration: arr[0, u8] contains a zero-length array");
  let text, _, errors =
    generate
      "extern \"C\" {\nvar fas_vec_3_u8 i32 = 5\nvar lanes vec[3,u8] = splat(1)\n}\n"
  in
  assert (
    errors = []
    && contains text
         "typedef uint8_t fas_vec_3_u8_ __attribute__((ext_vector_type(3)));\n"
    && contains text "extern fas_vec_3_u8_ lanes;\n");
  let header =
    C_exports.header ~name:"my-api.h" ~headers:[ "#include <api.h>\n" ]
      "void done(void);\n"
  in
  assert (
    header
    = "#ifndef FAS_MY_API_H_H\n\
       #define FAS_MY_API_H_H\n\
       #include <api.h>\n\
       #ifdef __cplusplus\n\
       extern \"C\" {\n\
       #endif\n\
       void done(void);\n\
       #ifdef __cplusplus\n\
       }\n\
       #endif\n\
       #endif\n");
  let plain_header =
    C_exports.header ~name:"plain.h" ~headers:[] "void print(void * text);\n"
  in
  assert (
    plain_header
    = "#ifndef FAS_PLAIN_H_H\n\
       #define FAS_PLAIN_H_H\n\
       #ifdef __cplusplus\n\
       extern \"C\" {\n\
       #endif\n\
       void print(void * text);\n\
       #ifdef __cplusplus\n\
       }\n\
       #endif\n\
       #endif\n");
  let scalar_declarations, _, errors =
    generate
      "extern \"C\" {\n\
       fn echo(x i32) i32 { return x }\n\
       var flag bool = true\n\
       var count usize = 1\n\
       }\n"
  in
  assert (errors = []);
  let scalar_header =
    C_exports.header ~name:"scalars.h" ~headers:[] scalar_declarations
  in
  assert (
    scalar_header
    = "#ifndef FAS_SCALARS_H_H\n\
       #define FAS_SCALARS_H_H\n\
       #include <stdbool.h>\n\
       #include <stdint.h>\n\
       #include <stddef.h>\n\
       #ifdef __cplusplus\n\
       extern \"C\" {\n\
       #endif\n\
       extern size_t count;\n\
       int32_t echo(int32_t x);\n\
       extern bool flag;\n\
       #ifdef __cplusplus\n\
       }\n\
       #endif\n\
       #endif\n");
  let struct_declarations, _, errors =
    generate "struct Pair { x i32\n y i32 }\nextern \"C\" { var pair Pair = {1, 2} }\n"
  in
  assert (errors = []);
  let struct_header = C_exports.header ~name:"pair.h" ~headers:[] struct_declarations in
  assert (
    struct_header
    = String.concat "\n"
        [
          "#ifndef FAS_PAIR_H_H";
          "#define FAS_PAIR_H_H";
          "#include <stdalign.h>";
          "#include <assert.h>";
          "#include <stdint.h>";
          "#include <stddef.h>";
          "#ifdef __cplusplus";
          "extern \"C\" {";
          "#endif";
          "struct Pair {";
          "  alignas(4) int32_t x;";
          "  int32_t y;";
          "};";
          "static_assert(sizeof(struct Pair) == 8, \"struct Pair size\");";
          "static_assert(alignof(struct Pair) == 4, \"struct Pair alignment\");";
          "static_assert(offsetof(struct Pair, x) == 0, \"Pair.x offset\");";
          "static_assert(offsetof(struct Pair, y) == 4, \"Pair.y offset\");";
          "extern struct Pair pair;";
          "#ifdef __cplusplus";
          "}";
          "#endif";
          "#endif";
        ]
      ^ "\n");
  let records =
    [
      C_exports.{ name = "Tag"; spelling = "struct Tag"; header = None };
      C_exports.
        { name = "Alias"; spelling = "Alias"; header = Some "#include <alias.h>\n" };
    ]
  in
  let hir =
    expect_ok
      (Sema.check
         (parse_file "handles.fas"
            "opaque Tag\n\
             opaque Alias\n\
             extern \"C\" { fn get(x handle[Tag], y handle[Alias]) handle[Alias] { \
             return y } }\n"))
  in
  let text, headers, errors = C_exports.declarations records hir in
  assert (
    text = "struct Tag;\nAlias * get(struct Tag * x, Alias * y);\n"
    && headers = [ "#include <alias.h>\n" ]
    && errors = []);
  let root = Filename.temp_file "fas-export-pins-" ".fas" in
  let h = root ^ ".h" and out = root ^ ".api.h" in
  let dir = root ^ ".dir" in
  Unix.mkdir dir 0o700;
  let angle = Filename.concat dir "types.h" in
  let inner = Filename.concat dir "inner.h" in
  let write path text =
    let ch = open_out_bin path in
    output_string ch text;
    close_out ch
  in
  let read path =
    let ch = open_in_bin path in
    let text = really_input_string ch (in_channel_length ch) in
    close_in ch;
    text
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun p -> try Sys.remove p with Sys_error _ -> ())
        [ root; h; out; angle; inner ];
      Unix.rmdir dir)
    (fun () ->
      List.iter
        (fun source ->
          write root source;
          match Driver.run (cli_run [ "--emit-header"; root ]) with
          | Error [ d ] ->
              assert (
                d.Diag.message
                = "export `bad` has no C declaration: Empty contains an empty struct")
          | _ -> failwith "c-export-empty-header: expected exact rejection")
        [ "struct Empty {}\nextern \"C\" { var bad Empty = {} }\n" ];
      write root "extern \"C\" { var bad arr[0,u8] = {} }\n";
      (match Driver.run (cli_run [ "--emit-header"; root ]) with
      | Error [ d ] ->
          assert (
            d.Diag.message
            = "export `bad` has no C declaration: arr[0, u8] contains a zero-length \
               array")
      | _ -> failwith "c-export-zero-header: expected exact rejection");
      write root
        "use \"C\" <<C\n\
         int exported(int x);\n\
         int invoke(void) { return exported(2); }\n\
         C\n\
         extern \"C\" { fn exported(x i32) i32 { return x } }\n";
      ignore (expect_ok (Driver.run (cli_run [ "--emit-header"; root ])));
      write root
        "use \"C\" <<C\n\
         long exported(int x);\n\
         C\n\
         extern \"C\" { fn exported(x i32) i32 { return x } }\n";
      (match Driver.run (cli_run [ "--emit-header"; root ]) with
      | Error [ d ] ->
          assert (
            d.Diag.message
            = "C declaration `exported` has type `fn(i32)->i64`, but Fas declares \
               `fn(i32)->i32`"
            && d.primary.line = 4 && d.primary.file = root)
      | _ -> failwith "c-export-incompatible-prototype: expected signature rejection");
      write root
        "use \"C\" <<C\n\
         void *missing(void) { return &bad; }\n\
         C\n\
         extern \"C\" { var bad arr[0,u8] = {} }\n";
      (match Driver.run (cli_run [ "--emit-ir"; root ]) with
      | Error [ d ] ->
          assert (
            d.Diag.message = "C compilation failed: use of undeclared identifier 'bad'"
            && d.primary.line = 2)
      | _ -> failwith "c-export-omitted-container: expected mapped Clang rejection");
      List.iter
        (fun (source, expected) ->
          write root ("use \"C\" <<C\nint sentinel(void) { return 1; }\nC\n" ^ source);
          ignore (expect_ok (Driver.run (cli_run [ "--emit-ir"; root ])));
          match Driver.run (cli_run [ "--emit-header"; root ]) with
          | Error [ d ] -> assert (d.Diag.message = expected)
          | _ -> failwith "c-export-third-run: expected exact rejection")
        [
          ( "use \"C\" <<T\n\
             typedef struct { int x; } Alias;\n\
             T\n\
             extern \"C\" { fn echo(x handle[Alias]) handle[Alias] { return x } }\n",
            "export `echo` has no C declaration: `Alias` is declared only in a C \
             container" );
          ( "extern \"C\" { fn unsigned(new i32) i32 { return new } }\n",
            "export `unsigned` has no C declaration: `unsigned` is a reserved C or C++ \
             identifier" );
          ( "struct S { char u8\nclass i32 }\nextern \"C\" { var value S = {1,2} }\n",
            "export `value` has no C declaration: `char` is a reserved C or C++ \
             identifier" );
          ( "struct Inner { class i32 }\n\
             struct Outer { x arr[1,Inner] }\n\
             extern \"C\" { var value Outer = {{{1}}} }\n",
            "export `value` has no C declaration: `class` is a reserved C or C++ \
             identifier" );
          ( "struct size_t { x i32 }\nextern \"C\" { var value size_t = {1} }\n",
            "export `value` has no C declaration: `size_t` is a reserved C or C++ \
             identifier" );
        ];
      write root
        "extern \"C\" { fn echo(new i32, assert i32) i32 { return new + assert } }\n";
      let header = expect_ok (Driver.run (cli_run [ "--emit-header"; root ])) in
      assert (contains header "int32_t echo(int32_t , int32_t );\n");
      write root
        "use \"C\" <<C\n\
         struct Tag;\n\
         C\n\
         extern \"C\" { fn echo(x handle[Tag]) handle[Tag] { return x } }\n";
      let header = expect_ok (Driver.run (cli_run [ "--emit-header"; root ])) in
      assert (contains header "struct Tag * echo(struct Tag * x);\n");
      write h
        "#ifndef EXPORT_TYPES_H\n\
         #define EXPORT_TYPES_H\n\
         typedef struct { int x; } Alias;\n\
         struct Tag;\n\
         #endif\n";
      let exports =
        "extern \"C\" {\n\
         fn exported(x handle[Alias], y handle[Tag]) handle[Alias] { return x }\n\
         }\n"
      in
      write root (Printf.sprintf "use \"C\" %S\n%s" (Filename.basename h) exports);
      ignore (expect_ok (Driver.run (cli_run [ "--emit-header"; "-o"; out; root ])));
      let header = read out in
      assert (contains header (Printf.sprintf "#include %S\n" (Filename.basename h)));
      assert (
        contains header "struct Tag;\nAlias * exported(Alias * x, struct Tag * y);\n");
      assert (
        String.starts_with
          ~prefix:("#ifndef FAS_" ^ C_exports.guard (Filename.basename out) ^ "_H\n")
          header);
      write angle (read h);
      write root (Printf.sprintf "use \"C\" <%s>\n%s" "types.h" exports);
      let header =
        expect_ok (Driver.run (cli_run [ "--emit-header"; "-I"; dir; root ]))
      in
      assert (contains header ("#include <" ^ "types.h" ^ ">\n"));
      write inner "typedef struct { int x; } NestedAlias;\n";
      write angle "#include \"inner.h\"\n";
      write root
        "use \"C\" <types.h>\n\
         use \"C\" <<C\n\
         NestedAlias *wrapper(NestedAlias *x) { return echo(x); }\n\
         C\n\
         extern \"C\" { fn echo(x handle[NestedAlias]) handle[NestedAlias] { return x \
         } }\n";
      let nested =
        expect_ok (Driver.run (cli_run [ "--emit-header"; "-I"; dir; root ]))
      in
      assert (
        contains nested "#include <types.h>\n"
        && contains nested "NestedAlias * echo(NestedAlias * x);\n");

      write root "struct S { char u8 }\nextern \"C\" { var value S = {1} }\n";
      (match Driver.run (cli_run [ "--emit-header"; root ]) with
      | Error [ d ] ->
          assert (
            d.Diag.message
            = "export `value` has no C declaration: `char` is a reserved C or C++ \
               identifier"
            && d.primary.Span.file = root && d.primary.Span.line = 2
            && d.primary.Span.column = 14
            && d.notes
               = [
                   Printf.sprintf "offending field `char` is declared here: %s:1:12"
                     root;
                 ])
      | _ -> failwith "c-export-field-span: expected exact rejection location");

      assert (
        String.starts_with
          ~prefix:
            ("#ifndef FAS_"
            ^ C_exports.guard (Filename.remove_extension (Filename.basename root))
            ^ "_H\n")
          header));
  semantic_accept "struct-fields-adjacent" "struct V { x i32 y i32 z i32 }\n";
  semantic_accept "struct-fields-comma-separated" "struct V { x i32, y i32 }\n";
  semantic_accept "struct-fields-one-per-line" "struct V {\nx i32\ny i32\n}\n";
  semantic_accept "struct-field-nested-struct-on-one-line"
    "struct V { x i32 }\nstruct B { lo V hi V }\n";
  semantic_accept "struct-field-multiline-array-type"
    "struct S { values arr[\n2,\ni32\n] }\n";
  let split_struct_field = "struct S { a\ni32 }\n" in
  incr checks_run;
  (match Parser.parse (source split_struct_field) with
  | Ok _ -> failwith "struct-field-newline-before-type: expected parse rejection"
  | Error [ diagnostic ] ->
      let span = diagnostic.Diag.primary in
      if
        diagnostic.message <> "expected a type, found newline"
        || span.Span.file <> "regression.fas"
        || span.start_offset <> 12 || span.end_offset <> 13 || span.line <> 1
        || span.column <> 13
      then
        failwith "struct-field-newline-before-type: diagnostic message or span changed"
  | Error _ -> failwith "struct-field-newline-before-type: expected one diagnostic");
  semantic_accept "newline-whitespace-in-delimited-lists"
    "struct Shape {\n\
     value i32\n\
     }\n\
     const values arr[2,i32] = {\n\
     1,\n\
     2\n\
     }\n\
     fn identity[T](\n\
     value T\n\
     ) T { return value }\n\
     fn combine(\n\
     left i32,\n\
     right i32\n\
     ) i32 { return (left +\n\
     right) }\n\
     fn f() i32 {\n\
     items arr[\n\
     2,\n\
     i32\n\
     ] = {\n\
     3,\n\
     4\n\
     }\n\
     return identity[\n\
     i32\n\
     ](combine(\n\
     items[\n\
     0\n\
     ],\n\
     values[\n\
     1\n\
     ]))\n\
     }\n";
  print_endline "C export spelling, omission, diagnostics and header pins: passed"
