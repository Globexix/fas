let source text = Source.create ~file:"regression.fas" ~text

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
  let items =
    files
    |> List.map (fun (file, text) -> (parse_file file text).Ast.items)
    |> List.concat
  in
  Sema.check { Ast.items }

let semantic_diagnostics text =
  let program = expect_ok (Parser.parse (source text)) in
  match Sema.check program with
  | Ok _ -> failwith "expected semantic rejection"
  | Error diagnostics -> diagnostics

let semantic_messages text =
  List.map (fun (diagnostic : Diag.t) -> diagnostic.message) (semantic_diagnostics text)

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

let parse_error_message name fragment text =
  match Parser.parse (source text) with
  | Ok _ -> failwith (name ^ ": expected parse rejection")
  | Error diagnostics ->
      let rendered = Diag.render_all ~source:None diagnostics in
      if not (contains rendered fragment) then
        failwith (name ^ ": unexpected diagnostic: " ^ rendered)

let cli_error name fragment args =
  match Cli.parse (Array.of_list ("fas" :: args)) with
  | Error message when contains message fragment -> ()
  | Error message -> failwith (name ^ ": unexpected diagnostic: " ^ message)
  | Ok _ -> failwith (name ^ ": expected CLI rejection")

let cli_run args =
  match Cli.parse (Array.of_list ("fas" :: args)) with
  | Ok (Cli.Run config) -> config
  | Ok Cli.Help -> failwith "expected compiler invocation"
  | Error message -> failwith message

let lower_of text =
  let program = expect_ok (Parser.parse (source text)) in
  let hir = expect_ok (Sema.check program) in
  expect_ok (Lower.lower hir)

let llvm_of text = Ir.render (lower_of text)

let lower_struct_error name fragment (struct_def : Hir.struct_def) =
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
  semantic_error "integer-vector-bitcast-width" "illegal cast"
    "fn f(value u64) vec[4,u32] { return bitcast[vec[4,u32]](value) }\n";
  semantic_error "integer-vector-bitcast-implicit" "type mismatch"
    "fn f(value vec[4,i32]) vec[4,u32] { return value }\n";
  semantic_error "integer-vector-bitcast-array" "illegal cast"
    "fn f(value arr[2,u32]) vec[2,u32] { return bitcast[vec[2,u32]](value) }\n";
  semantic_error "integer-vector-bitcast-struct" "illegal cast"
    "struct Pair { left u32 right u32 }\n\
    \ fn f(value Pair) vec[2,u32] { return bitcast[vec[2,u32]](value) }\n";
  semantic_error "integer-vector-bitcast-pointer" "illegal cast"
    "fn f(value ptr[u8]) vec[1,u64] { return bitcast[vec[1,u64]](value) }\n";
  semantic_error "integer-vector-bitcast-zext" "illegal cast"
    "fn f(value vec[4,u8]) vec[4,u16] { return zext[vec[4,u16]](value) }\n";
  semantic_error "integer-vector-bitcast-sext" "illegal cast"
    "fn f(value vec[4,i8]) vec[4,i16] { return sext[vec[4,i16]](value) }\n";
  semantic_error "integer-vector-bitcast-trunc" "illegal cast"
    "fn f(value vec[4,u16]) vec[4,u8] { return trunc[vec[4,u8]](value) }\n";
  semantic_error "integer-vector-bitcast-opaque" "illegal cast"
    "opaque Handle\nfn f(value i64) void { bitcast[Handle](value)\n return }\n";
  semantic_error "integer-vector-bitcast-void" "illegal cast"
    "fn f(value i64) void { bitcast[void](value)\n return }\n";
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
  semantic_error "integer-vector-comparison-condition" "if condition must be scalar"
    "fn f(left vec[4,i32], right vec[4,i32]) i32 {\n\
    \ if left == right { return 1 }\n\
    \ return 0\n\
     }\n";
  semantic_error "integer-vector-comparison-no-reduction"
    "logical operands must be scalar"
    "fn f(left vec[4,i32], right vec[4,i32]) vec[4,bool] {\n\
    \ return (left == right) && (left != right)\n\
     }\n";
  let integer_vector_shift_counts =
    llvm_of
      "fn shifts(values vec[4,u32], signed_values vec[4,i32], narrow u8, equal u32, \
       wide u64) vec[4,u32] {\n\
      \ shl_narrow vec[4,u32] = shl(values, narrow)\n\
      \ shl_equal vec[4,u32] = shl(values, equal)\n\
      \ shl_wide vec[4,u32] = shl(values, wide)\n\
      \ lshr_narrow vec[4,u32] = lshr(values, narrow)\n\
      \ lshr_equal vec[4,u32] = lshr(values, equal)\n\
      \ lshr_wide vec[4,u32] = lshr(values, wide)\n\
      \ ashr_narrow vec[4,i32] = ashr(signed_values, narrow)\n\
      \ ashr_equal vec[4,i32] = ashr(signed_values, equal)\n\
      \ ashr_wide vec[4,i32] = ashr(signed_values, wide)\n\
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
        ("integer-vector-shift-count-splat-" ^ operation)
        "vector shifts require a scalar integer count"
        (Printf.sprintf
           "fn f(values vec[4,u32]) vec[4,u32] { return %s(values, splat(1)) }\n"
           operation);
      semantic_error
        ("integer-vector-shift-count-named-" ^ operation)
        "vector shifts require a scalar integer count"
        (Printf.sprintf
           "fn f(values vec[4,u32], count vec[4,u32]) vec[4,u32] { return %s(values, \
            count) }\n"
           operation))
    [ "shl"; "lshr"; "ashr"; "rotl"; "rotr" ];
  semantic_error "integer-vector-shift-count-noninteger" "integer shift"
    "fn f(values vec[4,u32], count bool) vec[4,u32] { return shl(values, count) }\n";
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
  if not (contains usize_specialization "N=usize:3") then
    failwith "usize-specialization: specialization key lost usize identity";
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
      \ v vec[256,i64] = splat(1)\n\
      \ v[N] = 8\n\
      \ return a[N] + v[N] }\n"
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
    "struct S { x i64 y i64 }\nfn f() i64 { s S\n true || &s\n t S = s\n return 0 }\n";
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
     fn f(p i64) i64 { s S\n\
    \ q ptr[S] = p ? &s : null\n\
     t S = s\n\
    \ return 0 }\n";
  semantic_error "place-init-ternary-cross-arm" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn choose(p ptr[S], x S) S { return x }\n\
     fn f(p i64) i64 { s S\n\
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
    "struct S { x i64 }\nfn f(p i64) i64 { s S\nif p { s.x = 1 }\nreturn s.x }\n";
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f(p i64) i64 { s S\n\
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
     fn f(p i64) i64 { s S\n\
     if p { s.x = 1 } else { s.y = 2 }\n\
    \ return s.x }\n";
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f(p i64) i64 { s S\n\
        if p { s.x = 1 } else { return 0 }\n\
       \ return s.x }\n");
  semantic_error "aggregate-loop-only" "use of uninitialized local `s`"
    "struct S { x i64 }\nfn f(p i64) i64 { s S\n while p { s.x = 1 }\n return s.x }\n";
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn take(p ptr[S]) void { return }\n\
        fn f(p i64) i64 { s S\n\
       \ if p { take(&s) } else { s.x = 1 }\n\
        return s.x }\n");
  semantic_error "aggregate-branch-raw-missing-field" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn take(p ptr[S]) void { return }\n\
     fn f(p i64) i64 { s S\n\
    \ if p { take(&s) } else { s.x = 1 }\n\
     return s.y }\n";
  semantic_error "aggregate-branch-raw-whole" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn take(p ptr[S]) void { return }\n\
     fn f(p i64) i64 { s S\n\
    \ if p { take(&s) } else { s.x = 1 }\n\
     t S = s\n\
    \ return 0 }\n";
  semantic_error "aggregate-compound-read" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\nfn f() i64 { s S\n s.x += 1\n return 0 }\n";
  semantic_error "place-init-after-return" "use of uninitialized local `x`"
    "fn f(p i64) i64 { x i64\n if p { return 0\n x = 1 }\n return x }\n";
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
  semantic_error "nested-defer" "nested defer is not allowed"
    "fn f() void { defer { defer { } } }\n";
  semantic_error "defer-return" "return is not allowed inside defer"
    "fn f() void { defer { return } }\n";
  semantic_error "defer-break" "break is not allowed inside defer"
    "fn f() void { while true { defer { break } break } }\n";
  semantic_error "defer-continue" "continue is not allowed inside defer"
    "fn f() void { while true { defer { continue } break } }\n";
  semantic_error "lexical-scope-same-block" "duplicate local `value`"
    "fn f() i64 { value i64 = 1\n value i64 = 2\n return value }\n";
  ignore
    (lower_of
       "fn f() i64 { value i64 = 1\n { value i64 = 2\n value += 1 }\n return value }\n");
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
       \ value i64 = 100\n\
       \ out.* += value - 100\n\
       \ }\n");
  semantic_error "lexical-scope-const-shadow" "shadows a const"
    "const value i64 = 1\nfn f() i64 { value i64 = 2\n return value }\n";
  semantic_error "lexical-scope-const-array-shadow" "shadows a const"
    "const values arr[1,i64] = {1}\n fn f() i64 { values i64 = 2\n return values }\n";
  semantic_error "lexical-scope-declaration-before-use" "unknown assignment target"
    "fn f() i64 { value = 1\n value i64 = 2\n return value }\n";
  semantic_error "void-value-return" "void function cannot return a value"
    "extern \"C\" { fn sink() void }\nfn f() void { return sink() }\n";
  semantic_error "nonvoid-fallthrough" "may reach the end without returning"
    "fn f() i64 { }\n";
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
       asm fn raw() i64 {\n\
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
  if not (contains explicit_assembly_llvm "define i64 @assembly_helper(i64 %x)") then
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
      "fn read(handle ptr[Handle]) i64 { value Pair = (Pair){11}\n return value.x }\n"
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
  semantic_error "pointer-bitcast-discards-const" "illegal cast"
    "fn use(value ptr[const u8]) ptr[u8] { return bitcast[ptr[u8]](value) }\n";
  semantic_error "pointer-bitcast-u32-width" "illegal cast"
    "fn use(value ptr[u8]) u32 { return bitcast[u32](value) }\n";
  semantic_error "pointer-bitcast-i32-width" "illegal cast"
    "fn use(value i32) ptr[u8] { return bitcast[ptr[u8]](value) }\n";
  semantic_error "const-pointer-bitcast-u32-width" "illegal cast"
    "fn use(value ptr[const u8]) u32 { return bitcast[u32](value) }\n";
  semantic_error "integer-bitcast-const-pointer-u32-width" "illegal cast"
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
        fn id[N const usize](value u64) u64 { return value + N }\n");
  ignore
    (lower_of "fn choose(value bool) i64 { if (value){ return 1 } else { return 0 } }\n");
  ignore
    (lower_of
       "fn controls(value i64) i64 {\n\
        while (value){ break }\n\
        switch (value){ case 0: { return 0 } default: { } }\n\
        for ; value; (value){ break }\n\
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
      output_string channel "asm fn raw() i64 {\n retq\n}\nfn main() i64 { return 0 }\n";
      close_out channel;
      let config =
        cli_run [ "-debug"; "--emit-llvm"; "-no-inline"; "raw"; asm_profile_path ]
      in
      match Driver.run config with
      | Error diagnostics ->
          let rendered = Diag.render_all ~source:None diagnostics in
          if not (contains rendered "not a normal definition") then
            failwith "no-inline: raw assembly diagnostic changed"
      | Ok _ -> failwith "no-inline: raw assembly was accepted");
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
  if not (contains byte_literal_semantics "ret i64 6") then
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
    llvm_of "fn main() i64 { return zext[i64](len(\"abc\")) }\n"
  in
  if not (contains raw_literal_length "ret i64 3") then
    failwith "fas-031-len: raw literal length is incorrect";
  let c_literal_length =
    llvm_of "fn main() i64 { return zext[i64](len(c\"abc\")) }\n"
  in
  if not (contains c_literal_length "ret i64 3") then
    failwith "fas-031-len: C literal payload length is incorrect";
  let literal_const_specialization =
    llvm_of
      "fn literal_size[N const usize]() usize { return N }\n\
       fn main() usize { return literal_size[len(\"abc\")]() }\n"
  in
  if not (contains literal_const_specialization "N=usize:3") then
    failwith "fas-031-len: literal length was not accepted as a const argument";
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
                   fn main() u64 { return id[3](2) + id[1 + 2](3) }\n"))))
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
  semantic_error "const-param-bool-rejected" "const parameter type must be an integer"
    "fn id[N const bool](x u64) u64 { return x }\n";

  semantic_error "const-param-pointer-rejected"
    "const parameter type must be an integer"
    "fn id[N const ptr[u8]](x u64) u64 { return x }\n";

  semantic_error "const-param-duplicate" "duplicate generic parameter `N`"
    "fn id[N const usize, N const usize](x u64) u64 { return x + N }\n";

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
  parse_error_message "type-generic-missing-const-type" "expected a type"
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
     if true { return value + N }\n\
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
  ignore
    (expect_ok
       (Sema.check
          (expect_ok
             (Parser.parse
                (source
                   "fn inner[T, M const usize](x T) u64 { return zext[u64](M) }\n\
                    fn outer[N const usize](x i64) u64 { return inner[i64, 3](x) }\n\
                    fn main() u64 { return outer[5](40) }\n")))));
  semantic_error "extern-c-type-parameter"
    "extern \"C\" functions cannot have type parameters"
    "extern \"C\" { fn identity[T](value T) T }\n";
  semantic_error "generic-main" "entry point `main` cannot have generic parameters"
    "fn main[T]() i64 { return 42 }\n";
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
  if not (contains mixed_layout_argument "ret i64 8") then
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

  let const_generic_function_type_source =
    "const THREE usize = 3\n\
     fn array_identity[T, N const usize](value arr[N, T]) arr[N, T] {\n\
    \ copy arr[N, T] = value\n\
    \ return copy\n\
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
  semantic_error "const-generic-function-type-mismatch" "type mismatch"
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
  if not (contains const_array_len_generic_llvm "ret i64 3") then
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

  let const_generic_struct_function_source =
    "const THREE usize = 3\n\
     struct Unit { value u64 }\n\
     struct Buffer[T, N const usize] { data arr[N, T] }\n\
     struct Wrapped[T, N const usize] { value Buffer[T, N] }\n\
     fn pass[T, N const usize](value Buffer[T, N]) Buffer[T, N] {\n\
    \ copy Buffer[T, N] = value\n\
    \ return copy\n\
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
                      body =
                        Hir.Statements
                          [
                            Hir.Let
                              ( { Hir.name = "x"; ty = Hir.Opaque "X"; id = 0 },
                                None,
                                Span.synthetic );
                          ];
                      linkage = Hir.Internal;
                      variadic = false;
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
  ignore
    (llvm_of
       "struct Pair[T] { left T right T }\n\
        fn main() i64 { return ((Pair[i64]){12, 4}).left }\n");

  print_endline "regression tests: 291 passed"
