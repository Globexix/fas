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

let semantic_messages text =
  let program = expect_ok (Parser.parse (source text)) in
  match Sema.check program with
  | Ok _ -> failwith "expected semantic rejection"
  | Error diagnostics ->
      List.map (fun (diagnostic : Diag.t) -> diagnostic.message) diagnostics

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
  if
    not
      (contains explicit_assembly_llvm
         "define i64 @assembly_helper(i64 %x)")
  then failwith "assembly-linkage-explicit: C ABI helper is not externally visible";
  if
    not
      (contains (Ir.raw_assembly explicit_assembly_program) "call assembly_helper")
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
            "struct Buffer[T, N const usize] { data ptr[T] }\n\
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
       fields = [ { ty = Ast.Ptr (Ast.Named_type "T"); _ } ];
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
  parse_error_message "type-generic-missing-const-type" "expected a type"
    "fn bad[T const](value T) T { return value }\n";
  semantic_error "type-generic-checkpoint"
    "type-generic declarations are not available in this checkpoint"
    "struct Box[T] { value T }\n";

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
               params = [ ("x", Hir.Opaque "X") ];
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
                          [ Hir.Let ("x", Hir.Opaque "X", None, Span.synthetic) ];
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

  print_endline "regression tests: 180 passed"
