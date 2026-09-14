let source text = Source.create ~file:"test.fas" ~text

let expect_ok = function
  | Ok value -> value
  | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)

let contains text needle =
  let n = String.length text and m = String.length needle in
  let rec go i = i + m <= n && (String.sub text i m = needle || go (i + 1)) in
  m = 0 || go 0

let () =
  assert (Limits.budget_version_name Limits.default_budget_version = "0.15");
  assert (Limits.for_budget_version Limits.V0_15 = Limits.default);
  assert (Limits.default.max_tokens = 1_000_000);
  assert (Limits.default.max_nesting = 128);
  assert (Limits.default.max_asm_bytes = 4_000_000);
  assert (Limits.default.max_interned_string_bytes = 4_000_000);
  assert (Limits.default.max_rendered_ir_bytes = 4_000_000);
  assert (Limits.default.max_specializations = 10_000);
  assert (Limits.default.max_specialization_depth = 64);
  assert (Limits.default.max_aggregate_elements = 1_000_000);
  assert (Limits.default.max_object_alignment = 1_048_576);
  assert (Limits.default.max_object_size = 1_073_741_824);
  assert (Target_layout.pointer_integer_bits Target_layout.current = Ok 64);
  assert (
    Target_layout.pointer_integer_bits
      { Target_layout.current with pointer_size = 4; pointer_align = 4 }
    = Ok 32);
  assert (
    Target_layout.pointer_integer_bits
      { Target_layout.current with pointer_size = 16; pointer_align = 16 }
    = Error "unsupported pointer size: 16");
  let layout_declarations =
    [
      ("Leaf", [ ("value", Hir.Int Hir.U8) ], None);
      ("Left", [ ("leaf", Hir.Struct "Leaf") ], None);
      ("Right", [ ("leaf", Hir.Struct "Leaf") ], None);
      ("Root", [ ("left", Hir.Struct "Left"); ("right", Hir.Struct "Right") ], None);
    ]
  in
  let layout_cache = Hir.struct_layout_cache layout_declarations in
  let root_layout =
    match Hir.compute_struct_cached layout_cache "Root" with
    | Ok definition -> definition
    | Error message -> failwith message
  in
  assert (root_layout.size = 2);
  assert (Hashtbl.length layout_cache.definitions = 4);
  ignore
    (match Hir.compute_struct_cached layout_cache "Root" with
    | Ok definition -> definition
    | Error message -> failwith message);
  assert (Hashtbl.length layout_cache.definitions = 4);
  let pointer_declarations =
    [ ("Pointer", [ ("value", Hir.Ptr (Hir.Int Hir.U8)) ], None) ]
  in
  let target32 = { Target_layout.current with pointer_size = 4; pointer_align = 4 } in
  let pointer64 =
    match
      Hir.compute_struct_cached (Hir.struct_layout_cache pointer_declarations) "Pointer"
    with
    | Ok definition -> definition
    | Error message -> failwith message
  in
  let pointer32 =
    match
      Hir.compute_struct_cached
        (Hir.struct_layout_cache ~target:target32 pointer_declarations)
        "Pointer"
    with
    | Ok definition -> definition
    | Error message -> failwith message
  in
  assert (pointer64.size = 8);
  assert (pointer32.size = 4);
  let recursive_declarations =
    [
      ("First", [ ("second", Hir.Struct "Second") ], None);
      ("Second", [ ("first", Hir.Struct "First") ], None);
    ]
  in
  let recursive_cache = Hir.struct_layout_cache recursive_declarations in
  (match Hir.compute_struct_cached recursive_cache "First" with
  | Error message -> assert (message = "recursive by-value struct `First`")
  | Ok _ -> assert false);
  assert (Hashtbl.length recursive_cache.definitions = 0);
  let overflowing_declarations =
    [
      ( "Overflowing",
        [ ("large", Hir.Array (max_int, Hir.Int Hir.U8)); ("tail", Hir.Int Hir.U8) ],
        None );
    ]
  in
  let overflowing_cache = Hir.struct_layout_cache overflowing_declarations in
  (match Hir.compute_struct_cached overflowing_cache "Overflowing" with
  | Error message -> assert (message = "aggregate size overflows")
  | Ok _ -> assert false);
  assert (Hashtbl.length overflowing_cache.definitions = 0);
  let overflowing_program =
    expect_ok
      (Parser.parse
         (source
            (Printf.sprintf
               "struct Overflowing { large arr[%d,u8] tail u8 }\n\
                fn f() void { return }\n"
               max_int)))
  in
  (match
     Sema.check
       ~limits:
         {
           Limits.default with
           max_aggregate_elements = max_int;
           max_object_size = max_int;
         }
       overflowing_program
   with
  | Error diagnostics ->
      assert (
        contains (Diag.render_all ~source:None diagnostics) "aggregate size overflows")
  | Ok _ -> assert false);
  let padding_overflow_declarations =
    [
      ( "PaddingOverflow",
        [ ("large", Hir.Array (max_int - 1, Hir.Int Hir.U8)) ],
        Some (1 lsl 31) );
    ]
  in
  let padding_overflow_cache = Hir.struct_layout_cache padding_overflow_declarations in
  (match Hir.compute_struct_cached padding_overflow_cache "PaddingOverflow" with
  | Error message -> assert (message = "aggregate size overflows")
  | Ok _ -> assert false);
  assert (Hashtbl.length padding_overflow_cache.definitions = 0);
  let object_size_program =
    expect_ok (Parser.parse (source "fn f() void { value arr[16,u8]\n return }\n"))
  in
  ignore
    (expect_ok
       (Sema.check
          ~limits:{ Limits.default with max_object_size = 16 }
          object_size_program));
  (match
     Sema.check ~limits:{ Limits.default with max_object_size = 15 } object_size_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "object size exceeds compiler budget of 15 bytes")
  | Ok _ -> assert false);
  let generic_object_size_program =
    expect_ok
      (Parser.parse
         (source
            "struct Box[T] { value T }\nfn f(value Box[arr[16,u8]]) void { return }\n"))
  in
  ignore
    (expect_ok
       (Sema.check
          ~limits:{ Limits.default with max_object_size = 16 }
          generic_object_size_program));
  (match
     Sema.check
       ~limits:{ Limits.default with max_object_size = 15 }
       generic_object_size_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "object size exceeds compiler budget of 15 bytes")
  | Ok _ -> assert false);
  let aligned_program =
    expect_ok
      (Parser.parse
         (source "struct Aligned @align(16) { value u8 }\nfn f() void { return }\n"))
  in
  ignore
    (expect_ok
       (Sema.check
          ~limits:{ Limits.default with max_object_alignment = 16 }
          aligned_program));
  (match
     Sema.check ~limits:{ Limits.default with max_object_alignment = 8 } aligned_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "alignment exceeds compiler budget of 8")
  | Ok _ -> assert false);
  let target_alignment_program =
    expect_ok (Parser.parse (source "struct Invalid @align(4294967296) { value u8 }\n"))
  in
  (match
     Sema.check
       ~limits:{ Limits.default with max_object_alignment = 8 }
       target_alignment_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "alignment exceeds target maximum of 2147483648")
  | Ok _ -> assert false);
  let natural_alignment_program =
    expect_ok (Parser.parse (source "fn f() void { value vec[16,u8]\n return }\n"))
  in
  ignore
    (expect_ok
       (Sema.check
          ~limits:{ Limits.default with max_object_alignment = 16 }
          natural_alignment_program));
  (match
     Sema.check
       ~limits:{ Limits.default with max_object_alignment = 8 }
       natural_alignment_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "alignment exceeds compiler budget of 8")
  | Ok _ -> assert false);
  let generic_alignment_program =
    expect_ok
      (Parser.parse
         (source
            "struct Box[T] { value T }\nfn f(value Box[vec[16,u8]]) void { return }\n"))
  in
  (match
     Sema.check
       ~limits:{ Limits.default with max_object_alignment = 8 }
       generic_alignment_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "alignment exceeds compiler budget of 8")
  | Ok _ -> assert false);
  let aggregate_budget_program =
    expect_ok
      (Parser.parse
         (source
            "struct Pair { left arr[3,u8] right arr[3,u8] }\n\
             fn f() void { value Pair\n\
            \ return }\n"))
  in
  ignore
    (expect_ok
       (Sema.check
          ~limits:{ Limits.default with max_aggregate_elements = 6 }
          aggregate_budget_program));
  (match
     Sema.check
       ~limits:{ Limits.default with max_aggregate_elements = 5 }
       aggregate_budget_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "aggregate element count exceeds the configured limit")
  | Ok _ -> assert false);
  let aggregate_substitution_program =
    expect_ok
      (Parser.parse
         (source
            "struct Pair[T] { left T right T }\n\
             fn f(value Pair[arr[3,u8]]) void { return }\n"))
  in
  ignore
    (expect_ok
       (Sema.check
          ~limits:{ Limits.default with max_aggregate_elements = 6 }
          aggregate_substitution_program));
  (match
     Sema.check
       ~limits:{ Limits.default with max_aggregate_elements = 5 }
       aggregate_substitution_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "aggregate element count exceeds the configured limit")
  | Ok _ -> assert false);
  let nested_vector_budget_program =
    expect_ok
      (Parser.parse
         (source
            "struct Wrapped { values arr[2,vec[3,u8]] }\n\
             fn f(value Wrapped) void { return }\n"))
  in
  ignore
    (expect_ok
       (Sema.check
          ~limits:{ Limits.default with max_aggregate_elements = 6 }
          nested_vector_budget_program));
  (match
     Sema.check
       ~limits:{ Limits.default with max_aggregate_elements = 5 }
       nested_vector_budget_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "aggregate element count exceeds the configured limit")
  | Ok _ -> assert false);
  let diagnostic_source = Source.create ~file:"first.fas" ~text:"wrong line\n" in
  let foreign_span =
    Span.make ~file:"second.fas" ~start_offset:0 ~end_offset:1 ~line:1 ~column:1
  in
  let foreign_diagnostic = Diag.error foreign_span "failure" in
  let rendered = Diag.render ~source:(Some diagnostic_source) foreign_diagnostic in
  assert (not (contains rendered "wrong line"));
  let tokens = expect_ok (Lexer.lex (source "fn main() i64 { return 0x2a + 1\n }\n")) in
  assert (List.exists (fun token -> token.Token.kind = Token.Kw_fn) tokens);
  assert (List.exists (fun token -> token.Token.kind = Token.Int "0x2a") tokens);
  (match Lexer.lex (source "0x") with
  | Ok _ -> assert false
  | Error diagnostics ->
      let rendered = Diag.render_all ~source:None diagnostics in
      assert (contains rendered "invalid integer literal");
      assert (not (contains rendered "invalid digit in integer literal")));
  let program =
    expect_ok
      (Parser.parse
         (source
            "struct S { x i64 }\n\
             const C i64 = 4\n\
             fn f(a i64) i64 { y i64 = a + C\n\
            \ return y }\n"))
  in
  assert (List.length program.Ast.items = 3);
  let typed = expect_ok (Sema.check program) in
  assert (List.length typed.Hir.structs = 1 && List.length typed.Hir.funcs = 1);
  let bad = expect_ok (Parser.parse (source "fn bad() i64 { x i64\n return x }\n")) in
  (match Sema.check bad with
  | Ok _ -> assert false
  | Error diagnostics -> assert (diagnostics <> []));
  let opaque =
    expect_ok
      (Parser.parse (source "opaque Ctx\nfn bad(p ptr[Ctx]) i64 { return p.* }\n"))
  in
  (match Sema.check opaque with
  | Ok _ -> assert false
  | Error diagnostics -> assert (diagnostics <> []));
  let generic =
    expect_ok
      (Parser.parse
         (source
            "fn id[N const usize](x u64) u64 { return x + bitcast[u64](N) }\n\
             fn main() u64 { return id[3](2) }\n"))
  in
  let specialized = expect_ok (Sema.check generic) in
  assert (List.length specialized.Hir.funcs = 2);
  let lowered = expect_ok (Lower.lower specialized) in
  assert (List.length lowered.Ir.funcs = 2);
  let ir_module ?(structs = []) ?(globals = []) ?no_inline_function funcs =
    {
      Ir.target_triple = Target_layout.current.triple;
      data_layout = Target_layout.current.llvm_data_layout;
      structs;
      globals;
      funcs;
      no_inline_function;
    }
  in
  let ir_function blocks =
    {
      Ir.name = "control_flow";
      params = [];
      ret = Ir.Void;
      ret_extension = Ir.No_extension;
      blocks;
      linkage = Ir.Internal;
      variadic = false;
      asm_body = None;
    }
  in
  let ir_block ?(instrs = []) id terminator =
    { Ir.id; label = "b"; instrs; terminator }
  in
  let expect_ir_error fragment module_ =
    match Ir.validate module_ with
    | Error message -> assert (contains message fragment)
    | Ok () -> assert false
  in
  let valid_control_flow =
    ir_module
      [
        ir_function
          [
            ir_block
              ~instrs:
                [
                  Ir.Cast (0, "zext", Ir.I8, Ir.Const (Ir.I8, 255L), Ir.I16);
                  Ir.Bin (1, Ir.Add, Ir.I16, Ir.Local (0, Ir.I16), Ir.Const (Ir.I16, 1L));
                ]
              0
              (Ir.CondBr (Ir.Const (Ir.I1, 1L), 1, 2));
            ir_block 1 (Ir.Switch (Ir.I8, Ir.Const (Ir.I8, 0L), [ (0L, 2) ], 3));
            ir_block 2 (Ir.Br 3);
            ir_block
              ~instrs:
                [
                  Ir.Phi
                    ( 2,
                      Ir.I16,
                      [ (Ir.Const (Ir.I16, 1L), 1); (Ir.Local (1, Ir.I16), 2) ] );
                ]
              3 (Ir.Ret None);
          ];
      ]
  in
  assert (Ir.validate valid_control_flow = Ok ());
  let duplicate_block_ids =
    ir_module [ ir_function [ ir_block 0 (Ir.Ret None); ir_block 0 (Ir.Ret None) ] ]
  in
  expect_ir_error "duplicate block id 0" duplicate_block_ids;
  expect_ir_error "negative block id -1"
    (ir_module [ ir_function [ ir_block (-1) (Ir.Ret None) ] ]);
  let missing_branch_successor = ir_module [ ir_function [ ir_block 0 (Ir.Br 1) ] ] in
  expect_ir_error "block 0 has unknown successor 1" missing_branch_successor;
  expect_ir_error "entry block 0 has predecessors"
    (ir_module [ ir_function [ ir_block 0 (Ir.Br 1); ir_block 1 (Ir.Br 0) ] ]);
  let missing_switch_successor =
    ir_module
      [
        ir_function
          [ ir_block 0 (Ir.Switch (Ir.I8, Ir.Const (Ir.I8, 0L), [ (0L, 1) ], 0)) ];
      ]
  in
  expect_ir_error "block 0 has unknown successor 1" missing_switch_successor;
  let mistyped_operand =
    ir_module
      [
        ir_function
          [
            ir_block
              ~instrs:
                [
                  Ir.Bin (0, Ir.Add, Ir.I8, Ir.Const (Ir.I16, 1L), Ir.Const (Ir.I8, 2L));
                ]
              0 (Ir.Ret None);
          ];
      ]
  in
  expect_ir_error "operand with the wrong type" mistyped_operand;
  let malformed_constant =
    ir_module
      [
        ir_function
          [
            ir_block
              ~instrs:
                [
                  Ir.Bin (0, Ir.And, Ir.I1, Ir.Const (Ir.I1, -1L), Ir.Const (Ir.I1, 1L));
                ]
              0 (Ir.Ret None);
          ];
      ]
  in
  expect_ir_error "integer constant does not fit its type" malformed_constant;
  let invalid_cast =
    ir_module
      [
        ir_function
          [
            ir_block
              ~instrs:[ Ir.Cast (0, "zext", Ir.I16, Ir.Const (Ir.I16, 1L), Ir.I8) ]
              0 (Ir.Ret None);
          ];
      ]
  in
  expect_ir_error "invalid `zext` types" invalid_cast;
  let duplicate_value_ids =
    ir_module
      [
        ir_function
          [
            ir_block
              ~instrs:[ Ir.Alloca (0, Ir.I8, 1); Ir.Alloca (0, Ir.I16, 2) ]
              0 (Ir.Ret None);
          ];
      ]
  in
  expect_ir_error "duplicate value id 0" duplicate_value_ids;
  let undefined_value =
    ir_module
      [
        ir_function
          [
            ir_block
              ~instrs:
                [ Ir.Bin (0, Ir.Add, Ir.I8, Ir.Local (4, Ir.I8), Ir.Const (Ir.I8, 1L)) ]
              0 (Ir.Ret None);
          ];
      ]
  in
  expect_ir_error "uses undefined value 4" undefined_value;
  let wrong_value_claim =
    ir_module
      [
        ir_function
          [
            ir_block
              ~instrs:
                [ Ir.Alloca (0, Ir.I8, 1); Ir.Load (1, Ir.I8, Ir.Local (0, Ir.I16), 1) ]
              0 (Ir.Ret None);
          ];
      ]
  in
  expect_ir_error "value 0 claims the wrong type" wrong_value_claim;
  let forward_value_use =
    ir_module
      [
        ir_function
          [
            ir_block
              ~instrs:
                [
                  Ir.Bin (1, Ir.Add, Ir.I8, Ir.Local (0, Ir.I8), Ir.Const (Ir.I8, 1L));
                  Ir.Bin (0, Ir.Add, Ir.I8, Ir.Const (Ir.I8, 2L), Ir.Const (Ir.I8, 3L));
                ]
              0 (Ir.Ret None);
          ];
      ]
  in
  expect_ir_error "value 0 does not dominate its use" forward_value_use;
  let sibling_value_use =
    ir_module
      [
        ir_function
          [
            ir_block 0 (Ir.CondBr (Ir.Const (Ir.I1, 1L), 1, 2));
            ir_block
              ~instrs:
                [
                  Ir.Bin (0, Ir.Add, Ir.I8, Ir.Const (Ir.I8, 1L), Ir.Const (Ir.I8, 2L));
                ]
              1 (Ir.Br 3);
            ir_block
              ~instrs:
                [ Ir.Bin (1, Ir.Add, Ir.I8, Ir.Local (0, Ir.I8), Ir.Const (Ir.I8, 3L)) ]
              2 (Ir.Br 3);
            ir_block 3 (Ir.Ret None);
          ];
      ]
  in
  expect_ir_error "value 0 does not dominate its use" sibling_value_use;
  let invalid_phi_edge_use =
    ir_module
      [
        ir_function
          [
            ir_block 0 (Ir.CondBr (Ir.Const (Ir.I1, 1L), 1, 2));
            ir_block
              ~instrs:
                [
                  Ir.Bin (0, Ir.Add, Ir.I8, Ir.Const (Ir.I8, 1L), Ir.Const (Ir.I8, 2L));
                ]
              1 (Ir.Br 3);
            ir_block 2 (Ir.Br 3);
            ir_block
              ~instrs:
                [
                  Ir.Phi
                    (1, Ir.I8, [ (Ir.Const (Ir.I8, 0L), 1); (Ir.Local (0, Ir.I8), 2) ]);
                ]
              3 (Ir.Ret None);
          ];
      ]
  in
  expect_ir_error "value 0 does not dominate its use" invalid_phi_edge_use;
  let incomplete_phi =
    ir_module
      [
        ir_function
          [
            ir_block 0 (Ir.CondBr (Ir.Const (Ir.I1, 1L), 1, 2));
            ir_block 1 (Ir.Br 3);
            ir_block 2 (Ir.Br 3);
            ir_block
              ~instrs:[ Ir.Phi (0, Ir.I8, [ (Ir.Const (Ir.I8, 1L), 1) ]) ]
              3 (Ir.Ret None);
          ];
      ]
  in
  expect_ir_error "phi inputs do not match its predecessors" incomplete_phi;
  let duplicate_phi_input =
    ir_module
      [
        ir_function
          [
            ir_block 0 (Ir.Br 1);
            ir_block
              ~instrs:
                [
                  Ir.Phi
                    (0, Ir.I8, [ (Ir.Const (Ir.I8, 1L), 0); (Ir.Const (Ir.I8, 2L), 0) ]);
                ]
              1 (Ir.Ret None);
          ];
      ]
  in
  expect_ir_error "phi has duplicate incoming block 0" duplicate_phi_input;
  let misplaced_phi =
    ir_module
      [
        ir_function
          [
            ir_block 0 (Ir.Br 1);
            ir_block
              ~instrs:
                [
                  Ir.Alloca (0, Ir.I8, 1);
                  Ir.Phi (1, Ir.I8, [ (Ir.Const (Ir.I8, 1L), 0) ]);
                ]
              1 (Ir.Ret None);
          ];
      ]
  in
  expect_ir_error "phi after a non-phi instruction" misplaced_phi;
  let call_target =
    {
      (ir_function []) with
      Ir.name = "callee";
      params = [ { Ir.name = "x"; ty = Ir.I8; extension = Ir.Zero_extension } ];
      ret = Ir.I16;
      ret_extension = Ir.Sign_extension;
      variadic = true;
    }
  in
  let call_caller call =
    {
      (ir_function [ ir_block ~instrs:[ call ] 0 (Ir.Ret None) ]) with
      Ir.name = "caller";
    }
  in
  let valid_call =
    Ir.Call
      ( Some 0,
        Ir.Sign_extension,
        Ir.I16,
        "callee",
        [
          (Ir.I8, Ir.Zero_extension, Ir.Const (Ir.I8, 7L));
          (Ir.I64, Ir.No_extension, Ir.Const (Ir.I64, 9L));
        ] )
  in
  assert (Ir.validate (ir_module [ call_target; call_caller valid_call ]) = Ok ());
  expect_ir_error "duplicate function name `callee`"
    (ir_module [ call_target; call_target ]);
  expect_ir_error "call to `missing` has no matching function"
    (ir_module
       [ call_caller (Ir.Call (None, Ir.No_extension, Ir.Void, "missing", [])) ]);
  expect_ir_error "call to `callee` has the wrong return type"
    (ir_module
       [
         call_target;
         call_caller
           (Ir.Call
              ( Some 0,
                Ir.Sign_extension,
                Ir.I8,
                "callee",
                [ (Ir.I8, Ir.Zero_extension, Ir.Const (Ir.I8, 7L)) ] ));
       ]);
  expect_ir_error "call to `callee` has the wrong return extension"
    (ir_module
       [
         call_target;
         call_caller
           (Ir.Call
              ( Some 0,
                Ir.Zero_extension,
                Ir.I16,
                "callee",
                [ (Ir.I8, Ir.Zero_extension, Ir.Const (Ir.I8, 7L)) ] ));
       ]);
  expect_ir_error "call to `callee` argument 0 has the wrong type"
    (ir_module
       [
         call_target;
         call_caller
           (Ir.Call
              ( Some 0,
                Ir.Sign_extension,
                Ir.I16,
                "callee",
                [ (Ir.I16, Ir.Zero_extension, Ir.Const (Ir.I16, 7L)) ] ));
       ]);
  expect_ir_error "call to `callee` argument 0 has the wrong extension"
    (ir_module
       [
         call_target;
         call_caller
           (Ir.Call
              ( Some 0,
                Ir.Sign_extension,
                Ir.I16,
                "callee",
                [ (Ir.I8, Ir.Sign_extension, Ir.Const (Ir.I8, 7L)) ] ));
       ]);
  expect_ir_error "call to `callee` has 0 arguments but requires at least 1"
    (ir_module
       [
         call_target;
         call_caller (Ir.Call (Some 0, Ir.Sign_extension, Ir.I16, "callee", []));
       ]);
  let fixed_target = { call_target with Ir.variadic = false } in
  expect_ir_error "call to `callee` has 2 arguments but requires exactly 1"
    (ir_module [ fixed_target; call_caller valid_call ]);
  let byte_struct = { Ir.name = "Byte"; fields = [ Ir.I8 ]; tail_padding = 0 } in
  let string_global = Ir.String_global { name = ".str.0"; bytes = "ok" } in
  let array_global =
    Ir.Array_global { name = "numbers"; elem_ty = Ir.I8; elems = [ 1L; 2L ]; align = 1 }
  in
  let module_reference_user =
    {
      (ir_function
         [
           ir_block
             ~instrs:
               [
                 Ir.Alloca (0, Ir.Struct "Byte", 1);
                 Ir.String_ptr (1, 0, 2);
                 Ir.Global_ptr (2, "numbers", Ir.Array (2, Ir.I8));
                 Ir.Load
                   (3, Ir.I8, Ir.Global ("numbers", Ir.Ptr (Ir.Array (2, Ir.I8))), 1);
               ]
             0 (Ir.Ret None);
         ])
      with
      Ir.name = "module_reference_user";
    }
  in
  assert (
    Ir.validate
      (ir_module ~structs:[ byte_struct ]
         ~globals:[ string_global; array_global ]
         [ module_reference_user ])
    = Ok ());
  expect_ir_error "duplicate struct name `Byte`"
    (ir_module ~structs:[ byte_struct; byte_struct ] []);
  expect_ir_error "struct `Broken` references an unknown struct"
    (ir_module
       ~structs:
         [ { Ir.name = "Broken"; fields = [ Ir.Struct "Missing" ]; tail_padding = 0 } ]
       []);
  expect_ir_error "struct `Recursive` has a recursive value layout"
    (ir_module
       ~structs:
         [
           {
             Ir.name = "Recursive";
             fields = [ Ir.Array (1, Ir.Struct "Recursive") ];
             tail_padding = 0;
           };
         ]
       []);
  expect_ir_error "duplicate global name `numbers`"
    (ir_module ~globals:[ array_global; array_global ] []);
  expect_ir_error "global `numbers` has an element outside its type"
    (ir_module
       ~globals:
         [
           Ir.Array_global
             { name = "numbers"; elem_ty = Ir.I1; elems = [ 2L ]; align = 1 };
         ]
       []);
  expect_ir_error "symbol `collision` is both a global and a function"
    (ir_module
       ~globals:
         [
           Ir.Array_global
             { name = "collision"; elem_ty = Ir.I8; elems = []; align = 1 };
         ]
       [ { (ir_function []) with Ir.name = "collision" } ]);
  expect_ir_error "string pointer `.str.0` has the wrong length"
    (ir_module ~globals:[ string_global ] [ call_caller (Ir.String_ptr (0, 0, 3)) ]);
  expect_ir_error "global pointer `numbers` has the wrong type"
    (ir_module ~globals:[ array_global ]
       [ call_caller (Ir.Global_ptr (0, "numbers", Ir.Array (1, Ir.I8))) ]);
  expect_ir_error "uses unknown global `missing`"
    (ir_module
       [ call_caller (Ir.Load (0, Ir.I8, Ir.Global ("missing", Ir.Ptr Ir.I8), 1)) ]);
  expect_ir_error "global `numbers` claims the wrong type"
    (ir_module ~globals:[ array_global ]
       [ call_caller (Ir.Load (0, Ir.I8, Ir.Global ("numbers", Ir.I8), 1)) ]);
  expect_ir_error "no-inline function `missing` does not exist"
    (ir_module ~no_inline_function:"missing" []);
  assert (Ir.render lowered = Ir.render lowered);
  let unresolved_template_call =
    {
      Hir.structs = [];
      consts = [];
      const_arrays = [];
      funcs =
        [
          {
            Hir.name = "main";
            params = [];
            ret = Hir.Int Hir.I64;
            body =
              Hir.Statements
                [
                  Hir.Return
                    ( Some
                        (Hir.Call
                           (Hir.User "identity", [], Hir.Int Hir.I64, Span.synthetic)),
                      Span.synthetic );
                ];
            linkage = Hir.Internal;
            variadic = false;
          };
        ];
      strings = [];
    }
  in
  (match Lower.lower unresolved_template_call with
  | Ok _ -> assert false
  | Error [ diagnostic ] ->
      assert (diagnostic.message = "unknown function `identity` reached lowering")
  | Error _ -> assert false);
  let golden_source = source "fn main() i64 { x i64 = 2\n return x + 3\n }\n" in
  let golden_ast = expect_ok (Parser.parse golden_source) in
  let golden_hir = expect_ok (Sema.check golden_ast) in
  let golden_ir = Ir.render (expect_ok (Lower.lower golden_hir)) in
  let channel = open_in "ir_simple.expected" in
  let expected = really_input_string channel (in_channel_length channel) in
  close_in channel;
  assert (golden_ir = expected);
  (match Process.run [| "/bin/printf"; "process-ok" |] with
  | Ok (out, _) -> assert (out = "process-ok")
  | Error _ -> assert false);
  let builtin =
    expect_ok
      (Parser.parse (source "fn f(p ptr[u8]) ptr[u8] { return ptr_add(p, 1) }\n"))
  in
  assert (List.length builtin.Ast.items = 1);
  let asm =
    "fn fake() i64 { return 1 }\n\
     asm fn raw_move(x i64) i64 // comment containing {\n\
     {\n\
    \  # brace }\n\
    \  .ascii \"asm fn fake { }\"\n\
     }\n"
  in
  let asm_program = expect_ok (Parser.parse (source asm)) in
  assert (List.length asm_program.Ast.items = 2);
  (match List.nth asm_program.Ast.items 1 with
  | Ast.Func { body = Ast.Asm text; _ } -> assert (String.length text > 10)
  | _ -> assert false);
  let cumulative_asm =
    source "asm fn first() void {1234}\nasm fn second() void {5678}\n"
  in
  ignore
    (expect_ok
       (Parser.parse ~limits:{ Limits.default with max_asm_bytes = 8 } cumulative_asm));
  (match
     Parser.parse ~limits:{ Limits.default with max_asm_bytes = 7 } cumulative_asm
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "cumulative raw asm bytes exceed the configured limit")
  | Ok _ -> assert false);
  (match
     Parser.parse ~limits:{ Limits.default with max_asm_bytes = 3 } cumulative_asm
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "raw asm body exceeds the configured limit")
  | Ok _ -> assert false);
  let rendered_module =
    let parsed =
      expect_ok
        (Parser.parse
           (source
              "fn main() i64 { s ptr[const u8] = \"a\\n\\t\\\\\\\"z\\0\"\n\
              \ return 0\n\
              \ }\n"))
    in
    let hir = expect_ok (Sema.check parsed) in
    expect_ok (Lower.lower hir)
  in
  let rendered_text = Ir.render rendered_module in
  let rendered_bytes = String.length rendered_text in
  assert (contains rendered_text "c\"a\\0A\\09\\5C\\22z\\00\"");
  let expect_render_ok budget =
    match Ir.render_bounded ~budget rendered_module with
    | Ok text -> assert (text = rendered_text)
    | Error _ -> assert false
  in
  let expect_render_error budget =
    match Ir.render_bounded ~budget rendered_module with
    | Error message ->
        assert (
          message
          = Printf.sprintf "rendered LLVM text exceeds the configured limit of %d bytes"
              budget)
    | Ok _ -> assert false
  in
  expect_render_ok rendered_bytes;
  expect_render_error (rendered_bytes - 1);
  expect_render_error 0;
  expect_render_ok rendered_bytes;
  assert (
    Ir.render_bounded ~budget:rendered_bytes rendered_module
    = Ir.render_bounded ~budget:rendered_bytes rendered_module);
  (match Ir.render_bounded ~budget:(-1) rendered_module with
  | Error message -> assert (message = "rendered LLVM text budget must not be negative")
  | Ok _ -> assert false);
  (match Ir.render_bounded ~budget:min_int rendered_module with
  | Error message -> assert (message = "rendered LLVM text budget must not be negative")
  | Ok _ -> assert false);
  let multi_block =
    ir_module
      [
        {
          (ir_function
             [
               ir_block 0 (Ir.CondBr (Ir.Const (Ir.I1, 1L), 1, 2));
               ir_block 1 (Ir.Br 2);
               ir_block 2 (Ir.Ret (Some (Ir.I32, Ir.Const (Ir.I32, 0L))));
             ])
          with
          Ir.name = "flow";
          Ir.ret = Ir.I32;
        };
      ]
  in
  assert (Ir.validate multi_block = Ok ());
  let multi_block_expected =
    "; ModuleID = 'fas'\n\
     source_filename = \"fas\"\n\
     target datalayout = \
     \"e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128\"\n\
     target triple = \"x86_64-unknown-linux-gnu\"\n\n\
     define internal i32 @flow() {\n\
     b0:\n\
    \  br i1 true, label %b1, label %b2\n\
     b1:\n\
    \  br label %b2\n\
     b2:\n\
    \  ret i32 0\n\
     }\n"
  in
  assert (Ir.render multi_block = multi_block_expected);
  assert (
    Ir.render_bounded ~budget:(String.length multi_block_expected) multi_block
    = Ok multi_block_expected);
  let verify_path = Filename.temp_file "fas-render-verify-" ".ll" in
  Fun.protect
    ~finally:(fun () -> Sys.remove verify_path)
    (fun () ->
      let channel = open_out_bin verify_path in
      output_string channel multi_block_expected;
      close_out channel;
      match
        Process.run [| "opt-22"; "-passes=verify"; "-disable-output"; verify_path |]
      with
      | Ok _ -> ()
      | Error failure ->
          failwith
            ("opt-22 rejected rendered multi-block LLVM: " ^ failure.Process.stderr));
  let wide_module =
    let params =
      List.init 100_000 (fun i ->
          { Ir.name = "p" ^ string_of_int i; ty = Ir.I8; extension = Ir.No_extension })
    in
    let args =
      List.init 100_000 (fun _ -> (Ir.I8, Ir.No_extension, Ir.Const (Ir.I8, 1L)))
    in
    ir_module
      [
        { (ir_function []) with Ir.name = "wide"; Ir.params };
        {
          (ir_function
             [
               ir_block
                 ~instrs:[ Ir.Call (None, Ir.No_extension, Ir.Void, "wide", args) ]
                 0 (Ir.Ret None);
             ])
          with
          Ir.name = "caller";
        };
      ]
  in
  assert (Ir.validate wide_module = Ok ());
  (match Ir.render_bounded ~budget:1024 wide_module with
  | Error message ->
      assert (message = "rendered LLVM text exceeds the configured limit of 1024 bytes")
  | Ok _ -> assert false);
  let wide_text = Ir.render wide_module in
  (match Ir.render_bounded ~budget:(String.length wide_text) wide_module with
  | Ok text -> assert (text = wide_text)
  | Error _ -> assert false);
  let func_string_ids (func : Hir.func) =
    let from_expr = function Hir.EString (id, _) -> [ id ] | _ -> [] in
    match func.Hir.body with
    | Hir.Statements statements ->
        List.concat_map
          (function
            | Hir.Let (_, Some value, _) -> from_expr value
            | Hir.Assign (_, value, _) -> from_expr value
            | Hir.Return (Some value, _) -> from_expr value
            | Hir.Expr (value, _) -> from_expr value
            | _ -> [])
          statements
    | _ -> []
  in
  let expect_budget_error name ~line ~column ~message ~notes outcome =
    match outcome with
    | Error [ diagnostic ] ->
        assert (diagnostic.Diag.message = message);
        assert (diagnostic.Diag.primary.Span.file = "test.fas");
        assert (diagnostic.Diag.primary.Span.line = line);
        assert (diagnostic.Diag.primary.Span.column = column);
        assert (diagnostic.Diag.notes = notes)
    | _ -> failwith (name ^ ": expected a single budget diagnostic")
  in
  let string_literal_program =
    expect_ok
      (Parser.parse (source "fn f() void { s ptr[const u8] = \"abc\"\n return }\n"))
  in
  expect_budget_error "string single" ~line:1 ~column:33
    ~message:"string literal exceeds the configured limit of 2 bytes" ~notes:[]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 2 }
       string_literal_program);
  let boundary_strings =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 3 }
         string_literal_program)
  in
  assert (boundary_strings.Hir.strings = [ "abc" ]);
  let c_string_program =
    expect_ok
      (Parser.parse (source "fn f() void { s ptr[const u8] = c\"abc\"\n return }\n"))
  in
  expect_budget_error "c-string single" ~line:1 ~column:33
    ~message:"string literal exceeds the configured limit of 3 bytes" ~notes:[]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 3 }
       c_string_program);
  let nul_charged_strings =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 4 }
         c_string_program)
  in
  assert (nul_charged_strings.Hir.strings = [ "abc\000" ]);
  let duplicated_strings =
    expect_ok
      (Parser.parse
         (source
            "fn f() void { a ptr[const u8] = \"abc\"\n\
            \ b ptr[const u8] = \"abc\"\n\
            \ return }\n"))
  in
  let deduplicated =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 3 }
         duplicated_strings)
  in
  assert (deduplicated.Hir.strings = [ "abc" ]);
  let function_named name hir =
    List.find (fun (func : Hir.func) -> func.Hir.name = name) hir.Hir.funcs
  in
  assert (func_string_ids (function_named "f" deduplicated) = [ 0; 0 ]);
  let cross_function_strings =
    expect_ok
      (Parser.parse
         (source
            "fn f() void { a ptr[const u8] = \"ab\"\n\
            \ return }\n\
             fn g() void { b ptr[const u8] = \"ab\"\n\
            \ c ptr[const u8] = \"cd\"\n\
            \ return }\n"))
  in
  expect_budget_error "cross-function cumulative" ~line:4 ~column:20
    ~message:"cumulative interned string bytes exceed the configured limit of 3 bytes"
    ~notes:[]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 3 }
       cross_function_strings);
  let shared_strings =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 4 }
         cross_function_strings)
  in
  assert (shared_strings.Hir.strings = [ "ab"; "cd" ]);
  assert (func_string_ids (function_named "f" shared_strings) = [ 0 ]);
  assert (func_string_ids (function_named "g" shared_strings) = [ 0; 1 ]);
  let ordering_strings =
    expect_ok
      (Parser.parse
         (source
            "fn f() void { a ptr[const u8] = \"ab\"\n\
            \ return }\n\
             fn g() void { b ptr[const u8] = \"cde\"\n\
            \ return }\n"))
  in
  expect_budget_error "ordering cumulative" ~line:3 ~column:33
    ~message:"cumulative interned string bytes exceed the configured limit of 4 bytes"
    ~notes:[]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 4 }
       ordering_strings);
  let ordered_strings =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 5 }
         ordering_strings)
  in
  assert (ordered_strings.Hir.strings = [ "ab"; "cde" ]);
  assert (func_string_ids (function_named "f" ordered_strings) = [ 0 ]);
  assert (func_string_ids (function_named "g" ordered_strings) = [ 1 ]);
  let specialized_strings =
    expect_ok
      (Parser.parse
         (source
            "fn echo[T](v T) ptr[const u8] { return \"abc\" }\n\
             fn main() void { a ptr[const u8] = echo[u8](1)\n\
            \ b ptr[const u8] = echo[i64](2)\n\
            \ return }\n"))
  in
  let specialization_shared =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 3 }
         specialized_strings)
  in
  assert (specialization_shared.Hir.strings = [ "abc" ]);
  let specializations =
    List.filter
      (fun (func : Hir.func) -> contains func.Hir.name "$spec$")
      specialization_shared.Hir.funcs
  in
  assert (List.length specializations = 2);
  assert (List.for_all (fun func -> func_string_ids func = [ 0 ]) specializations);
  let legality_single_strings =
    expect_ok
      (Parser.parse
         (source
            "fn big[N const u64](v ptr[u8]) ptr[const u8] { return \"toolongstr\" }\n\
             fn main(v ptr[u8]) void { d ptr[const u8] = big[1](v)\n\
            \ return }\n"))
  in
  expect_budget_error "legality single" ~line:1 ~column:55
    ~message:"string literal exceeds the configured limit of 8 bytes"
    ~notes:[ "while instantiating `big[1]` at test.fas:2:48" ]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 8 }
       legality_single_strings);
  let legality_cumulative_strings =
    expect_ok
      (Parser.parse
         (source
            "fn pair[N const u64](v ptr[u8]) ptr[const u8] {\n\
            \ a ptr[const u8] = \"aa\"\n\
            \ b ptr[const u8] = \"bb\"\n\
            \ return v }\n\
             fn main(v ptr[u8]) void { d ptr[const u8] = pair[1](v)\n\
            \ return }\n"))
  in
  expect_budget_error "legality cumulative" ~line:3 ~column:20
    ~message:"cumulative interned string bytes exceed the configured limit of 3 bytes"
    ~notes:[ "while instantiating `pair[1]` at test.fas:5:49" ]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 3 }
       legality_cumulative_strings);
  expect_budget_error "fresh state failure repeat" ~line:4 ~column:20
    ~message:"cumulative interned string bytes exceed the configured limit of 3 bytes"
    ~notes:[]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 3 }
       cross_function_strings);
  let refreshed =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 5 }
         ordering_strings)
  in
  assert (refreshed.Hir.strings = [ "ab"; "cde" ]);
  let after_failure =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 4 }
         cross_function_strings)
  in
  assert (after_failure.Hir.strings = [ "ab"; "cd" ]);
  (match Lexer.lex (source "/* unterminated") with
  | Ok _ -> assert false
  | Error _ -> ());
  (match Parser.parse (source "fn broken( i64) void {}") with
  | Ok _ -> assert false
  | Error diagnostics -> assert (diagnostics <> []));
  (match Parser.parse (source "const K arr[2, u8] = { 1 2 }\n") with
  | Ok _ -> assert false
  | Error diagnostics -> assert (diagnostics <> []));
  let overflow =
    expect_ok (Parser.parse (source "const X u64 = 18446744073709551616\n"))
  in
  (match Sema.check overflow with
  | Ok _ -> assert false
  | Error diagnostics -> assert (diagnostics <> []));
  let divzero = expect_ok (Parser.parse (source "const X i64 = 1 / 0\n")) in
  (match Sema.check divzero with
  | Ok _ -> assert false
  | Error diagnostics -> assert (diagnostics <> []));
  let bad_cmp =
    expect_ok (Parser.parse (source "fn bad() bool { return true < false }\n"))
  in
  (match Sema.check bad_cmp with
  | Ok _ -> assert false
  | Error diagnostics -> assert (diagnostics <> []));
  let expect_cli = function
    | Ok (Cli.Run value) -> value
    | Ok Cli.Help -> failwith "expected compiler invocation"
    | Error message -> failwith message
  in
  let cli =
    expect_cli (Cli.parse [| "fas"; "--emit-ast"; "-o"; "out"; "one.fas"; "two.fas" |])
  in
  assert (
    cli.Cli.emit = Cli.Ast && cli.output = "out" && cli.output_explicit
    && cli.inputs = [ "one.fas"; "two.fas" ]);
  let cli_obj = expect_cli (Cli.parse [| "fas"; "-c"; "path/prog.fas" |]) in
  assert (
    cli_obj.Cli.emit = Cli.Obj && cli_obj.output = "prog.o"
    && not cli_obj.output_explicit);
  let cli_asm = expect_cli (Cli.parse [| "fas"; "-S"; "path/prog.fas" |]) in
  assert (cli_asm.Cli.emit = Cli.Asm && cli_asm.output = "prog.s");
  let cli_named_obj =
    expect_cli (Cli.parse [| "fas"; "-c"; "-o"; "artifact"; "prog.fas" |])
  in
  assert (
    cli_named_obj.Cli.emit = Cli.Obj
    && cli_named_obj.output = "artifact"
    && cli_named_obj.output_explicit);
  (match Cli.parse [| "fas"; "--help" |] with
  | Ok Cli.Help -> ()
  | Ok (Cli.Run _) | Error _ -> assert false);
  let input_path = Filename.temp_file "fas-driver-" ".fas" in
  Fun.protect
    ~finally:(fun () -> Sys.remove input_path)
    (fun () ->
      let channel = open_out_bin input_path in
      output_string channel "fn main() i64 { return 3 }\n";
      close_out channel;
      let stdout_config =
        expect_cli (Cli.parse [| "fas"; "--emit-llvm"; input_path |])
      in
      assert (not stdout_config.output_explicit);
      (match Driver.run stdout_config with
      | Ok output -> assert (output <> "")
      | Error _ -> assert false);
      List.iter
        (fun flag ->
          let output_path = Filename.temp_file "fas-driver-output-" ".txt" in
          Fun.protect
            ~finally:(fun () -> Sys.remove output_path)
            (fun () ->
              let config =
                expect_cli (Cli.parse [| "fas"; flag; "-o"; output_path; input_path |])
              in
              assert config.output_explicit;
              (match Driver.run config with
              | Ok output -> assert (output = "")
              | Error _ -> assert false);
              let channel = open_in_bin output_path in
              let contents = really_input_string channel (in_channel_length channel) in
              close_in channel;
              assert (contents <> "")))
        [ "--emit-ast"; "--emit-ir"; "--emit-llvm" ]);
  (match Cli.parse [| "fas"; "--unknown" |] with Ok _ -> assert false | Error _ -> ());
  let sema_error ?message text =
    let program = expect_ok (Parser.parse (source text)) in
    match Sema.check program with
    | Ok _ -> assert false
    | Error diagnostics -> (
        match message with
        | None -> ()
        | Some fragment ->
            assert (contains (Diag.render_all ~source:None diagnostics) fragment))
  in
  sema_error "fn f(a i64, a i64) i64 { return a }\n";
  sema_error "fn f() i64 { break\n return 0 }\n";
  sema_error "fn f() i64 { switch 1 { case 1: { } case 1: { } } return 0 }\n";
  sema_error "fn f(x i64) i64 { switch x { case x: { } } return 0 }\n";
  sema_error ~message:"duplicate local `x`"
    "fn f() i64 { x i64 = 1\n x i64 = 2\n return x }\n";
  ignore
    (expect_ok
       (Sema.check
          (expect_ok
             (Parser.parse
                (source "fn f() i64 { x i64 = 1\n { x i64 = 2 } return x }\n")))));
  sema_error "fn f() i64 { x vec[0,u64]\n return 0 }\n";
  sema_error ~message:"use of uninitialized local `x`"
    "fn f() i64 { x i64\n defer { x = 1 }\n return x }\n";
  sema_error ~message:"nested defer is not allowed"
    "fn f() void { defer { defer { } } }\n";
  sema_error ~message:"void function cannot return a value"
    "extern \"C\" { fn sink() void }\nfn f() void { return sink() }\n";
  sema_error ~message:"may reach the end without returning" "fn f() i64 { }\n";
  let edge_program =
    expect_ok
      (Parser.parse
         (source
            "const B i64 = sext[i64](true)\n\
             fn rem(x i8) i8 { return x % -1 }\n\
             fn vecrot() i64 { x vec[4,u32] = splat(1)\n\
            \ y vec[4,u32] = rotl(x, 1)\n\
            \ return sext[i64](true) }\n"))
  in
  let edge_hir = expect_ok (Sema.check edge_program) in
  let edge_ir = expect_ok (Lower.lower edge_hir) in
  let edge_llvm = Ir.render edge_ir in
  assert (contains edge_llvm "call void @llvm.trap()");
  assert (contains edge_llvm "@llvm.fshl.v4i32");
  assert (contains edge_llvm "declare <4 x i32> @llvm.fshl.v4i32");
  assert (contains edge_llvm "sext i1 true to i64");
  let edge_debug = Ir.render_debug edge_ir in
  assert (contains edge_debug "Module {");
  assert (edge_debug <> edge_llvm);
  let token_limits = { Limits.default with max_tokens = 1 } in
  (match Lexer.lex ~limits:token_limits (source "x /* trailing trivia */ ") with
  | Ok [ { Token.kind = Token.Ident "x"; _ }; { kind = Token.Eof; _ } ] -> ()
  | Ok _ | Error _ -> assert false);
  (match Lexer.lex ~limits:token_limits (source "x y") with
  | Error diagnostics ->
      assert (contains (Diag.render_all ~source:None diagnostics) "token limit exceeded")
  | Ok _ -> assert false);
  let type_nesting_limits = { Limits.default with max_nesting = 2 } in
  ignore
    (expect_ok
       (Parser.parse ~limits:type_nesting_limits
          (source "fn f(value ptr[i64]) void { return }\n")));
  (match
     Parser.parse ~limits:type_nesting_limits
       (source "fn f(value ptr[ptr[i64]]) void { return }\n")
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "type nesting exceeds the configured limit")
  | Ok _ -> assert false);
  let unary_nesting_limits = { Limits.default with max_nesting = 3 } in
  ignore
    (expect_ok
       (Parser.parse ~limits:unary_nesting_limits
          (source "fn f(value bool) bool { return !value }\n")));
  (match
     Parser.parse ~limits:unary_nesting_limits
       (source "fn f(value bool) bool { return !!value }\n")
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "unary nesting exceeds the configured limit")
  | Ok _ -> assert false);
  let switch_nesting_limits = { Limits.default with max_nesting = 3 } in
  ignore
    (expect_ok
       (Parser.parse ~limits:switch_nesting_limits
          (source "fn f(value i64) void { switch value { default: { return } } }\n")));
  (match
     Parser.parse ~limits:switch_nesting_limits
       (source
          "fn f(value i64) void { switch value { default: switch value { default: \
           return } } }\n")
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "nesting exceeds the configured limit")
  | Ok _ -> assert false);
  (match Process.run [||] with Error _ -> () | Ok _ -> assert false);
  (match Process.run [| "/definitely/missing/fas-tool" |] with
  | Error _ -> ()
  | Ok _ -> assert false);
  let parity_text =
    "extern \"C\" { fn printf(fmt ptr[const u8], ...) i32 }\n\
     struct Pair { a i64 b i64 }\n\
     const K arr[2,u32] = { 1, 2 }\n\
     fn first(p ptr[Pair], x i64) i64 { defer { printf(\"d\") } if p != null { y Pair \
     = (Pair){x, 2}\n\
    \ return y.a } return x == 0 ? 3 : 4 }\n\
     fn second() i64 { printf(\"s\")\n\
    \ return zext[i64](K[1]) }\n"
  in
  let pp = expect_ok (Parser.parse (source parity_text)) in
  let ph = expect_ok (Sema.check pp) in
  assert (List.length ph.Hir.strings = 2);
  let pir = Ir.render (expect_ok (Lower.lower ph)) in
  List.iter
    (fun needle -> assert (contains pir needle))
    [
      "target datalayout";
      "%struct.Pair = type";
      "[2 x i32]";
      "...";
      "phi i64";
      "alloca i64";
    ];
  assert (not (contains pir "noalias"));
  assert (not (contains pir "align 16"));
  print_endline "frontend unit tests: ok"
