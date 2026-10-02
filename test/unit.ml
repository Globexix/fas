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
  assert (Limits.default.max_interned_string_bytes = 4_000_000);
  assert (Limits.default.max_rendered_ir_bytes = 4_000_000);
  assert (Limits.default.max_specializations = 10_000);
  assert (Limits.default.max_specialization_depth = 64);
  assert (Limits.default.max_aggregate_elements = 1_000_000);
  assert (Limits.default.max_object_alignment = 1_048_576);
  assert (Limits.default.max_object_size = 1_073_741_824);
  assert (Limits.default.max_use_files = 10_000);
  assert (Limits.default.max_use_bytes = 1_073_741_824);
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
  let pointer_declarations = [ ("Address", [ ("value", Hir.Addr) ], None) ] in
  let target32 = { Target_layout.current with pointer_size = 4; pointer_align = 4 } in
  let pointer64 =
    match
      Hir.compute_struct_cached (Hir.struct_layout_cache pointer_declarations) "Address"
    with
    | Ok definition -> definition
    | Error message -> failwith message
  in
  let pointer32 =
    match
      Hir.compute_struct_cached
        (Hir.struct_layout_cache ~target:target32 pointer_declarations)
        "Address"
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
          "object size exceeds budget max_object_size of 15 (profile 0.15)")
  | Ok _ -> assert false);
  let generic_object_size_program =
    expect_ok
      (Parser.parse
         (source
            "struct Box[T] { value T }\n\
             fn f() void { value Box[arr[16,u8]]\n\
            \ return }\n"))
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
          "object size exceeds budget max_object_size of 15 (profile 0.15)")
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
          "alignment exceeds budget max_object_alignment of 8 (profile 0.15)")
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
          "alignment exceeds budget max_object_alignment of 8 (profile 0.15)")
  | Ok _ -> assert false);
  let generic_alignment_program =
    expect_ok
      (Parser.parse
         (source
            "struct Box[T] { value T }\n\
             fn f() void { value Box[vec[16,u8]]\n\
            \ return }\n"))
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
          "alignment exceeds budget max_object_alignment of 8 (profile 0.15)")
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
             fn f() void { value Pair[arr[3,u8]]\n\
            \ return }\n"))
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
             fn f() void { value Wrapped\n\
            \ return }\n"))
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
      (Parser.parse
         (source "opaque Ctx\nfn bad(p handle[Ctx]) i64 { return p[u64] }\n"))
  in
  (match Sema.check opaque with
  | Ok _ -> assert false
  | Error diagnostics -> assert (diagnostics <> []));
  let generic =
    expect_ok
      (Parser.parse
         (source
            "fn id[N const usize](x u64) u64 { return x + bitcast[u64](N) }\n\
             fn test() u64 { return id[3](2) }\n"))
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
                   (3, Ir.I8, Ir.Global ("numbers", Ir.Pointer (Ir.Array (2, Ir.I8))), 1);
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
       [ call_caller (Ir.Load (0, Ir.I8, Ir.Global ("missing", Ir.Pointer Ir.I8), 1)) ]);
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
      globals = [];
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
  let golden_source = source "fn test() i64 { x i64 = 2\n return x + 3\n }\n" in
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
    expect_ok (Parser.parse (source "fn f(p addr) usize { return addr_bits(p) }\n"))
  in
  assert (List.length builtin.Ast.items = 1);
  let rendered_module =
    let parsed =
      expect_ok
        (Parser.parse
           (source "fn test() i64 { s addr = \"a\\n\\t\\\\\\\"z\\0\"\n return 0\n }\n"))
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
        { (ir_function []) with Ir.name = "wide"; Ir.params };
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
  let expect_budget_rejection module_ =
    match Ir.render_bounded ~budget:256 module_ with
    | Error message ->
        assert (message = "rendered LLVM text exceeds the configured limit of 256 bytes")
    | Ok _ -> assert false
  in
  let long_name_module =
    ir_module [ { (ir_function []) with Ir.name = String.make 1_000_000 'a' } ]
  in
  assert (Ir.validate long_name_module = Ok ());
  expect_budget_rejection long_name_module;
  let long_struct_module =
    ir_module
      ~structs:
        [ { Ir.name = String.make 1_000_000 's'; fields = []; tail_padding = 0 } ]
      [ ir_function [ ir_block 0 (Ir.Ret None) ] ]
  in
  assert (Ir.validate long_struct_module = Ok ());
  expect_budget_rejection long_struct_module;
  let long_plain_global =
    ir_module
      ~globals:
        [ Ir.String_global { name = ".str.0"; bytes = String.make 1_000_000 'A' } ]
      [ ir_function [ ir_block 0 (Ir.Ret None) ] ]
  in
  assert (Ir.validate long_plain_global = Ok ());
  expect_budget_rejection long_plain_global;
  let quoted_name_module = ir_module [ { (ir_function []) with Ir.name = "a\nb" } ] in
  assert (Ir.validate quoted_name_module = Ok ());
  assert (contains (Ir.render quoted_name_module) "@\"a\\nb\"");
  expect_budget_rejection
    (ir_module [ { (ir_function []) with Ir.name = String.make 1_000_000 '\n' } ]);
  let ast_source text = Source.create ~file:"ast.fas" ~text in
  let ast_expected = "fn f(a i64) i64 {\n  y i64 = a + 1\n  return y\n}\n" in
  let ast_program =
    expect_ok
      (Parser.parse (ast_source "fn f(a i64) i64 { y i64 = a + 1\n return y }\n"))
  in
  assert (Ast.render_program ast_program = ast_expected);
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
    expect_ok (Parser.parse (source "fn f() void { s addr = \"abc\"\n return }\n"))
  in
  expect_budget_error "string single" ~line:1 ~column:24
    ~message:
      "string literal bytes exceed budget max_interned_string_bytes of 2 (profile 0.15)"
    ~notes:[]
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
    expect_ok (Parser.parse (source "fn f() void { s addr = c\"abc\"\n return }\n"))
  in
  expect_budget_error "c-string single" ~line:1 ~column:24
    ~message:
      "string literal bytes exceed budget max_interned_string_bytes of 3 (profile 0.15)"
    ~notes:[]
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
         (source "fn f() void { a addr = \"abc\"\n b addr = \"abc\"\n return }\n"))
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
            "fn f() void { a addr = \"ab\"\n\
            \ return }\n\
             fn g() void { b addr = \"ab\"\n\
            \ c addr = \"cd\"\n\
            \ return }\n"))
  in
  expect_budget_error "cross-function cumulative" ~line:4 ~column:11
    ~message:
      "cumulative interned string bytes exceed budget max_interned_string_bytes of 3 \
       (profile 0.15)"
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
            "fn f() void { a addr = \"ab\"\n\
            \ return }\n\
             fn g() void { b addr = \"cde\"\n\
            \ return }\n"))
  in
  expect_budget_error "ordering cumulative" ~line:3 ~column:24
    ~message:
      "cumulative interned string bytes exceed budget max_interned_string_bytes of 4 \
       (profile 0.15)"
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
            "fn echo[T](v T) addr { return \"abc\" }\n\
             fn test() void { a addr = echo[u8](1)\n\
            \ b addr = echo[i64](2)\n\
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
            "fn big[N const u64](v addr) addr { return \"toolongstr\" }\n\
             fn test(v addr) void { d addr = big[1](v)\n\
            \ return }\n"))
  in
  expect_budget_error "legality single" ~line:1 ~column:43
    ~message:
      "string literal bytes exceed budget max_interned_string_bytes of 8 (profile 0.15)"
    ~notes:[ "while instantiating `big[1]` at test.fas:2:36" ]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 8 }
       legality_single_strings);
  let legality_cumulative_strings =
    expect_ok
      (Parser.parse
         (source
            "fn pair[N const u64](v addr) addr {\n\
            \ a addr = \"aa\"\n\
            \ b addr = \"bb\"\n\
            \ return v }\n\
             fn test(v addr) void { d addr = pair[1](v)\n\
            \ return }\n"))
  in
  expect_budget_error "legality cumulative" ~line:3 ~column:11
    ~message:
      "cumulative interned string bytes exceed budget max_interned_string_bytes of 3 \
       (profile 0.15)"
    ~notes:[ "while instantiating `pair[1]` at test.fas:5:37" ]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 3 }
       legality_cumulative_strings);
  expect_budget_error "fresh state failure repeat" ~line:4 ~column:11
    ~message:
      "cumulative interned string bytes exceed budget max_interned_string_bytes of 3 \
       (profile 0.15)"
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
  let cli = expect_cli (Cli.parse [| "fas"; "--emit-ir"; "-o"; "out"; "one.fas" |]) in
  assert (
    cli.Cli.emit = Cli.Ir && cli.output = "out" && cli.output_explicit
    && cli.input = "one.fas" && cli.link_inputs = []);
  let cli_links =
    expect_cli
      (Cli.parse [| "fas"; "-L"; "lib"; "prog.fas"; "helper.c"; "-lm"; "obj.o" |])
  in
  assert (
    cli_links.Cli.input = "prog.fas"
    && cli_links.link_inputs = [ "-L"; "lib"; "helper.c"; "-lm"; "obj.o" ]);
  let cli_c_flags =
    expect_cli
      (Cli.parse
         [|
           "fas";
           "-I";
           "include dir";
           "-Isecond";
           "-isystem";
           "system dir";
           "-isystemthird";
           "-D";
           "FEATURE=7";
           "-DOTHER=9";
           "prog.fas";
           "helper.c";
         |])
  in
  assert (
    cli_c_flags.Cli.c_flags
    = [
        "-I";
        "include dir";
        "-Isecond";
        "-isystem";
        "system dir";
        "-isystemthird";
        "-D";
        "FEATURE=7";
        "-DOTHER=9";
      ]
    && cli_c_flags.link_inputs = [ "helper.c" ]);
  (match Cli.parse [| "fas"; "--emit-llvm"; "prog.fas"; "helper.c" |] with
  | Error message ->
      assert (message = "C inputs and link flags require an executable output")
  | Ok _ -> assert false);
  (match Cli.parse [| "fas"; "one.fas"; "two.fas" |] with
  | Error message
    when message
         = "multiple input files are not supported; use \"path.fas\" for dependencies"
    ->
      ()
  | Error message -> failwith ("multiple input files: unexpected message " ^ message)
  | Ok _ -> failwith "multiple input files: expected rejection");
  let header = expect_cli (Cli.parse [| "fas"; "--emit-header"; "root.fas" |]) in
  assert (header.emit = Cli.Header && not header.output_explicit);
  List.iter
    (fun mode ->
      List.iter
        (fun args ->
          match Cli.parse (Array.of_list (("fas" :: args) @ [ "root.fas" ])) with
          | Error message ->
              assert (
                message = "--emit-header cannot be combined with other output modes")
          | Ok _ -> assert false)
        [ [ "--emit-header"; mode ]; [ mode; "--emit-header" ] ])
    [ "--emit-ir"; "--emit-llvm"; "-S"; "--emit-asm"; "-c"; "--emit-obj" ];
  (match Cli.parse [| "fas"; "--emit-header"; "root.fas"; "extra.c" |] with
  | Error message ->
      assert (message = "C inputs and link flags require an executable output")
  | Ok _ -> assert false);
  assert (contains Cli.usage "--emit-header");
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
  | Ok Cli.Help -> assert (contains Cli.usage "-I DIR, -isystem DIR, -D NAME[=VALUE]")
  | Ok (Cli.Run _) | Error _ -> assert false);
  let input_path = Filename.temp_file "fas-driver-" ".fas" in
  Fun.protect
    ~finally:(fun () -> Sys.remove input_path)
    (fun () ->
      let channel = open_out_bin input_path in
      output_string channel "fn test() i64 { return 3 }\n";
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
        [ "--emit-ir"; "--emit-llvm" ]);
  (match Cli.parse [| "fas"; "--emit-ast"; "one.fas" |] with
  | Error message -> assert (message = "unknown option: --emit-ast")
  | Ok _ -> assert false);
  (match Cli.parse [| "fas"; "-o"; "-"; "one.fas" |] with
  | Error message -> assert (message = "-o - is not supported")
  | Ok _ -> assert false);
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
          (source "fn f(value addr) void { return }\n")));
  (match
     Parser.parse ~limits:type_nesting_limits
       (source "fn f(value arr[2, arr[2, u8]]) void { return }\n")
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
    "extern \"C\" { fn printf(fmt addr, ...) i32 }\n\
     struct Pair { a i64 b i64 }\n\
     const K arr[2,u32] = { 1, 2 }\n\
     fn first(p addr, x i64) i64 { defer { printf(\"d\") } if p != addr_from_bits(0) { \
     y Pair = (Pair){x, 2}\n\
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
  let run_budget_tests () =
    assert (Limits.default.Limits.max_ast_nodes = 4_000_000);
    assert (Limits.default.Limits.max_ir_nodes = 4_000_000);
    assert (Limits.default.Limits.max_static_data_bytes = 1_073_741_824);
    assert (Limits.budget_profile_name Limits.default = "0.15");
    let expect_diag needles result =
      match result with
      | Ok _ -> assert false
      | Error diagnostics ->
          let rendered = Diag.render_all ~source:None diagnostics in
          List.iter (fun needle -> assert (contains rendered needle)) needles
    in
    let expect_ir_diag needles offender result =
      match result with
      | Ok () -> assert false
      | Error (actual, message) ->
          assert (actual = offender);
          List.iter (fun needle -> assert (contains message needle)) needles
    in
    let ast_unit_text = "fn f(a i64) i64 { x i64 = a + 1\n return x }\n" in
    let ast_program = expect_ok (Parser.parse (source ast_unit_text)) in
    let ast_nodes = Ast.count_expanded_nodes ast_program in
    assert (ast_nodes > 8);
    let ast_exact = { Limits.default with max_ast_nodes = ast_nodes } in
    let ast_over = { Limits.default with max_ast_nodes = ast_nodes - 1 } in
    assert (
      Ast.render_program
        (expect_ok (Parser.parse ~limits:ast_exact (source ast_unit_text)))
      = Ast.render_program ast_program);
    (match Parser.parse ~limits:ast_over (source ast_unit_text) with
    | Ok _ -> assert false
    | Error [ diagnostic ] ->
        assert (contains diagnostic.Diag.message "max_ast_nodes");
        assert (contains diagnostic.Diag.message (string_of_int (ast_nodes - 1)));
        assert (contains diagnostic.Diag.message "0.15");
        assert (diagnostic.Diag.primary.Span.file = "test.fas")
    | Error _ -> assert false);
    let ast_over_first =
      Parser.parse ~limits:ast_over (source ast_unit_text)
      |> Result.map_error (fun diagnostics -> Diag.render_all ~source:None diagnostics)
    in
    let ast_over_second =
      Parser.parse ~limits:ast_over (source ast_unit_text)
      |> Result.map_error (fun diagnostics -> Diag.render_all ~source:None diagnostics)
    in
    assert (ast_over_first = ast_over_second);
    ignore
      (expect_ok
         (Parser.parse
            ~limits:{ Limits.default with max_ast_nodes = max_int }
            (source ast_unit_text)));
    expect_diag
      [ "budget max_ast_nodes must not be negative"; "0.15" ]
      (Parser.parse
         ~limits:{ Limits.default with max_ast_nodes = min_int }
         (source ast_unit_text));
    ignore
      (expect_ok
         (Parser.parse ~limits:{ Limits.default with max_ast_nodes = 0 } (source "")));
    expect_diag [ "max_ast_nodes"; "0.15" ]
      (Parser.parse
         ~limits:{ Limits.default with max_ast_nodes = 0 }
         (source "opaque Q\n"));
    let many_functions =
      String.concat ""
        (List.init 12 (fun i -> Printf.sprintf "fn f%d() i64 { return %d }\n" i i))
    in
    let many_nodes =
      Ast.count_expanded_nodes (expect_ok (Parser.parse (source many_functions)))
    in
    let one_nodes =
      Ast.count_expanded_nodes
        (expect_ok (Parser.parse (source "fn f0() i64 { return 0 }\n")))
    in
    let many_over = { Limits.default with max_ast_nodes = many_nodes - 1 } in
    assert (one_nodes < many_nodes - 1);
    ignore
      (expect_ok (Parser.parse ~limits:many_over (source "fn f0() i64 { return 0 }\n")));
    expect_diag [ "max_ast_nodes"; "0.15" ]
      (Parser.parse ~limits:many_over (source many_functions));
    let unit_a =
      expect_ok
        (Parser.parse
           (Source.create ~file:"unit_a.fas" ~text:"fn a() i64 { return 1 }\n"))
    in
    let unit_b =
      expect_ok
        (Parser.parse
           (Source.create ~file:"unit_b.fas" ~text:"fn b() i64 { return 2 }\n"))
    in
    let combined = { Ast.items = unit_a.Ast.items @ unit_b.Ast.items } in
    let combined_nodes = Ast.count_expanded_nodes combined in
    assert (
      Ast.check_expanded_nodes
        ~limits:{ Limits.default with max_ast_nodes = combined_nodes }
        combined
      = Ok ());
    (match
       Ast.check_expanded_nodes
         ~limits:{ Limits.default with max_ast_nodes = combined_nodes - 1 }
         combined
     with
    | Ok () -> assert false
    | Error diagnostic ->
        assert (contains diagnostic.Diag.message "max_ast_nodes");
        assert (contains diagnostic.Diag.message "0.15");
        assert (diagnostic.Diag.primary.Span.file = "unit_b.fas"));
    assert (
      Ast.check_expanded_nodes
        ~limits:{ Limits.default with max_ast_nodes = 0 }
        { Ast.items = [] }
      = Ok ());
    (match Ast.item_span_by_name ast_program "f" with
    | Some span -> assert (span.Span.file = "test.fas")
    | None -> assert false);
    assert (Ast.item_span_by_name ast_program "missing" = None);
    let medium_functions =
      String.concat ""
        (List.init 8 (fun i ->
             Printf.sprintf
               "fn m%d(x i64) i64 { a i64 = x + %d\n\
               \ b i64 = a * 2\n\
               \ c i64 = b - x\n\
               \ return c }\n"
               i i))
    in
    let medium_ir =
      expect_ok
        (Lower.lower
           (expect_ok (Sema.check (expect_ok (Parser.parse (source medium_functions))))))
    in
    let rendered_before = Ir.render medium_ir in
    let medium_nodes = Ir.count_lowered_nodes medium_ir in
    assert (medium_nodes > 8);
    assert (
      Ir.check_lowered_nodes
        ~limits:{ Limits.default with max_ir_nodes = medium_nodes }
        medium_ir
      = Ok ());
    assert (Ir.render medium_ir = rendered_before);
    let per_function_nodes =
      List.map
        (fun f -> Ir.count_lowered_nodes { medium_ir with Ir.funcs = [ f ] })
        medium_ir.Ir.funcs
    in
    let ir_over = { Limits.default with max_ir_nodes = medium_nodes - 1 } in
    assert (List.for_all (fun n -> n < medium_nodes - 1) per_function_nodes);
    expect_ir_diag
      [ "max_ir_nodes"; string_of_int (medium_nodes - 1); "0.15"; "at function `m7`" ]
      (Some "m7")
      (Ir.check_lowered_nodes ~limits:ir_over medium_ir);
    let ir_over_first = Ir.check_lowered_nodes ~limits:ir_over medium_ir in
    let ir_over_second = Ir.check_lowered_nodes ~limits:ir_over medium_ir in
    assert (ir_over_first = ir_over_second);
    assert (
      Ir.check_lowered_nodes
        ~limits:{ Limits.default with max_ir_nodes = max_int }
        medium_ir
      = Ok ());
    expect_ir_diag
      [ "budget max_ir_nodes must not be negative"; "0.15" ]
      None
      (Ir.check_lowered_nodes
         ~limits:{ Limits.default with max_ir_nodes = min_int }
         medium_ir);
    let static_small =
      ir_module
        ~globals:
          [
            Ir.String_global { name = "s0"; bytes = "ok" };
            Ir.Array_global
              { name = "a0"; elem_ty = Ir.I32; elems = [ 1L; 2L; 3L; 4L ]; align = 4 };
          ]
        []
    in
    assert (
      Ir.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 18 }
        static_small
      = Ok ());
    assert (
      Ir.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = max_int }
        static_small
      = Ok ());
    expect_ir_diag
      [ "max_static_data_bytes"; "17"; "0.15"; "at global `a0`" ]
      (Some "a0")
      (Ir.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 17 }
         static_small);
    expect_ir_diag
      [ "budget max_static_data_bytes must not be negative"; "0.15" ]
      None
      (Ir.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = min_int }
         static_small);
    let six = String.make 6 's' in
    let static_cumulative =
      ir_module
        ~globals:
          [
            Ir.String_global { name = "g0"; bytes = six };
            Ir.String_global { name = "g1"; bytes = six };
            Ir.String_global { name = "g2"; bytes = six };
          ]
        []
    in
    assert (
      Ir.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 18 }
        static_cumulative
      = Ok ());
    expect_ir_diag
      [ "max_static_data_bytes"; "10"; "0.15"; "at global `g1`" ]
      (Some "g1")
      (Ir.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 10 }
         static_cumulative);
    let static_overflow =
      ir_module
        ~globals:
          [
            Ir.Array_global
              {
                name = "huge";
                elem_ty = Ir.Array (max_int, Ir.Array (max_int, Ir.I8));
                elems = [ 0L ];
                align = 1;
              };
          ]
        []
    in
    expect_ir_diag
      [ "max_static_data_bytes"; "0.15"; "at global `huge`" ]
      (Some "huge")
      (Ir.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = max_int }
         static_overflow);
    let static_overflow_pair =
      ir_module
        ~globals:
          [
            Ir.Array_global
              {
                name = "wide";
                elem_ty = Ir.Array (max_int, Ir.I8);
                elems = [ 0L; 0L ];
                align = 1;
              };
          ]
        []
    in
    expect_ir_diag
      [ "max_static_data_bytes"; "0.15"; "at global `wide`" ]
      (Some "wide")
      (Ir.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = max_int }
         static_overflow_pair);
    let static_over_first =
      Ir.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 17 }
        static_small
    in
    let static_over_second =
      Ir.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 17 }
        static_small
    in
    assert (static_over_first = static_over_second);
    let static_padded =
      ir_module
        ~structs:
          [
            {
              Ir.name = "P";
              fields = [ Ir.I8; Ir.Array (3, Ir.I8); Ir.I32; Ir.I16 ];
              tail_padding = 2;
            };
            {
              Ir.name = "Q";
              fields =
                [
                  Ir.Array (0, Ir.Vector (8, Ir.I8));
                  Ir.Struct "P";
                  Ir.Array (4, Ir.I8);
                  Ir.I64;
                ];
              tail_padding = 0;
            };
          ]
        ~globals:
          [
            Ir.Array_global
              {
                name = "padded";
                elem_ty = Ir.Struct "P";
                elems = [ 0L; 0L ];
                align = 4;
              };
            Ir.Array_global
              { name = "nested"; elem_ty = Ir.Struct "Q"; elems = [ 0L ]; align = 8 };
          ]
        []
    in
    assert (
      Ir.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 48 }
        static_padded
      = Ok ());
    expect_ir_diag
      [ "max_static_data_bytes"; "47"; "0.15"; "at global `nested`" ]
      (Some "nested")
      (Ir.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 47 }
         static_padded);
    let static_padded_first =
      Ir.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 47 }
        static_padded
    in
    let static_padded_second =
      Ir.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 47 }
        static_padded
    in
    assert (static_padded_first = static_padded_second)
  in
  run_budget_tests ();
  let run_type_node_budget_tests () =
    assert (Limits.default.Limits.max_type_nodes = 4_000_000);
    let expect_type_diag needles result =
      match result with
      | Ok _ -> assert false
      | Error diagnostics ->
          let rendered = Diag.render_all ~source:None diagnostics in
          List.iter (fun needle -> assert (contains rendered needle)) needles
    in
    let two_use_text =
      "fn two[T](x0 T, x1 T) i64 { return 0 }\n\
       fn use0(a addr) i64 { return two[addr](a, a) }\n"
    in
    let two_program = expect_ok (Parser.parse (source two_use_text)) in
    let exact_two =
      expect_ok
        (Sema.check ~limits:{ Limits.default with max_type_nodes = 3 } two_program)
    in
    assert (List.length exact_two.Hir.funcs = 2);
    assert (
      Ir.render (expect_ok (Lower.lower exact_two))
      = Ir.render (expect_ok (Lower.lower (expect_ok (Sema.check two_program)))));
    (match
       Sema.check
         ~limits:{ Limits.default with max_type_nodes = 2 }
         (expect_ok (Parser.parse (source two_use_text)))
     with
    | Ok _ -> assert false
    | Error [ diagnostic ] ->
        assert (contains diagnostic.Diag.message "max_type_nodes");
        assert (contains diagnostic.Diag.message "of 2 (profile 0.15)");
        assert (contains diagnostic.Diag.message "two");
        assert (diagnostic.Diag.primary.Span.file = "test.fas")
    | Error _ -> assert false);
    let two_over_first =
      Sema.check
        ~limits:{ Limits.default with max_type_nodes = 2 }
        (expect_ok (Parser.parse (source two_use_text)))
      |> Result.map_error (fun diagnostics -> Diag.render_all ~source:None diagnostics)
    in
    let two_over_second =
      Sema.check
        ~limits:{ Limits.default with max_type_nodes = 2 }
        (expect_ok (Parser.parse (source two_use_text)))
      |> Result.map_error (fun diagnostics -> Diag.render_all ~source:None diagnostics)
    in
    assert (two_over_first = two_over_second);
    ignore
      (expect_ok
         (Sema.check
            ~limits:{ Limits.default with max_type_nodes = max_int }
            (expect_ok (Parser.parse (source two_use_text)))));
    expect_type_diag
      [ "budget max_type_nodes must not be negative"; "0.15" ]
      (Sema.check
         ~limits:{ Limits.default with max_type_nodes = min_int }
         (expect_ok (Parser.parse (source two_use_text))));
    ignore
      (expect_ok
         (Sema.check
            ~limits:{ Limits.default with max_type_nodes = 0 }
            (expect_ok (Parser.parse (source "")))));
    expect_type_diag [ "max_type_nodes"; "0.15" ]
      (Sema.check
         ~limits:{ Limits.default with max_type_nodes = 0 }
         (expect_ok (Parser.parse (source two_use_text))));
    let cumulative_text =
      "fn two[T](x0 T, x1 T) i64 { return 0 }\n\
       fn use0(a addr, b usize) i64 { return two[addr](a, a) + two[usize](b, b) }\n"
    in
    let cumulative_program = expect_ok (Parser.parse (source cumulative_text)) in
    let exact_cumulative =
      expect_ok
        (Sema.check
           ~limits:{ Limits.default with max_type_nodes = 6 }
           cumulative_program)
    in
    assert (List.length exact_cumulative.Hir.funcs = 3);
    expect_type_diag
      [ "max_type_nodes"; "of 5 (profile 0.15)"; "test.fas" ]
      (Sema.check
         ~limits:{ Limits.default with max_type_nodes = 5 }
         (expect_ok (Parser.parse (source cumulative_text))));
    let box_text =
      "struct Box[T] { a T, b T }\nfn use0() i64 { x Box[addr]\n return 0 }\n"
    in
    let box_program = expect_ok (Parser.parse (source box_text)) in
    let exact_box =
      expect_ok
        (Sema.check ~limits:{ Limits.default with max_type_nodes = 2 } box_program)
    in
    assert (List.length exact_box.Hir.structs = 1);
    (match
       Sema.check
         ~limits:{ Limits.default with max_type_nodes = 1 }
         (expect_ok (Parser.parse (source box_text)))
     with
    | Ok _ -> assert false
    | Error [ diagnostic ] ->
        assert (contains diagnostic.Diag.message "struct specialization");
        assert (contains diagnostic.Diag.message "Box");
        assert (contains diagnostic.Diag.message "of 1 (profile 0.15)");
        assert (diagnostic.Diag.primary.Span.file = "test.fas")
    | Error _ -> assert false);
    let pick_text =
      "fn pick[N const i64](x i64) i64 { return x + N }\n\
       fn use0(a i64) i64 { return pick[1](a) }\n"
    in
    let pick_program = expect_ok (Parser.parse (source pick_text)) in
    let exact_pick =
      expect_ok
        (Sema.check ~limits:{ Limits.default with max_type_nodes = 3 } pick_program)
    in
    assert (List.length exact_pick.Hir.funcs = 2);
    expect_type_diag
      [ "function specialization"; "pick"; "of 2 (profile 0.15)"; "test.fas" ]
      (Sema.check
         ~limits:{ Limits.default with max_type_nodes = 2 }
         (expect_ok (Parser.parse (source pick_text))));
    let widen_text =
      "fn widen[T](x0 addr, x1 addr, x2 addr, x3 addr) i64 {\n\
       a T\n\
      \ b T\n\
      \ c T\n\
      \ d T\n\
      \ return 0 }\n\
       fn use0(a addr) i64 { return widen[arr[2,arr[2,u8]]](a, a, a, a) }\n\
       fn use1(b addr) i64 { return widen[arr[3,arr[2,u8]]](b, b, b, b) }\n"
    in
    let widen_program = expect_ok (Parser.parse (source widen_text)) in
    let exact_widen =
      expect_ok
        (Sema.check ~limits:{ Limits.default with max_type_nodes = 34 } widen_program)
    in
    assert (List.length exact_widen.Hir.funcs = 4);
    expect_type_diag
      [ "max_type_nodes"; "of 33 (profile 0.15)"; "widen" ]
      (Sema.check
         ~limits:{ Limits.default with max_type_nodes = 33 }
         (expect_ok (Parser.parse (source widen_text))));
    let widen_over_first =
      Sema.check
        ~limits:{ Limits.default with max_type_nodes = 33 }
        (expect_ok (Parser.parse (source widen_text)))
      |> Result.map_error (fun diagnostics -> Diag.render_all ~source:None diagnostics)
    in
    let widen_over_second =
      Sema.check
        ~limits:{ Limits.default with max_type_nodes = 33 }
        (expect_ok (Parser.parse (source widen_text)))
      |> Result.map_error (fun diagnostics -> Diag.render_all ~source:None diagnostics)
    in
    assert (widen_over_first = widen_over_second)
  in
  run_type_node_budget_tests ();
  let run_diagnostic_naming_tests () =
    let expect_naming needles ~limits text =
      match Sema.check ~limits (expect_ok (Parser.parse (source text))) with
      | Ok _ -> assert false
      | Error diagnostics ->
          let rendered = Diag.render_all ~source:None diagnostics in
          List.iter (fun needle -> assert (contains rendered needle)) needles
    in
    expect_naming
      [
        "string literal bytes exceed budget max_interned_string_bytes of 2 (profile \
         0.15)";
      ]
      ~limits:{ Limits.default with max_interned_string_bytes = 2 }
      "fn f() void { s addr = \"abc\"\n return }\n";
    expect_naming
      [
        "cumulative interned string bytes exceed budget max_interned_string_bytes of 3 \
         (profile 0.15)";
      ]
      ~limits:{ Limits.default with max_interned_string_bytes = 3 }
      "fn f() void { a addr = \"ab\"\n b addr = \"cd\"\n return }\n";
    expect_naming
      [ "object size exceeds budget max_object_size of 15 (profile 0.15)" ]
      ~limits:{ Limits.default with max_object_size = 15 }
      "fn f() void { value arr[16,u8]\n return }\n";
    expect_naming
      [ "alignment exceeds budget max_object_alignment of 8 (profile 0.15)" ]
      ~limits:{ Limits.default with max_object_alignment = 8 }
      "struct Aligned @align(16) { value u8 }\nfn f() void { return }\n";
    expect_naming
      [
        "aggregate element count exceeds the configured limit";
        "budget max_aggregate_elements of 99 (profile 0.15)";
      ]
      ~limits:{ Limits.default with max_aggregate_elements = 99 }
      "fn f() void { value arr[100,u8]\n return }\n";
    expect_naming
      [
        "const specialization count limit exceeded";
        "budget max_specializations of 1 (profile 0.15)";
      ]
      ~limits:{ Limits.default with max_specializations = 1 }
      "fn id[N const i64]() i64 { return N }\n\
       fn a() i64 { return id[1]() }\n\
       fn b() i64 { return id[2]() }\n";
    expect_naming
      [
        "const specialization recursion depth limit exceeded";
        "budget max_specialization_depth of 1 (profile 0.15)";
      ]
      ~limits:{ Limits.default with max_specialization_depth = 1 }
      "fn inner[N const i64]() i64 { return N }\n\
       fn outer[M const i64]() i64 { return inner[M]() }\n\
       fn test() i64 { return outer[5]() }\n";
    expect_naming
      [
        "function specialization count limit exceeded";
        "budget max_specializations of 1 (profile 0.15)";
      ]
      ~limits:{ Limits.default with max_specializations = 1 }
      "fn identity[T](value T) T { return value }\n\
       fn test() i64 { a u8 = identity[u8](1)\n\
      \ return identity[i64](1) }\n";
    expect_naming
      [
        "struct specialization recursion depth limit exceeded";
        "budget max_specialization_depth of 1 (profile 0.15)";
      ]
      ~limits:{ Limits.default with max_specialization_depth = 1 }
      "struct Inner[T] { value T }\n\
       struct Outer[T] { inner Inner[T] }\n\
       fn test() i64 { value Outer[u8]\n\
      \ return 0 }\n"
  in
  let run_specialization_span_tests () =
    let program =
      expect_ok
        (Parser.parse
           (Source.create ~file:"spans.fas"
              ~text:
                "fn first() u64 { return 1 }\n\
                 fn second[T](value T) T { return value }\n\
                 fn test() i64 { return first() }\n"))
    in
    let first_span, template_span =
      match program.Ast.items with
      | [ Ast.Func { span = first; _ }; Ast.Func { span = template; _ }; _ ] ->
          (first, template)
      | _ -> failwith "unexpected span-mapping program shape"
    in
    assert (Ast.item_span_by_name program "first" = Some first_span);
    assert (Ast.item_span_by_name program "second" = Some template_span);
    assert (Ast.item_span_by_name program "missing" = None);
    let type_mangled =
      Sema_specialization.mangle_type_specialization "second" [ Ast.Bool ]
    in
    assert (Ast.item_span_by_name program type_mangled = Some template_span);
    assert (
      Ast.item_span_by_name program ("second" ^ Ast.specialization_name_delimiter)
      = Some template_span);
    assert (
      Ast.item_span_by_name program
        ("missing" ^ Ast.specialization_name_delimiter ^ "4_bool")
      = None);
    assert (
      Ast.item_span_by_name program
        ("first" ^ Ast.specialization_name_delimiter ^ "named6_nope$spec$4_bool")
      = Some first_span)
  in
  let run_debug_ir_budget_tests () =
    let empty = ir_module [] in
    let expected_empty =
      Printf.sprintf
        "Module {\n\
        \  target_triple = %S;\n\
        \  data_layout = %S;\n\
        \  no_inline_function = None;\n\
        \  globals = [\n\
        \  ];\n\
        \  functions = [\n\
        \  ];\n\
         }\n"
        empty.Ir.target_triple empty.Ir.data_layout
    in
    (match Ir.render_debug_bounded ~limits:Limits.default empty with
    | Ok text -> assert (text = expected_empty)
    | Error _ -> assert false);
    let small =
      ir_module
        ~globals:[ Ir.String_global { name = "s"; bytes = "abc" } ]
        [ ir_function [ ir_block 0 (Ir.Ret None) ] ]
    in
    let full =
      match
        Ir.render_debug_bounded
          ~limits:{ Limits.default with max_rendered_ir_bytes = max_int }
          small
      with
      | Ok text -> text
      | Error _ -> assert false
    in
    let length = String.length full in
    (match
       Ir.render_debug_bounded
         ~limits:{ Limits.default with max_rendered_ir_bytes = length }
         small
     with
    | Ok text -> assert (text = full)
    | Error _ -> assert false);
    (match
       Ir.render_debug_bounded
         ~limits:{ Limits.default with max_rendered_ir_bytes = length - 1 }
         small
     with
    | Error message ->
        assert (
          contains message
            "rendered debug IR bytes exceed budget max_rendered_ir_bytes of");
        assert (contains message "(profile 0.15)")
    | Ok _ -> assert false);
    (match
       Ir.render_debug_bounded
         ~limits:{ Limits.default with max_rendered_ir_bytes = min_int }
         small
     with
    | Error message ->
        assert (contains message "budget max_rendered_ir_bytes must not be negative")
    | Ok _ -> assert false);
    let over =
      match
        Ir.render_debug_bounded
          ~limits:{ Limits.default with max_rendered_ir_bytes = length - 1 }
          small
      with
      | Error message -> message
      | Ok _ -> assert false
    in
    match
      Ir.render_debug_bounded
        ~limits:{ Limits.default with max_rendered_ir_bytes = length - 1 }
        small
    with
    | Error message -> assert (message = over)
    | Ok _ -> assert false
  in
  let run_scratch_budget_tests () =
    let one =
      ir_module
        [
          ir_function
            [
              ir_block ~instrs:[ Ir.Alloca (0, Ir.I32, 4) ] 0 (Ir.Br 1);
              ir_block ~instrs:[ Ir.Alloca (1, Ir.I64, 8) ] 1 (Ir.Ret None);
            ];
        ]
    in
    assert (
      Ir.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = 12 }
        one
      = Ok ());
    assert (
      Ir.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = max_int }
        one
      = Ok ());
    assert (
      Ir.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = 0 }
        (ir_module [])
      = Ok ());
    (match
       Ir.check_stack_scratch_bytes
         ~limits:{ Limits.default with max_stack_scratch_bytes = 11 }
         one
     with
    | Error (offender, message) ->
        assert (offender = Some "control_flow");
        assert (
          contains message
            "cumulative alloca scratch bytes exceed budget max_stack_scratch_bytes of \
             11");
        assert (contains message "(profile 0.15)");
        assert (contains message "at function `control_flow`")
    | Ok () -> assert false);
    (match
       Ir.check_stack_scratch_bytes
         ~limits:{ Limits.default with max_stack_scratch_bytes = min_int }
         one
     with
    | Error (offender, message) ->
        assert (offender = None);
        assert (contains message "budget max_stack_scratch_bytes must not be negative")
    | Ok () -> assert false);
    let overflow =
      ir_module
        [
          ir_function
            [
              ir_block
                ~instrs:
                  [ Ir.Alloca (0, Ir.Array (max_int, Ir.Array (max_int, Ir.I8)), 1) ]
                0 (Ir.Ret None);
            ];
        ]
    in
    (match
       Ir.check_stack_scratch_bytes
         ~limits:{ Limits.default with max_stack_scratch_bytes = max_int }
         overflow
     with
    | Error (offender, message) ->
        assert (offender = Some "control_flow");
        assert (
          contains message
            "cumulative alloca scratch bytes exceed budget max_stack_scratch_bytes")
    | Ok () -> assert false);
    let over_first =
      Ir.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = 11 }
        one
    in
    let over_second =
      Ir.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = 11 }
        one
    in
    assert (over_first = over_second)
  in
  let run_vector_size_tests () =
    let check_scratch budget slot_ty =
      Ir.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = budget }
        (ir_module
           [
             ir_function
               [ ir_block ~instrs:[ Ir.Alloca (0, slot_ty, 1) ] 0 (Ir.Ret None) ];
           ])
    in
    assert (check_scratch 8 (Ir.Vector (3, Ir.I16)) = Ok ());
    assert (check_scratch 1 (Ir.Vector (8, Ir.I1)) = Ok ());
    assert (check_scratch 16 (Ir.Vector (3, Ir.I32)) = Ok ());
    assert (check_scratch 4 (Ir.Vector (3, Ir.I8)) = Ok ());
    assert (check_scratch 8 (Ir.Array (2, Ir.Vector (3, Ir.I8))) = Ok ());
    let over budget slot_ty =
      match check_scratch budget slot_ty with
      | Error (offender, message) ->
          assert (offender = Some "control_flow");
          assert (
            contains message
              "cumulative alloca scratch bytes exceed budget max_stack_scratch_bytes")
      | Ok () -> assert false
    in
    over 7 (Ir.Vector (3, Ir.I16));
    over 0 (Ir.Vector (8, Ir.I1));
    over 15 (Ir.Vector (3, Ir.I32));
    over 3 (Ir.Vector (3, Ir.I8));
    over 7 (Ir.Array (2, Ir.Vector (3, Ir.I8)));
    let vec_global elem_ty =
      ir_module
        ~globals:[ Ir.Array_global { name = "vg"; elem_ty; elems = [ 0L ]; align = 4 } ]
        []
    in
    assert (
      Ir.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 4 }
        (vec_global (Ir.Vector (3, Ir.I8)))
      = Ok ());
    (match
       Ir.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 3 }
         (vec_global (Ir.Vector (3, Ir.I8)))
     with
    | Error (offender, _) -> assert (offender = Some "vg")
    | Ok () -> assert false);
    let wrapped =
      ir_module
        ~structs:
          [
            {
              Ir.name = "W";
              fields = [ Ir.Array (2, Ir.Vector (3, Ir.I8)) ];
              tail_padding = 0;
            };
          ]
        ~globals:
          [
            Ir.Array_global
              { name = "w"; elem_ty = Ir.Struct "W"; elems = [ 0L ]; align = 4 };
          ]
        []
    in
    assert (
      Ir.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 8 }
        wrapped
      = Ok ());
    (match
       Ir.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 7 }
         wrapped
     with
    | Error (offender, _) -> assert (offender = Some "w")
    | Ok () -> assert false);
    let source_program =
      expect_ok
        (Parser.parse
           (Source.create ~file:"vec_size.fas"
              ~text:"fn f(x vec[3,u16]) void { return }\n"))
    in
    let source_hir = expect_ok (Sema.check source_program) in
    let source_ir = expect_ok (Lower.lower source_hir) in
    assert (
      Ir.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = 8 }
        source_ir
      = Ok ());
    match
      Ir.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = 7 }
        source_ir
    with
    | Error (offender, message) ->
        assert (offender = Some "f");
        assert (
          contains message
            "cumulative alloca scratch bytes exceed budget max_stack_scratch_bytes")
    | Ok () -> assert false
  in
  let run_layout_accounting_parity_tests () =
    let hir_size ty =
      match Hir.layout [] ty with
      | Ok (size, _) -> size
      | Error message -> failwith message
    in
    let assert_pair ir_ty hir_ty =
      match Ir.static_type_bytes [] [] ir_ty with
      | Ok bytes -> assert (bytes = hir_size hir_ty)
      | Error () -> assert false
    in
    let elems =
      [
        (Ir.I1, Hir.Bool);
        (Ir.I8, Hir.Int Hir.U8);
        (Ir.I16, Hir.Int Hir.U16);
        (Ir.I32, Hir.Int Hir.U32);
        (Ir.I64, Hir.Int Hir.U64);
      ]
    in
    List.iter (fun (ir_e, hir_e) -> assert_pair ir_e hir_e) elems;
    assert_pair (Ir.Pointer Ir.I8) Hir.Addr;
    List.iter
      (fun lanes ->
        List.iter
          (fun (ir_e, hir_e) ->
            assert_pair (Ir.Vector (lanes, ir_e)) (Hir.Vec (lanes, hir_e)))
          elems)
      [ 1; 2; 3; 4; 5; 7; 8; 9; 16; 31; 64 ];
    List.iter
      (fun count ->
        List.iter
          (fun lanes ->
            List.iter
              (fun (ir_e, hir_e) ->
                assert_pair
                  (Ir.Array (count, Ir.Vector (lanes, ir_e)))
                  (Hir.Array (count, Hir.Vec (lanes, hir_e))))
              [ (Ir.I1, Hir.Bool); (Ir.I8, Hir.Int Hir.U8); (Ir.I16, Hir.Int Hir.U16) ])
          [ 1; 3; 8 ])
      [ 0; 1; 2; 3 ];
    assert (Ir.static_type_bytes [] [] (Ir.Vector (2, Ir.Array (2, Ir.I8))) = Error ());
    (match Hir.layout [] (Hir.Vec (2, Hir.Array (2, Hir.Int Hir.U8))) with
    | Error _ -> ()
    | Ok _ -> assert false);
    assert (Ir.static_type_bytes [] [] (Ir.Vector (2, Ir.Void)) = Error ());
    (match Hir.layout [] (Hir.Vec (2, Hir.Void)) with
    | Error _ -> ()
    | Ok _ -> assert false);
    assert (Ir.static_type_bytes [] [] (Ir.Vector (2, Ir.Struct "s")) = Error ());
    (match Hir.layout [] (Hir.Vec (2, Hir.Struct "s")) with
    | Error _ -> ()
    | Ok _ -> assert false);
    assert (Ir.static_type_bytes [] [] (Ir.Vector (2, Ir.Vector (2, Ir.I8))) = Error ());
    match Hir.layout [] (Hir.Vec (2, Hir.Vec (2, Hir.Int Hir.U8))) with
    | Error _ -> ()
    | Ok _ -> assert false
  in
  let run_top_level_order_tests () =
    let source_program =
      expect_ok
        (Parser.parse
           (Source.create ~file:"top_level_order.fas"
              ~text:
                "const a u32 = 1\n\
                 const b u32 = 2\n\
                 const g0 arr[2,u8] = { 1, 2 }\n\
                 const c u32 = 3\n\
                 const g1 arr[2,u8] = { 3, 4 }\n\
                 const d u32 = 4\n\
                 const w0 i64 = 7\n\
                 const v0 vec[2,i32] = bitcast[vec[2,i32]](w0)\n\
                 const l0 i64 = bitcast[i64](v0)\n\
                 const w1 i64 = 9\n\
                 const v1 vec[2,i32] = bitcast[vec[2,i32]](w1)\n\
                 const l1 i64 = bitcast[i64](v1)\n\
                 fn f() void { return }\n"))
    in
    let hir = expect_ok (Sema.check source_program) in
    assert (
      List.map (fun (c : Hir.const_def) -> c.Hir.name) hir.Hir.consts
      = [ "a"; "b"; "c"; "d"; "w0"; "w1"; "l0"; "l1" ]);
    assert (
      List.map (fun (c : Hir.const_arr_def) -> c.Hir.name) hir.Hir.const_arrays
      = [ "g0"; "g1" ])
  in
  let run_phase_invariant_tests () =
    let ok = function Ok () -> () | Error message -> failwith message in
    let rejected needle = function
      | Ok () -> failwith ("phase invariant accepted invalid input: " ^ needle)
      | Error message ->
          if not (contains message needle) then
            failwith ("phase invariant wrong diagnostic: " ^ message)
    in
    ok (Sema_invariants.check_declarations [ (0, "a"); (1, "b"); (2, "c") ]);
    rejected "not dense" (Sema_invariants.check_declarations [ (0, "a"); (0, "b") ]);
    rejected "collected twice"
      (Sema_invariants.check_declarations [ (0, "a"); (1, "a") ]);
    ok
      (Sema_invariants.check_const_environment ~declared:[ "a"; "g0"; "b"; "g1" ]
         ~early:[ "a"; "b" ] ~consts:[ "a"; "b" ] ~arrays:[ "g0"; "g1" ]);
    ok
      (Sema_invariants.check_const_environment ~declared:[ "a"; "g0"; "c" ]
         ~early:[ "a" ] ~consts:[ "a"; "c" ] ~arrays:[ "g0" ]);
    rejected "do not match declared"
      (Sema_invariants.check_const_environment ~declared:[ "a"; "b" ] ~early:[ "a" ]
         ~consts:[ "a" ] ~arrays:[]);
    rejected "emitted twice"
      (Sema_invariants.check_const_environment ~declared:[ "a" ] ~early:[ "a" ]
         ~consts:[ "a"; "a" ] ~arrays:[]);
    rejected "emitted twice"
      (Sema_invariants.check_const_environment ~declared:[ "g0" ] ~early:[] ~consts:[]
         ~arrays:[ "g0"; "g0" ]);
    rejected "do not match declared"
      (Sema_invariants.check_const_environment ~declared:[ "a" ] ~early:[ "a" ]
         ~consts:[ "a" ] ~arrays:[ "a" ]);
    rejected "array and vector constants are not in declaration order"
      (Sema_invariants.check_const_environment ~declared:[ "g0"; "g1" ] ~early:[]
         ~consts:[] ~arrays:[ "g1"; "g0" ]);
    rejected "early-resolved set"
      (Sema_invariants.check_const_environment ~declared:[ "a"; "b" ] ~early:[ "b" ]
         ~consts:[ "a"; "b" ] ~arrays:[]);
    rejected "early-resolved scalar constants are not in declaration order"
      (Sema_invariants.check_const_environment ~declared:[ "a"; "b" ]
         ~early:[ "b"; "a" ] ~consts:[ "b"; "a" ] ~arrays:[]);
    rejected "late scalar constants are not in declaration order"
      (Sema_invariants.check_const_environment ~declared:[ "a"; "x"; "y" ]
         ~early:[ "a" ] ~consts:[ "a"; "y"; "x" ] ~arrays:[]);
    ok (Sema_invariants.check_materialization ~pending:0 ~functions:[ "f"; "g" ]);
    rejected "remain pending"
      (Sema_invariants.check_materialization ~pending:2 ~functions:[]);
    rejected "emitted twice"
      (Sema_invariants.check_materialization ~pending:0 ~functions:[ "f"; "f" ])
  in
  run_diagnostic_naming_tests ();
  run_specialization_span_tests ();
  run_debug_ir_budget_tests ();
  run_scratch_budget_tests ();
  run_vector_size_tests ();
  run_layout_accounting_parity_tests ();
  let run_cross_budget_tests () =
    let one_diag name outcome =
      match outcome with
      | Error [ diagnostic ] -> diagnostic.Diag.message
      | Error diagnostics ->
          failwith
            (name ^ ": actual queue: "
            ^ String.concat " | " (List.map (fun d -> d.Diag.message) diagnostics))
      | Ok _ -> failwith (name ^ ": accepted")
    in
    let fails_alone name budget_needle limits text =
      match Sema.check ~limits (expect_ok (Parser.parse (source text))) with
      | Error diagnostics ->
          let rendered = Diag.render_all ~source:None diagnostics in
          if not (contains rendered budget_needle) then
            failwith (name ^ ": single budget " ^ budget_needle ^ " did not fire")
      | Ok _ -> failwith (name ^ ": single budget " ^ budget_needle ^ " accepted")
    in
    let pair name (limits_a, needle_a) (limits_b, needle_b) limits_both winner text =
      fails_alone name needle_a limits_a text;
      fails_alone name needle_b limits_b text;
      let run () =
        one_diag name
          (Sema.check ~limits:limits_both (expect_ok (Parser.parse (source text))))
      in
      let message = run () in
      let again = run () in
      let third = run () in
      if again <> message || third <> message then
        failwith (name ^ ": nondeterministic budget winner");
      if message <> winner then failwith (name ^ ": winner drifted: " ^ message)
    in
    pair "aggregate-vs-object"
      ({ Limits.default with max_aggregate_elements = 50 }, "max_aggregate_elements")
      ({ Limits.default with max_object_size = 8 }, "max_object_size")
      { Limits.default with max_aggregate_elements = 50; max_object_size = 8 }
      "object size exceeds budget max_object_size of 8 (profile 0.15)"
      "struct S { big arr[100,u8] }\nfn f() void { s S\n return }\n";
    pair "strings-vs-aggregate"
      ( { Limits.default with max_interned_string_bytes = 4 },
        "max_interned_string_bytes" )
      ({ Limits.default with max_aggregate_elements = 50 }, "max_aggregate_elements")
      { Limits.default with max_interned_string_bytes = 4; max_aggregate_elements = 50 }
      "aggregate element count exceeds the configured limit: budget \
       max_aggregate_elements of 50 (profile 0.15)"
      "struct S { big arr[100,u8] }\n\
       fn f() void { s S\n\
       q addr = \"hello world\"\n\
      \ return }\n";
    pair "typenodes-vs-specializations"
      ({ Limits.default with max_type_nodes = 2 }, "max_type_nodes")
      ({ Limits.default with max_specializations = 0 }, "max_specializations")
      { Limits.default with max_type_nodes = 2; max_specializations = 0 }
      "function specialization count limit exceeded: budget max_specializations of 0 \
       (profile 0.15)"
      "fn two[T](x0 T, x1 T) i64 { return 0 }\n\
       fn use0(a addr) i64 { return two[addr](a, a) }\n";
    let sd_text = "const A arr[4,u8] = { 1, 2, 3, 4 }\nfn f() void { return }\n" in
    let sd_program = expect_ok (Parser.parse (source sd_text)) in
    (match
       Ir.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 3 }
         (expect_ok (Lower.lower (expect_ok (Sema.check sd_program))))
     with
    | Ok () ->
        failwith "staticdata-vs-object: single budget max_static_data_bytes accepted"
    | Error _ -> ());
    (match
       Sema.check
         ~limits:{ Limits.default with max_object_size = 2 }
         (expect_ok (Parser.parse (source sd_text)))
     with
    | Ok _ -> failwith "staticdata-vs-object: single budget max_object_size accepted"
    | Error _ -> ());
    let sd_run () =
      one_diag "staticdata-vs-object"
        (Sema.check
           ~limits:
             { Limits.default with max_static_data_bytes = 3; max_object_size = 2 }
           (expect_ok (Parser.parse (source sd_text))))
    in
    let sd_message = sd_run () in
    let sd_again = sd_run () in
    let sd_third = sd_run () in
    if sd_again <> sd_message || sd_third <> sd_message then
      failwith "staticdata-vs-object: nondeterministic budget winner";
    if sd_message <> "object size exceeds budget max_object_size of 2 (profile 0.15)"
    then failwith ("staticdata-vs-object: winner drifted: " ^ sd_message);
    let global_data_program =
      expect_ok (Parser.parse (source "var Zero u32\nvar Initialized u32 = 0\n"))
    in
    let global_data_ir =
      expect_ok (Sema.check global_data_program) |> Lower.lower |> expect_ok
    in
    assert (
      Ir.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 4 }
        global_data_ir
      = Ok ());
    (match
       Ir.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 3 }
         global_data_ir
     with
    | Error (Some "Initialized", _) -> ()
    | Error (offender, _) ->
        failwith
          ("initialized global budget reported the wrong object: "
          ^ Option.value offender ~default:"none")
    | Ok () -> failwith "initialized global was omitted from static-data budget");
    let address_data_program =
      expect_ok
        (Parser.parse
           (source "var Target i32\nconst Table arr[2,addr] = {&Target,&Target}\n"))
    in
    let address_data_ir =
      expect_ok (Sema.check address_data_program) |> Lower.lower |> expect_ok
    in
    assert (
      Ir.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 16 }
        address_data_ir
      = Ok ());
    (match
       Ir.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 15 }
         address_data_ir
     with
    | Error (Some "Table", _) -> ()
    | _ -> failwith "address slots must each count eight static-data bytes");
    pair "specializations-vs-aggregate"
      ({ Limits.default with max_specializations = 0 }, "max_specializations")
      ({ Limits.default with max_aggregate_elements = 50 }, "max_aggregate_elements")
      { Limits.default with max_specializations = 0; max_aggregate_elements = 50 }
      "struct specialization count limit exceeded: budget max_specializations of 0 \
       (profile 0.15)"
      "struct Box[T] { a T, b T }\n\
       struct S { big arr[100,u8] }\n\
       fn use0() i64 { x Box[addr]\n\
      \ return 0 }\n\
       fn f() void { s S\n\
      \ return }\n";
    pair "strings-vs-typenodes"
      ( { Limits.default with max_interned_string_bytes = 4 },
        "max_interned_string_bytes" )
      ({ Limits.default with max_type_nodes = 2 }, "max_type_nodes")
      { Limits.default with max_interned_string_bytes = 4; max_type_nodes = 2 }
      "cumulative expanded type nodes exceed budget max_type_nodes of 2 (profile 0.15) \
       at function specialization `two$spec$4_addr`"
      "fn two[T](x0 T, x1 T) i64 { return 0 }\n\
       fn use0(a addr, s addr) i64 { q addr = \"hello world\"\n\
       return two[addr](a, a) }\n"
  in
  run_top_level_order_tests ();
  run_cross_budget_tests ();
  run_phase_invariant_tests ();
  print_endline "frontend unit tests: ok"

let () =
  assert (C_exports.guard "checksum.h" = "CHECKSUM");
  assert (C_exports.guard "/tmp/path/my-api.h" = "MY_API");
  assert (C_exports.guard "checksum" = "CHECKSUM")
