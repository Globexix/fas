let source text = Source.create ~file:"test.fas" ~text

let expect_ok = function
  | Ok value -> value
  | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)

let contains text needle =
  let n = String.length text and m = String.length needle in
  let rec go i = i + m <= n && (String.sub text i m = needle || go (i + 1)) in
  m = 0 || go 0

let () =
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
            "fn id[N const usize](x u64) u64 { return x + N }\n\
             fn main() u64 { return id[3](2) }\n"))
  in
  let specialized = expect_ok (Sema.check generic) in
  assert (List.length specialized.Hir.funcs = 2);
  let lowered = expect_ok (Lower.lower specialized) in
  assert (List.length lowered.Ir.funcs = 2);
  assert (Ir.render lowered = Ir.render lowered);
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
     asm fn raw(x i64) i64 // comment containing {\n\
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
  let expect_cli = function Ok value -> value | Error message -> failwith message in
  let cli =
    expect_cli
      (Cli.parse
         [| "fas"; "-release"; "--emit-ast"; "-o"; "out"; "one.fas"; "two.fas" |])
  in
  assert (
    cli.Cli.emit = Cli.Ast && cli.output = "out"
    && cli.inputs = [ "one.fas"; "two.fas" ]);
  let cli_obj = expect_cli (Cli.parse [| "fas"; "-c"; "path/prog.fas" |]) in
  assert (cli_obj.Cli.emit = Cli.Obj && cli_obj.output = "prog.o");
  let cli_asm = expect_cli (Cli.parse [| "fas"; "-S"; "path/prog.fas" |]) in
  assert (cli_asm.Cli.emit = Cli.Asm && cli_asm.output = "prog.s");
  let cli_named_obj =
    expect_cli (Cli.parse [| "fas"; "-c"; "-o"; "artifact"; "prog.fas" |])
  in
  assert (cli_named_obj.Cli.emit = Cli.Obj && cli_named_obj.output = "artifact");
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
  sema_error "fn f() i64 { x i64 = 1\n { x i64 = 2 } return x }\n";
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
  assert (contains edge_llvm "select i1");
  assert (contains edge_llvm "@llvm.fshl.v4i32");
  assert (contains edge_llvm "declare <4 x i32> @llvm.fshl.v4i32");
  assert (contains edge_llvm "zext i1 true to i64");
  let edge_debug = Ir.render_debug edge_ir in
  assert (contains edge_debug "Module {");
  assert (edge_debug <> edge_llvm);
  let token_limits = { Limits.default with max_tokens = 1 } in
  (match Lexer.lex ~limits:token_limits (source "x y") with
  | Error diagnostics ->
      assert (contains (Diag.render_all ~source:None diagnostics) "token limit exceeded")
  | Ok _ -> assert false);
  (match Process.run [||] with Error _ -> () | Ok _ -> assert false);
  (match Process.run [| "/definitely/missing/fas-tool" |] with
  | Error _ -> ()
  | Ok _ -> assert false);
  let parity_text =
    "extern \"C\" { fn printf(fmt ptr[u8], ...) i32 }\n\
     struct Pair { a i64 b i64 }\n\
     const K arr[2,u32] = { 1, 2 }\n\
     fn first(p noalias aligned[16] ptr[Pair], x i64) i64 { defer { printf(\"d\") } if \
     p != null { y Pair = (Pair){x, 2}\n\
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
      "noalias noundef align 16";
      "phi i64";
      "alloca i64";
    ];
  print_endline "frontend unit tests: ok"
