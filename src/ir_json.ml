open Ir

let str s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | c when Char.code c < 0x20 || Char.code c >= 0x7f ->
          Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let hex s =
  str
    (String.concat ""
       (List.map (Printf.sprintf "%02x")
          (List.map Char.code (List.of_seq (String.to_seq s)))))

let arr items = "[" ^ String.concat "," items ^ "]"

let obj fields =
  "{" ^ String.concat "," (List.map (fun (k, v) -> str k ^ ":" ^ v) fields) ^ "}"

let int n = string_of_int n
let i64 n = Int64.to_string n
let ty t = str (ty_name t)

let value = function
  | Const (t, n) -> arr [ str "const"; ty t; i64 n ]
  | Const_vector (t, ns) -> arr [ str "vconst"; ty t; arr (List.map i64 ns) ]
  | Null t -> arr [ str "null"; ty t ]
  | Undef t -> arr [ str "undef"; ty t ]
  | Zero t -> arr [ str "zero"; ty t ]
  | Local (n, t) -> arr [ str "local"; ty t; int n ]
  | Param (n, t) -> arr [ str "param"; ty t; str n ]
  | Global (n, t) -> arr [ str "global"; ty t; str n ]

let binop = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Sdiv -> "sdiv"
  | Srem -> "srem"
  | Udiv -> "udiv"
  | Urem -> "urem"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Shl -> "shl"
  | Lshr -> "lshr"
  | Ashr -> "ashr"

let cmp = function
  | Eq -> "eq"
  | Ne -> "ne"
  | Slt -> "slt"
  | Sle -> "sle"
  | Sgt -> "sgt"
  | Sge -> "sge"
  | Ult -> "ult"
  | Ule -> "ule"
  | Ugt -> "ugt"
  | Uge -> "uge"

let extension = function
  | No_extension -> str "none"
  | Sign_extension -> str "sext"
  | Zero_extension -> str "zext"

let gep_index = function Ir.Zero -> arr [ str "zero" ] | Index v -> value v

let instr ?(redirect = Fun.id) = function
  | Bin (d, op, t, a, b) ->
      arr [ str "bin"; int d; str (binop op); ty t; value a; value b ]
  | Cmp (d, c, t, a, b) -> arr [ str "cmp"; int d; str (cmp c); ty t; value a; value b ]
  | Alloca (d, t, align) -> arr [ str "alloca"; int d; ty t; int align ]
  | Load (d, t, p, align) -> arr [ str "load"; int d; ty t; value p; int align ]
  | Load_volatile (d, t, p, align) ->
      arr [ str "load_volatile"; int d; ty t; value p; int align ]
  | Store (t, v, p, align) -> arr [ str "store"; ty t; value v; value p; int align ]
  | Store_volatile (t, v, p, align) ->
      arr [ str "store_volatile"; ty t; value v; value p; int align ]
  | Gep (d, t, p, idx) ->
      arr [ str "gep"; int d; ty t; value p; arr (List.map gep_index idx) ]
  | Cast (d, k, from, v, target) ->
      arr [ str "cast"; int d; str k; ty from; value v; ty target ]
  | Call (d, ext, t, name, args) ->
      arr
        [
          str "call";
          (match d with Some d -> int d | None -> "null");
          extension ext;
          ty t;
          str (redirect name);
          arr (List.map (fun (t, e, v) -> arr [ ty t; extension e; value v ]) args);
        ]
  | Phi (d, t, incoming) ->
      arr
        [
          str "phi";
          int d;
          ty t;
          arr (List.map (fun (v, b) -> arr [ value v; int b ]) incoming);
        ]
  | Select (d, c, a, b) -> arr [ str "select"; int d; value c; value a; value b ]
  | Extract (d, t, v, i) -> arr [ str "extract"; int d; ty t; value v; value i ]
  | Insert (d, t, v, x, i) ->
      arr [ str "insert"; int d; ty t; value v; value x; value i ]
  | Shuffle_zero (d, t, v) -> arr [ str "shuffle_zero"; int d; ty t; value v ]
  | Shufflevector (d, t, a, b, m) ->
      arr [ str "shufflevector"; int d; ty t; value a; value b; value m ]
  | String_ptr (d, s, n) -> arr [ str "string_ptr"; int d; int s; int n ]
  | Global_ptr (d, n, t) -> arr [ str "global_ptr"; int d; str n; ty t ]
  | Trap -> arr [ str "trap" ]

let terminator = function
  | Ret None -> arr [ str "ret" ]
  | Ret (Some (t, v)) -> arr [ str "ret"; ty t; value v ]
  | Br b -> arr [ str "br"; int b ]
  | CondBr (c, t, f) -> arr [ str "condbr"; value c; int t; int f ]
  | Switch (t, v, cases, d) ->
      arr
        [
          str "switch";
          ty t;
          value v;
          arr (List.map (fun (k, b) -> arr [ i64 k; int b ]) cases);
          int d;
        ]
  | Unreachable -> arr [ str "unreachable" ]

let global = function
  | String_global { name; bytes } ->
      obj [ ("kind", str "string"); ("name", str name); ("bytes", hex bytes) ]
  | Array_global { name; elem_ty; elems; align } ->
      obj
        [
          ("kind", str "array");
          ("name", str name);
          ("elem", ty elem_ty);
          ("elems", arr (List.map i64 elems));
          ("align", int align);
        ]
  | Storage_global { name; storage_ty; size; bytes; pointers; readonly; align; linkage }
    ->
      obj
        [
          ("kind", str "storage");
          ("name", str name);
          ("ty", ty storage_ty);
          ("size", int size);
          ("bytes", match bytes with Some b -> hex b | None -> "null");
          ( "pointers",
            arr (List.map (fun (o, n, a) -> arr [ int o; str n; int a ]) pointers) );
          ("readonly", string_of_bool readonly);
          ("align", int align);
          ( "linkage",
            str
              (match linkage with
              | Ast.Internal_global -> "internal"
              | Ast.Export_c -> "export"
              | Ast.Import_c -> "import"
              | Ast.Import_const_c -> "import_const") );
        ]

let func ?(redirect = Fun.id) (f : func) =
  obj
    [
      ("name", str (redirect f.name));
      ( "params",
        arr
          (List.map
             (fun (p : param) ->
               obj
                 [
                   ("name", str p.name);
                   ("ty", ty p.ty);
                   ("extension", extension p.extension);
                 ])
             f.params) );
      ("ret", ty f.ret);
      ( "linkage",
        str (match f.linkage with Internal -> "internal" | External -> "external") );
      ("variadic", string_of_bool f.variadic);
      ( "blocks",
        arr
          (List.map
             (fun (b : block) ->
               obj
                 [
                   ("id", int b.id);
                   ("instrs", arr (List.map (instr ~redirect) b.instrs));
                   ("term", terminator b.terminator);
                 ])
             f.blocks) );
      ("ret_extension", extension f.ret_extension);
    ]

let render ?(redirect = Fun.id) ?(c_adapters = []) m =
  obj
    [
      ( "structs",
        arr
          (List.map
             (fun (s : struct_def) ->
               obj
                 [
                   ("name", str s.name);
                   ("fields", arr (List.map ty s.fields));
                   ("tail_padding", int s.tail_padding);
                 ])
             m.structs) );
      ("globals", arr (List.map global m.globals));
      ("funcs", arr (List.map (func ~redirect) m.funcs));
      ("format", str "fas-ir-json");
      ("version", int 1);
      ("target_triple", str m.target_triple);
      ("data_layout", str m.data_layout);
      ( "no_inline",
        match m.no_inline_function with Some name -> str name | None -> "null" );
      ( "c_adapters",
        arr
          (List.map
             (fun (name, symbol) -> obj [ ("name", str name); ("symbol", str symbol) ])
             c_adapters) );
    ]
  ^ "\n"
