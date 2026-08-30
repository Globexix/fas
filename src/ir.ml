type ty =
  | I1
  | I8
  | I16
  | I32
  | I64
  | Ptr of ty
  | Vector of int * ty
  | Struct of string
  | Array of int * ty
  | Void

type value =
  | Const of ty * int64
  | Null of ty
  | Undef of ty
  | Zero of ty
  | Local of int * ty
  | Param of string * ty
  | Global of string * ty

type binop =
  | Add
  | Sub
  | Mul
  | Sdiv
  | Srem
  | Udiv
  | Urem
  | And
  | Or
  | Xor
  | Shl
  | Lshr
  | Ashr

type cmp = Eq | Ne | Slt | Sle | Sgt | Sge | Ult | Ule | Ugt | Uge
type gep_index = Zero | Index of value
type extension = No_extension | Sign_extension | Zero_extension

type instr =
  | Bin of int * binop * ty * value * value
  | Cmp of int * cmp * ty * value * value
  | Alloca of int * ty * int
  | Load of int * ty * value * int
  | Store of ty * value * value * int
  | Gep of int * ty * value * gep_index list
  | Cast of int * string * ty * value * ty
  | Call of int option * extension * ty * string * (ty * extension * value) list
  | Phi of int * ty * (value * int) list
  | Select of int * value * value * value
  | Extract of int * ty * value * value
  | Insert of int * ty * value * value * value
  | Shuffle_zero of int * ty * value
  | String_ptr of int * int * int
  | Global_ptr of int * string * ty
  | Trap

type terminator =
  | Ret of (ty * value) option
  | Br of int
  | CondBr of value * int * int
  | Switch of ty * value * (int64 * int) list * int
  | Unreachable

type param = { name : string; ty : ty; extension : extension }
type linkage = Internal | External

type func = {
  name : string;
  params : param list;
  ret : ty;
  ret_extension : extension;
  blocks : block list;
  linkage : linkage;
  variadic : bool;
  asm_body : string option;
}

and block = { id : int; label : string; instrs : instr list; terminator : terminator }

type struct_def = { name : string; fields : ty list; tail_padding : int }

type global =
  | String_global of { name : string; bytes : string }
  | Array_global of { name : string; elem_ty : ty; elems : int64 list; align : int }

type module_ = {
  target_triple : string;
  data_layout : string;
  structs : struct_def list;
  globals : global list;
  funcs : func list;
}

let quote_identifier n =
  if
    String.for_all
      (function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '$' -> true | _ -> false)
      n
  then n
  else "\"" ^ String.escaped n ^ "\""

let struct_name n = "%" ^ quote_identifier ("struct." ^ n)

let rec ty_name = function
  | I1 -> "i1"
  | I8 -> "i8"
  | I16 -> "i16"
  | I32 -> "i32"
  | I64 -> "i64"
  | Ptr _ -> "ptr"
  | Vector (n, t) -> Printf.sprintf "<%d x %s>" n (ty_name t)
  | Struct n -> struct_name n
  | Array (n, t) -> Printf.sprintf "[%d x %s]" n (ty_name t)
  | Void -> "void"

let value_ty = function
  | Const (t, _)
  | Null t
  | Undef t
  | Zero t
  | Local (_, t)
  | Param (_, t)
  | Global (_, t) ->
      t

let symbol n =
  if
    String.for_all
      (function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '$' -> true | _ -> false)
      n
  then "@" ^ n
  else "@\"" ^ String.escaped n ^ "\""

let extension_name = function
  | No_extension -> ""
  | Sign_extension -> "signext "
  | Zero_extension -> "zeroext "

let extension_attr = function
  | No_extension -> ""
  | Sign_extension -> " signext"
  | Zero_extension -> " zeroext"

let value_name = function
  | Const (I1, 0L) -> "false"
  | Const (I1, _) -> "true"
  | Const (_, v) -> Int64.to_string v
  | Null _ -> "null"
  | Undef _ -> "poison"
  | Zero _ -> "zeroinitializer"
  | Local (i, _) -> Printf.sprintf "%%v%d" i
  | Param (n, _) -> "%" ^ n
  | Global (n, _) -> symbol n

let quote_bytes s =
  let b = Buffer.create (String.length s * 2) in
  String.iter
    (fun c ->
      let n = Char.code c in
      if n >= 32 && n < 127 && c <> '"' && c <> '\\' then Buffer.add_char b c
      else Buffer.add_string b (Printf.sprintf "\\%02X" n))
    s;
  Buffer.contents b

let bin_name = function
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

let cmp_name = function
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

let instr_line = function
  | Bin (i, op, t, a, b) ->
      Printf.sprintf "  %%v%d = %s %s %s, %s" i (bin_name op) (ty_name t) (value_name a)
        (value_name b)
  | Cmp (i, c, t, a, b) ->
      Printf.sprintf "  %%v%d = icmp %s %s %s, %s" i (cmp_name c) (ty_name t)
        (value_name a) (value_name b)
  | Alloca (i, t, a) -> Printf.sprintf "  %%v%d = alloca %s, align %d" i (ty_name t) a
  | Load (i, t, p, a) ->
      Printf.sprintf "  %%v%d = load %s, ptr %s, align %d" i (ty_name t) (value_name p)
        a
  | Store (t, v, p, a) ->
      Printf.sprintf "  store %s %s, ptr %s, align %d" (ty_name t) (value_name v)
        (value_name p) a
  | Gep (i, t, p, idxs) ->
      let one = function
        | Zero -> "i64 0"
        | Index v -> ty_name (value_ty v) ^ " " ^ value_name v
      in
      Printf.sprintf "  %%v%d = getelementptr %s, ptr %s, %s" i (ty_name t)
        (value_name p)
        (String.concat ", " (List.map one idxs))
  | Cast (i, k, st, v, dt) ->
      Printf.sprintf "  %%v%d = %s %s %s to %s" i k (ty_name st) (value_name v)
        (ty_name dt)
  | Call (Some i, extension, t, n, args) ->
      Printf.sprintf "  %%v%d = call %s%s %s(%s)" i (extension_name extension)
        (ty_name t) (symbol n)
        (String.concat ", "
           (List.map
              (fun (t, extension, v) ->
                ty_name t ^ " " ^ extension_name extension ^ value_name v)
              args))
  | Call (None, extension, t, n, args) ->
      Printf.sprintf "  call %s%s %s(%s)" (extension_name extension) (ty_name t)
        (symbol n)
        (String.concat ", "
           (List.map
              (fun (t, extension, v) ->
                ty_name t ^ " " ^ extension_name extension ^ value_name v)
              args))
  | Phi (i, t, xs) ->
      Printf.sprintf "  %%v%d = phi %s %s" i (ty_name t)
        (String.concat ", "
           (List.map (fun (v, b) -> Printf.sprintf "[ %s, %%b%d ]" (value_name v) b) xs))
  | Select (i, c, a, b) ->
      Printf.sprintf "  %%v%d = select %s %s, %s %s, %s %s" i
        (ty_name (value_ty c))
        (value_name c)
        (ty_name (value_ty a))
        (value_name a)
        (ty_name (value_ty b))
        (value_name b)
  | Extract (i, vt, v, l) ->
      Printf.sprintf "  %%v%d = extractelement %s %s, %s %s" i (ty_name vt)
        (value_name v)
        (ty_name (value_ty l))
        (value_name l)
  | Insert (i, vt, v, l, x) ->
      Printf.sprintf "  %%v%d = insertelement %s %s, %s %s, %s %s" i (ty_name vt)
        (value_name v)
        (ty_name (value_ty x))
        (value_name x)
        (ty_name (value_ty l))
        (value_name l)
  | Shuffle_zero (i, vt, v) ->
      let n = match vt with Vector (n, _) -> n | _ -> 0 in
      Printf.sprintf
        "  %%v%d = shufflevector %s %s, %s poison, <%d x i32> zeroinitializer" i
        (ty_name vt) (value_name v) (ty_name vt) n
  | String_ptr (i, index, n) ->
      Printf.sprintf "  %%v%d = getelementptr [%d x i8], ptr @.str.%d, i64 0, i64 0" i n
        index
  | Global_ptr (i, n, t) ->
      Printf.sprintf "  %%v%d = getelementptr %s, ptr @%s, i64 0" i (ty_name t) n
  | Trap -> "  call void @llvm.trap()"

let term_line = function
  | Ret None -> "  ret void"
  | Ret (Some (t, v)) -> Printf.sprintf "  ret %s %s" (ty_name t) (value_name v)
  | Br b -> Printf.sprintf "  br label %%b%d" b
  | CondBr (c, a, b) ->
      Printf.sprintf "  br i1 %s, label %%b%d, label %%b%d" (value_name c) a b
  | Switch (t, v, cases, d) ->
      Printf.sprintf "  switch %s %s, label %%b%d [ %s ]" (ty_name t) (value_name v) d
        (String.concat " "
           (List.map
              (fun (k, b) -> Printf.sprintf "%s %Ld, label %%b%d" (ty_name t) k b)
              cases))
  | Unreachable -> "  unreachable"

let param_string named p =
  ty_name p.ty ^ extension_attr p.extension ^ if named then " %" ^ p.name else ""

let render m =
  let header =
    [
      "; ModuleID = 'fas'";
      "source_filename = \"fas\"";
      "target datalayout = \"" ^ m.data_layout ^ "\"";
      "target triple = \"" ^ m.target_triple ^ "\"";
      "";
    ]
  in
  let structs =
    List.map
      (fun s ->
        let fields = List.map ty_name s.fields in
        let fields =
          if s.tail_padding = 0 then fields
          else fields @ [ Printf.sprintf "[%d x i8]" s.tail_padding ]
        in
        Printf.sprintf "%s = type { %s }" (struct_name s.name)
          (String.concat ", " fields))
      m.structs
  in
  let globals =
    List.map
      (function
        | String_global { name; bytes } ->
            Printf.sprintf "@%s = private unnamed_addr constant [%d x i8] c\"%s\"" name
              (String.length bytes) (quote_bytes bytes)
        | Array_global { name; elem_ty; elems; align } ->
            let es =
              String.concat ", "
                (List.map (fun v -> ty_name elem_ty ^ " " ^ Int64.to_string v) elems)
            in
            Printf.sprintf
              "@%s = private unnamed_addr constant [%d x %s] [%s], align %d" name
              (List.length elems) (ty_name elem_ty) es align)
      m.globals
  in
  let fn f =
    let ps = String.concat ", " (List.map (param_string (f.blocks <> [])) f.params) in
    let ps = if f.variadic then ps ^ if ps = "" then "..." else ", ..." else ps in
    if f.blocks = [] || Option.is_some f.asm_body then
      Printf.sprintf "declare %s%s %s(%s)"
        (extension_name f.ret_extension)
        (ty_name f.ret) (symbol f.name) ps
    else
      let link = if f.linkage = Internal then "internal " else "" in
      Printf.sprintf "define %s%s%s %s(%s) {\n%s\n}" link
        (extension_name f.ret_extension)
        (ty_name f.ret) (symbol f.name) ps
        (String.concat "\n"
           (List.concat_map
              (fun b ->
                (("b" ^ string_of_int b.id ^ ":") :: List.map instr_line b.instrs)
                @ [ term_line b.terminator ])
              f.blocks))
  in
  String.concat "\n" (header @ structs @ globals @ List.map fn m.funcs) ^ "\n"

let render_debug m =
  let global = function
    | String_global { name; bytes } ->
        Printf.sprintf "    String_global { name = %S; bytes = %S }" name bytes
    | Array_global { name; elem_ty; elems; align } ->
        Printf.sprintf
          "    Array_global { name = %S; elem_ty = %s; elems = [%s]; align = %d }" name
          (ty_name elem_ty)
          (String.concat "; " (List.map Int64.to_string elems))
          align
  in
  let block (b : block) =
    String.concat "\n"
      ([ Printf.sprintf "      Block { id = %d; label = %S; instrs = [" b.id b.label ]
      @ List.map
          (fun instruction -> "        " ^ String.trim (instr_line instruction))
          b.instrs
      @ [
          "      ];";
          "      terminator = " ^ String.trim (term_line b.terminator);
          "      }";
        ])
  in
  let func (f : func) =
    let params =
      f.params
      |> List.map (fun (p : param) -> Printf.sprintf "%s:%s" p.name (ty_name p.ty))
      |> String.concat ", "
    in
    String.concat "\n"
      ([
         Printf.sprintf
           "    Function { name = %S; params = [%s]; ret = %s; variadic = %b; blocks = \
            ["
           f.name params (ty_name f.ret) f.variadic;
       ]
      @ List.map block f.blocks @ [ "    ] }" ])
  in
  String.concat "\n"
    ([
       "Module {";
       Printf.sprintf "  target_triple = %S;" m.target_triple;
       Printf.sprintf "  data_layout = %S;" m.data_layout;
       "  globals = [";
     ]
    @ List.map global m.globals @ [ "  ];"; "  functions = [" ] @ List.map func m.funcs
    @ [ "  ];"; "}" ])
  ^ "\n"

let raw_assembly m =
  m.funcs
  |> List.filter_map (fun (f : func) ->
      Option.map
        (fun raw ->
          Printf.sprintf
            "\n.text\n.globl %s\n.type %s,@function\n%s:\n%s\n.size %s, .-%s\n" f.name
            f.name f.name raw f.name f.name)
        f.asm_body)
  |> String.concat "\n"
