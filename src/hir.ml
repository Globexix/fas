type int_kind = U8 | U16 | U32 | I8 | I16 | I32 | I64 | U64 | Usize | Isize

type ty =
  | Bool
  | Int of int_kind
  | Ptr of ty
  | ConstPtr of ty
  | Array of int * ty
  | Vec of int * ty
  | Struct of string
  | Opaque of string
  | Void

type field = { name : string; ty : ty; offset : int }
type struct_def = { name : string; fields : field list; size : int; align : int }
type const_def = { name : string; ty : ty; bits : int64 }
type const_arr_def = { name : string; ty : ty; elems : int64 list }
type func_sig = { params : (string * ty) list; ret : ty; variadic : bool }
type local = { name : string; ty : ty; id : int }
type linkage = Internal | External_c
type builtin = Rotl | Rotr | Popcount | Ctz | Clz | Add_sat | Sub_sat
type call_target = User of string | Builtin of builtin

type expr =
  | EInt of int64 * ty * Span.t
  | EBool of bool * Span.t
  | EVector of int64 list * ty * Span.t
  | Null of ty * Span.t
  | EString of int * Span.t
  | Local of local * Span.t
  | Unary of Ast.unop * expr * ty * Span.t
  | Binary of Ast.binop * expr * expr * ty * Span.t
  | Call of call_target * expr list * ty * Span.t
  | Cast of Ast.cast_kind * expr * ty * Span.t
  | Index of expr * expr * ty * Span.t
  | Field of expr * string * ty * int * Span.t
  | Deref of expr * ty * Span.t
  | Address of expr * ty * Span.t
  | Ptr_add of bool * expr * expr * ty * Span.t
  | Sizeof of ty * int * Span.t
  | Alignof of ty * int * Span.t
  | Offsetof of ty * string * int * Span.t
  | Splat of expr * ty * Span.t
  | Ternary of expr * expr * expr * ty * Span.t
  | Const_array of string * ty * Span.t
  | Struct_lit of string * expr list * ty * Span.t

type assign_target =
  | ALocal of local
  | ADeref of expr
  | AIndex of expr * expr
  | AField of expr * string * int

type stmt =
  | Let of local * expr option * Span.t
  | Assign of assign_target * expr * Span.t
  | Compound_assign of assign_target * Ast.binop * expr * ty * Span.t
  | Return of expr option * Span.t
  | If of expr * stmt list * stmt list option * Span.t
  | While of expr * stmt list * Span.t
  | For of stmt option * expr option * stmt option * stmt list * Span.t
  | Switch of expr * (expr * stmt list) list * stmt list option * Span.t
  | Break of Span.t
  | Continue of Span.t
  | Defer of stmt list * Span.t
  | Expr of expr * Span.t
  | Block of stmt list * Span.t

type func_body = Declaration | Statements of stmt list | Asm of string

type func = {
  name : string;
  params : local list;
  ret : ty;
  body : func_body;
  linkage : linkage;
  variadic : bool;
}

type program = {
  structs : struct_def list;
  consts : const_def list;
  const_arrays : const_arr_def list;
  funcs : func list;
  strings : string list;
}

let rec ty_equal a b =
  match (a, b) with
  | Bool, Bool | Void, Void -> true
  | Int a, Int b -> a = b
  | Ptr a, Ptr b -> ty_equal a b
  | ConstPtr a, ConstPtr b -> ty_equal a b
  | Array (na, a), Array (nb, b) | Vec (na, a), Vec (nb, b) -> na = nb && ty_equal a b
  | Struct a, Struct b | Opaque a, Opaque b -> a = b
  | _ -> false

let rec ty_name = function
  | Bool -> "bool"
  | Void -> "void"
  | Int U8 -> "u8"
  | Int U16 -> "u16"
  | Int U32 -> "u32"
  | Int U64 -> "u64"
  | Int I8 -> "i8"
  | Int I16 -> "i16"
  | Int I32 -> "i32"
  | Int I64 -> "i64"
  | Int Usize -> "usize"
  | Int Isize -> "isize"
  | Ptr t -> "ptr[" ^ ty_name t ^ "]"
  | ConstPtr t -> "ptr[const " ^ ty_name t ^ "]"
  | Array (n, t) -> Printf.sprintf "arr[%d, %s]" n (ty_name t)
  | Vec (n, t) -> Printf.sprintf "vec[%d, %s]" n (ty_name t)
  | Struct n | Opaque n -> n

let expr_ty = function
  | EInt (_, t, _)
  | EVector (_, t, _)
  | Unary (_, _, t, _)
  | Binary (_, _, _, t, _)
  | Call (_, _, t, _)
  | Cast (_, _, t, _)
  | Index (_, _, t, _)
  | Field (_, _, t, _, _)
  | Deref (_, t, _)
  | Address (_, t, _)
  | Ptr_add (_, _, _, t, _)
  | Splat (_, t, _)
  | Ternary (_, _, _, t, _)
  | Const_array (_, t, _)
  | Struct_lit (_, _, t, _) ->
      t
  | Local (local, _) -> local.ty
  | EBool _ -> Bool
  | Null (t, _) -> t
  | EString _ -> ConstPtr (Int U8)
  | Sizeof _ | Alignof _ | Offsetof _ -> Int Usize

let expr_span = function
  | EInt (_, _, s)
  | EVector (_, _, s)
  | EBool (_, s)
  | Null (_, s)
  | EString (_, s)
  | Local (_, s)
  | Unary (_, _, _, s)
  | Binary (_, _, _, _, s)
  | Call (_, _, _, s)
  | Cast (_, _, _, s)
  | Index (_, _, _, s)
  | Field (_, _, _, _, s)
  | Deref (_, _, s)
  | Address (_, _, s)
  | Ptr_add (_, _, _, _, s)
  | Sizeof (_, _, s)
  | Alignof (_, _, s)
  | Offsetof (_, _, _, s)
  | Splat (_, _, s)
  | Ternary (_, _, _, _, s)
  | Const_array (_, _, s)
  | Struct_lit (_, _, _, s) ->
      s

type flow_summary = {
  falls_through : bool;
  returns : bool;
  breaks : bool;
  continues : bool;
}

let flowing =
  { falls_through = true; returns = false; breaks = false; continues = false }

let choose_flow left right =
  {
    falls_through = left.falls_through || right.falls_through;
    returns = left.returns || right.returns;
    breaks = left.breaks || right.breaks;
    continues = left.continues || right.continues;
  }

let sequence_flow left right =
  {
    falls_through = left.falls_through && right.falls_through;
    returns = left.returns || (left.falls_through && right.returns);
    breaks = left.breaks || (left.falls_through && right.breaks);
    continues = left.continues || (left.falls_through && right.continues);
  }

let cleanup_flow cleanup exits =
  {
    falls_through = cleanup.falls_through && exits.falls_through;
    returns = cleanup.falls_through && exits.returns;
    breaks = cleanup.falls_through && exits.breaks;
    continues = cleanup.falls_through && exits.continues;
  }

let condition_is_true = function EBool (true, _) -> true | _ -> false

let rec stmt_flow = function
  | Return _ -> { flowing with falls_through = false; returns = true }
  | Break _ -> { flowing with falls_through = false; breaks = true }
  | Continue _ -> { flowing with falls_through = false; continues = true }
  | Block (body, _) -> block_flow body
  | If (_, then_body, else_body, _) ->
      choose_flow (block_flow then_body)
        (match else_body with None -> flowing | Some body -> block_flow body)
  | Switch (_, arms, default, _) ->
      let branches = List.map (fun (_, body) -> block_flow body) arms in
      let branches =
        match default with
        | None -> flowing :: branches
        | Some body -> block_flow body :: branches
      in
      List.fold_left choose_flow { flowing with falls_through = false } branches
  | While (condition, body, _) ->
      loop_flow (condition_is_true condition) (block_flow body)
  | For (init, condition, step, body, _) ->
      let prefix =
        match init with None -> flowing | Some statement -> stmt_flow statement
      in
      let iteration =
        sequence_flow (block_flow body)
          (match step with None -> flowing | Some statement -> stmt_flow statement)
      in
      let unconditional =
        match condition with
        | None -> true
        | Some expression -> condition_is_true expression
      in
      sequence_flow prefix (loop_flow unconditional iteration)
  | Defer (body, _) -> cleanup_flow (block_flow body) flowing
  | Let _ | Assign _ | Compound_assign _ | Expr _ -> flowing

and block_flow = function
  | [] -> flowing
  | Defer (body, _) :: rest -> cleanup_flow (block_flow body) (block_flow rest)
  | statement :: rest -> sequence_flow (stmt_flow statement) (block_flow rest)

and loop_flow unconditional body =
  {
    falls_through = (not unconditional) || body.breaks;
    returns = body.returns;
    breaks = false;
    continues = false;
  }

let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let add_size left right =
  if left < 0 || right < 0 || left > max_int - right then
    Error "aggregate size overflows"
  else Ok (left + right)

let int_bytes ?(target = Target_layout.current) = function
  | U8 | I8 -> 1
  | U16 | I16 -> 2
  | U32 | I32 -> 4
  | U64 | I64 -> 8
  | Usize | Isize -> target.Target_layout.pointer_size

let int_layout target k = Target_layout.integer target (int_bytes ~target k * 8)

let scalar_bits target = function
  | Bool -> Ok 1
  | Int k -> Ok (int_bytes ~target k * 8)
  | Ptr _ | ConstPtr _ -> Ok (target.Target_layout.pointer_size * 8)
  | _ -> Error "vector element type must be a scalar"

let layout ?(target = Target_layout.current) structs ty =
  let rec go visiting = function
    | Bool -> Target_layout.integer target 1
    | Int k -> int_layout target k
    | Ptr _ | ConstPtr _ -> Target_layout.pointer target
    | Array (n, t) ->
        let* s, a = go visiting t in
        let* size = Target_layout.multiply_size n s in
        Ok (size, a)
    | Vec (n, t) ->
        let* bits = scalar_bits target t in
        Target_layout.vector target n bits
    | Void -> Error "void has no object layout"
    | Opaque n -> Error ("opaque type `" ^ n ^ "` has no layout")
    | Struct n -> (
        if List.mem n visiting then
          Error (Printf.sprintf "recursive by-value struct `%s`" n)
        else
          match List.find_opt (fun (s : struct_def) -> s.name = n) structs with
          | None -> Error (Printf.sprintf "unknown struct `%s`" n)
          | Some (s : struct_def) -> Ok (s.size, s.align))
  in
  go [] ty

type struct_layout_cache = {
  target : Target_layout.t;
  decls : (string * (string * ty) list * int option) list;
  definitions : (string, struct_def) Hashtbl.t;
}

let struct_layout_cache ?(target = Target_layout.current) decls =
  { target; decls; definitions = Hashtbl.create (List.length decls) }

let compute_struct_cached cache name =
  let target = cache.target in
  let decls = cache.decls in
  let rec calc visiting n =
    match Hashtbl.find_opt cache.definitions n with
    | Some definition -> Ok (definition.fields, definition.size, definition.align)
    | None -> (
        if List.mem n visiting then
          Error (Printf.sprintf "recursive by-value struct `%s`" n)
        else
          match List.find_opt (fun (x, _, _) -> x = n) decls with
          | None -> Error (Printf.sprintf "unknown struct `%s`" n)
          | Some (_, fields, explicit) ->
              let rec each off maxa out = function
                | [] ->
                    let align = max maxa (Option.value ~default:1 explicit) in
                    let* size = Target_layout.round_up_size off align in
                    let definition = { name = n; fields = List.rev out; size; align } in
                    Hashtbl.replace cache.definitions n definition;
                    Ok (definition.fields, definition.size, definition.align)
                | (fname, fty) :: rest ->
                    let* size, align =
                      match fty with
                      | Struct sn ->
                          let* _, sz, al = calc (n :: visiting) sn in
                          Ok (sz, al)
                      | _ -> field_layout (n :: visiting) fty
                    in
                    let* next = Target_layout.round_up_size off align in
                    let* next_offset = add_size next size in
                    each next_offset (max maxa align)
                      ({ name = fname; ty = fty; offset = next } :: out)
                      rest
              in
              each 0 1 [] fields)
  and field_layout visiting = function
    | Bool -> Target_layout.integer target 1
    | Int k -> int_layout target k
    | Ptr _ | ConstPtr _ -> Target_layout.pointer target
    | Void -> Error "void has no object layout"
    | Opaque n -> Error (Printf.sprintf "opaque type `%s` has no layout" n)
    | Struct n ->
        let* _, s, a = calc visiting n in
        Ok (s, a)
    | Array (n, t) ->
        let* s, a = field_layout visiting t in
        let* size = Target_layout.multiply_size n s in
        Ok (size, a)
    | Vec (n, t) ->
        let* bits = scalar_bits target t in
        Target_layout.vector target n bits
  in
  let* fields, size, align = calc [] name in
  Ok { name; fields; size; align }

let compute_struct ?target decls name =
  compute_struct_cached (struct_layout_cache ?target decls) name

let render p =
  let one_struct (s : struct_def) =
    Printf.sprintf "struct %s size=%d align=%d" s.name s.size s.align
  in
  let one_fn f =
    Printf.sprintf "fn %s(%s) %s" f.name
      (String.concat ", "
         (List.map
            (fun (local : local) -> local.name ^ ":" ^ ty_name local.ty)
            f.params))
      (ty_name f.ret)
  in
  String.concat "\n" (List.map one_struct p.structs @ List.map one_fn p.funcs)
