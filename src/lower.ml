[@@@warning "-8-26-32-34-37-39-60-69"]

let unfinished name = Error [ Diag.error Span.synthetic ("lower scaffold: implement " ^ name) ]

type block_state = {
  id : int;
  instrs : Ir.instr Queue.t;
  term : Ir.terminator option ref;
}

type loop = { break_to : int; continue_to : int; keep_scopes : int }

type state = {
  mutable next_value : int;
  mutable next_block : int;
  blocks : block_state Queue.t;
  entry : block_state;
  mutable current : block_state;
  env : (string, Ir.value) Hashtbl.t;
  mutable env_scopes : (string * Ir.value option) list list;
  ret : Ir.ty;
  structs : Hir.struct_def list;
  strings : string list;
  mutable defer_scopes : Hir.stmt list list list;
  mutable loops : loop list;
}

let error span msg = Error [ Diag.error span msg ]
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let fresh s =
  let n = s.next_value in
  s.next_value <- n + 1;
  n

let fresh_block s =
  let n = s.next_block in
  s.next_block <- n + 1;
  let b = { id = n; instrs = Queue.create (); term = ref None } in
  Queue.add b s.blocks;
  b

let emit s i = Queue.add i s.current.instrs

(* Static local storage must be in the entry block so LLVM's mem2reg/SROA
   pipelines can promote it. Binding visibility and initializer effects remain
   at the declaration site; only the fixed-size alloca is hoisted. *)
let emit_entry s i = Queue.add i s.entry.instrs
let open_block s = !(s.current.term) = None

let value_ty = function
  | Ir.Const (t, _)
  | Null t
  | Undef t
  | Zero t
  | Local (_, t)
  | Param (_, t)
  | Global (_, t) ->
      t

let rec ty = function
  | Hir.Bool -> Ir.I1
  | Hir.Int Hir.U8 | Hir.Int Hir.I8 -> I8
  | Hir.Int Hir.U16 | Hir.Int Hir.I16 -> I16
  | Hir.Int Hir.U32 | Hir.Int Hir.I32 -> I32
  | Hir.Int Hir.U64 | Hir.Int Hir.I64 | Hir.Int Hir.Usize | Hir.Int Hir.Isize ->
      I64
  | Hir.Ptr t -> Ir.Ptr (ty t)
  | Hir.Vec (n, t) -> Ir.Vector (n, ty t)
  | Hir.Array (n, t) -> Ir.Array (n, ty t)
  | Hir.Struct n -> Ir.Struct n
  | Hir.Opaque n -> Ir.Struct n
  | Hir.Void -> Ir.Void

let align s t =
  match Hir.layout s.structs t with Ok (_, a) -> a | Error _ -> 1

let zero t = Ir.Const (t, 0L)

let ones = function
  | Ir.I1 -> 1L
  | I8 -> 0xffL
  | I16 -> 0xffffL
  | I32 -> 0xffff_ffffL
  | I64 -> Int64.minus_one
  | _ -> Int64.minus_one

let unsigned = function
  | Hir.Int (Hir.U8 | U16 | U32 | U64 | Usize) -> true
  | _ -> false

let cmp_for t = function
  | Ast.Eq -> Ir.Eq
  | Ne -> Ne
  | Lt -> if unsigned t then Ult else Slt
  | Le -> if unsigned t then Ule else Sle
  | Gt -> if unsigned t then Ugt else Sgt
  | Ge -> if unsigned t then Uge else Sge
  | _ -> Eq

let bin_for t = function
  | Ast.Add -> Ir.Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> if unsigned t then Udiv else Sdiv
  | Rem -> if unsigned t then Urem else Srem
  | Bit_and -> And
  | Bit_or -> Or
  | Bit_xor -> Xor
  | _ -> Add

let unfinished name = Error [ Diag.error Span.synthetic ("lower scaffold: implement " ^ name) ]

let rec expr _state _expression = unfinished "expr"
and exprs _state _expressions = unfinished "exprs"
and lower_builtin _state _builtin _arguments _ty = unfinished "lower_builtin"
and lower_short _state _operator _left _right = unfinished "lower_short"
and lower_ternary _state _condition _then_value _else_value _ty = unfinished "lower_ternary"
and materialize _state _expression = unfinished "materialize"
and address _state _expression = unfinished "address"
and index_address _state _aggregate _index = unfinished "index_address"
and field_address _state _aggregate _offset = unfinished "field_address"

let rec emit_defer_body _state _body = unfinished "emit_defer_body"
and emit_scope_defers _state _index = unfinished "emit_scope_defers"
and unwind _state _keep = unfinished "unwind"
and scoped _state _statements = unfinished "scoped"
and stmt _state _statement = unfinished "stmt"
and stmt_list _state _statements = unfinished "stmt_list"
and target_address _state _target = unfinished "target_address"
and vector_lane _state _aggregate _index = unfinished "vector_lane"
and lower_if _state _condition _then_body _else_body = unfinished "lower_if"
and lower_while _state _condition _body = unfinished "lower_while"
and lower_for _state _init _condition _step _body = unfinished "lower_for"
and lower_switch _state _expression _arms _default = unfinished "lower_switch"

let lower_func _structs _strings _force_external _function_ = unfinished "lower_func"
let intrinsic_decls _functions = []
let lower (_program : Hir.program) = unfinished "lower"
