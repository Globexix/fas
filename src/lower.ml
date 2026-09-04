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
  env : (int, Ir.value) Hashtbl.t;
  mutable env_scopes : (int * Ir.value option) list list;
  ret : Ir.ty;
  structs : Hir.struct_def list;
  strings : string list;
  c_functions : Hir.func list;
  mutable defer_scopes : Hir.stmt list list list;
  mutable loops : loop list;
}

let error span msg = Error [ Diag.error span msg ]
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let layout_ok structs t =
  match Hir.layout structs t with
  | Ok _ -> Ok ()
  | Error m ->
      error Span.synthetic
        (Printf.sprintf "internal error: type `%s` has no layout: %s" (Hir.ty_name t) m)

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
  | Hir.Int Hir.U64 | Hir.Int Hir.I64 -> I64
  | Hir.Int Hir.Usize | Hir.Int Hir.Isize -> (
      match Target_layout.current.pointer_size with
      | 4 -> I32
      | 8 -> I64
      | size -> invalid_arg (Printf.sprintf "unsupported pointer size: %d" size))
  | Hir.Ptr t | Hir.ConstPtr t -> Ir.Ptr (ty t)
  | Hir.Vec (n, t) -> Ir.Vector (n, ty t)
  | Hir.Array (n, t) -> Ir.Array (n, ty t)
  | Hir.Struct n -> Ir.Struct n
  | Hir.Opaque n -> Ir.Struct n
  | Hir.Void -> Ir.Void

let ir_extension = function
  | Target_layout.C_no_extension -> Ir.No_extension
  | Target_layout.C_sign_extension -> Ir.Sign_extension
  | Target_layout.C_zero_extension -> Ir.Zero_extension

let c_extension = function
  | Hir.Bool -> ir_extension Target_layout.current.c_abi_bool_extension
  | Hir.Int kind ->
      Target_layout.c_integer_extension Target_layout.current
        ~signed:
          (match kind with Hir.I8 | I16 | I32 | I64 | Isize -> true | _ -> false)
        (Hir.int_bytes kind * 8)
      |> ir_extension
  | _ -> Ir.No_extension

let align s t =
  match Hir.layout s.structs t with
  | Ok (_, a) -> a
  | Error m ->
      failwith
        (Printf.sprintf "internal error: type `%s` has no layout: %s" (Hir.ty_name t) m)

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
  | Hir.Vec (_, Hir.Int (Hir.U8 | U16 | U32 | U64 | Usize)) -> true
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

let width = function Ir.I1 -> 1 | I8 -> 8 | I16 -> 16 | I32 -> 32 | I64 -> 64 | _ -> 0

let coerce s v target =
  let from = value_ty v in
  if from = target then v
  else
    let id = fresh s in
    let k = if width from < width target then "zext" else "trunc" in
    emit s (Ir.Cast (id, k, from, v, target));
    Ir.Local (id, target)

let splat_scalar s vector_ty elem_ty value =
  let value = coerce s value elem_ty in
  let inserted = fresh s in
  emit s
    (Ir.Insert (inserted, vector_ty, Ir.Undef vector_ty, Ir.Const (Ir.I32, 0L), value));
  let shuffled = fresh s in
  emit s (Ir.Shuffle_zero (shuffled, vector_ty, Ir.Local (inserted, vector_ty)));
  Ir.Local (shuffled, vector_ty)

let shift_amount s target value =
  let elem_ty = match target with Ir.Vector (_, t) -> t | t -> t in
  let value = coerce s value elem_ty in
  let bits = width elem_ty in
  let mask = Int64.of_int (bits - 1) in
  let id = fresh s in
  emit s (Ir.Bin (id, Ir.And, elem_ty, value, Ir.Const (elem_ty, mask)));
  let value = Ir.Local (id, elem_ty) in
  match target with Ir.Vector _ -> splat_scalar s target elem_ty value | _ -> value

let rec intrinsic_suffix = function
  | Ir.I8 -> "i8"
  | Ir.I16 -> "i16"
  | Ir.I32 -> "i32"
  | Ir.I64 -> "i64"
  | Ir.Vector (lanes, elem) -> Printf.sprintf "v%d%s" lanes (intrinsic_suffix elem)
  | _ -> invalid_arg "intrinsic_suffix: non-integer type"

let value_const s t value =
  match t with
  | Ir.Vector (_, elem) ->
      let inserted = fresh s in
      emit s
        (Ir.Insert
           (inserted, t, Ir.Undef t, Ir.Const (Ir.I32, 0L), Ir.Const (elem, value)));
      let shuffled = fresh s in
      emit s (Ir.Shuffle_zero (shuffled, t, Ir.Local (inserted, t)));
      Ir.Local (shuffled, t)
  | _ -> Ir.Const (t, value)

let compare s cmp t lhs rhs =
  let id = fresh s in
  emit s (Ir.Cmp (id, cmp, t, lhs, rhs));
  match t with
  | Ir.Vector (lanes, _) -> Ir.Local (id, Ir.Vector (lanes, Ir.I1))
  | _ -> Ir.Local (id, Ir.I1)

let reduce_any s value =
  match value_ty value with
  | Ir.I1 -> value
  | Ir.Vector (lanes, Ir.I1) as t ->
      let id = fresh s in
      emit s
        (Ir.Call
           ( Some id,
             Ir.No_extension,
             Ir.I1,
             Printf.sprintf "llvm.vector.reduce.or.v%di1" lanes,
             [ (t, Ir.No_extension, value) ] ));
      Ir.Local (id, Ir.I1)
  | _ -> invalid_arg "reduce_any: non-boolean value"

let combine_conditions s lhs rhs =
  let id = fresh s in
  emit s (Ir.Bin (id, Ir.And, Ir.I1, lhs, rhs));
  Ir.Local (id, Ir.I1)

let either_condition s lhs rhs =
  let id = fresh s in
  emit s (Ir.Bin (id, Ir.Or, Ir.I1, lhs, rhs));
  Ir.Local (id, Ir.I1)

let emit_trap s =
  emit s Ir.Trap;
  s.current.term := Some Ir.Unreachable

let guard_condition s condition =
  let trap = fresh_block s and valid = fresh_block s in
  s.current.term := Some (Ir.CondBr (condition, trap.id, valid.id));
  s.current <- trap;
  emit_trap s;
  s.current <- valid

let signed_type = function
  | Hir.Int (Hir.I8 | I16 | I32 | I64 | Isize)
  | Hir.Vec (_, Hir.Int (Hir.I8 | I16 | I32 | I64 | Isize)) ->
      true
  | _ -> false

let division_guard s source_ty binop ir_ty lhs rhs =
  let zero_condition =
    reduce_any s (compare s Ir.Eq ir_ty rhs (value_const s ir_ty 0L))
  in
  let condition =
    if binop = Ir.Sdiv && signed_type source_ty then
      let bits = width (match ir_ty with Ir.Vector (_, t) -> t | t -> t) in
      let minimum = Int64.shift_left 1L (bits - 1) in
      let lhs_minimum =
        reduce_any s (compare s Ir.Eq ir_ty lhs (value_const s ir_ty minimum))
      in
      let rhs_minus_one =
        reduce_any s (compare s Ir.Eq ir_ty rhs (value_const s ir_ty Int64.minus_one))
      in
      either_condition s zero_condition (combine_conditions s lhs_minimum rhs_minus_one)
    else zero_condition
  in
  guard_condition s condition

let emit_binary s source_ty op ir_ty lhs rhs =
  let binop = bin_for source_ty op in
  let division = match binop with Ir.Sdiv | Srem | Udiv | Urem -> true | _ -> false in
  if division then division_guard s source_ty binop ir_ty lhs rhs;
  if binop = Ir.Srem then (
    let is_minus_one =
      compare s Ir.Eq ir_ty rhs (value_const s ir_ty Int64.minus_one)
    in
    let safe_rhs_id = fresh s in
    let safe_rhs = value_const s ir_ty 1L in
    emit s (Ir.Select (safe_rhs_id, is_minus_one, safe_rhs, rhs));
    let raw_id = fresh s in
    emit s (Ir.Bin (raw_id, binop, ir_ty, lhs, Ir.Local (safe_rhs_id, ir_ty)));
    let result_id = fresh s in
    emit s
      (Ir.Select
         (result_id, is_minus_one, value_const s ir_ty 0L, Ir.Local (raw_id, ir_ty)));
    Ir.Local (result_id, ir_ty))
  else
    let result = fresh s in
    emit s (Ir.Bin (result, binop, ir_ty, lhs, rhs));
    Ir.Local (result, ir_ty)

let truth s v =
  match value_ty v with
  | Ir.I1 -> v
  | t ->
      let id = fresh s in
      emit s
        (Ir.Cmp (id, Ir.Ne, t, v, match t with Ir.Ptr _ -> Ir.Null t | _ -> zero t));
      Ir.Local (id, Ir.I1)

let bind_local s binding value =
  let old = Hashtbl.find_opt s.env binding.Hir.id in
  (match s.env_scopes with
  | scope :: rest -> s.env_scopes <- ((binding.id, old) :: scope) :: rest
  | [] -> ());
  Hashtbl.replace s.env binding.id value

let push_scope s =
  s.env_scopes <- [] :: s.env_scopes;
  s.defer_scopes <- [] :: s.defer_scopes

let pop_scope s =
  match (s.env_scopes, s.defer_scopes) with
  | scope :: er, _ :: dr ->
      List.iter
        (fun (n, old) ->
          match old with
          | None -> Hashtbl.remove s.env n
          | Some v -> Hashtbl.replace s.env n v)
        scope;
      s.env_scopes <- er;
      s.defer_scopes <- dr
  | _ -> ()

let find_struct s n = List.find_opt (fun (d : Hir.struct_def) -> d.name = n) s.structs

let normalize_index s expression value =
  let bits = function
    | Hir.Int (Hir.U8 | Hir.I8) -> 8
    | Hir.Int (Hir.U16 | Hir.I16) -> 16
    | Hir.Int (Hir.U32 | Hir.I32) -> 32
    | Hir.Int (Hir.U64 | Hir.I64) -> 64
    | Hir.Int (Hir.Usize | Hir.Isize) -> Target_layout.current.pointer_size * 8
    | _ -> 64
  in
  let t = Hir.expr_ty expression in
  if bits t = 64 then value
  else
    let id = fresh s in
    let kind = if unsigned t then "zext" else "sext" in
    emit s (Ir.Cast (id, kind, value_ty value, value, Ir.I64));
    Ir.Local (id, Ir.I64)

let rec expr s = function
  | Hir.EInt (v, t, _) -> Ok (Ir.Const (ty t, v))
  | Hir.EBool (v, _) -> Ok (Ir.Const (Ir.I1, if v then 1L else 0L))
  | Hir.Null (t, _) -> Ok (Ir.Null (ty t))
  | Hir.EString (i, sp) ->
      if i < 0 || i >= List.length s.strings then error sp "missing interned string"
      else
        let id = fresh s in
        emit s (Ir.String_ptr (id, i, String.length (List.nth s.strings i)));
        Ok (Ir.Local (id, Ir.Ptr Ir.I8))
  | Hir.Local (local, sp) -> (
      match Hashtbl.find_opt s.env local.id with
      | None -> error sp ("unknown lowering local `" ^ local.name ^ "`")
      | Some p ->
          let id = fresh s in
          emit s (Ir.Load (id, ty local.ty, p, align s local.ty));
          Ok (Ir.Local (id, ty local.ty)))
  | Hir.Unary (op, e, t, _) -> (
      let* v = expr s e in
      let rt = ty t in
      match op with
      | Ast.Neg ->
          let id = fresh s in
          emit s (Ir.Bin (id, Sub, rt, zero rt, v));
          Ok (Ir.Local (id, rt))
      | Bit_not ->
          let id = fresh s in
          emit s (Ir.Bin (id, Xor, rt, v, Ir.Const (rt, ones rt)));
          Ok (Ir.Local (id, rt))
      | Not ->
          let b = truth s v in
          let id = fresh s in
          emit s (Ir.Cmp (id, Ir.Eq, Ir.I1, b, Ir.Const (Ir.I1, 0L)));
          Ok (Ir.Local (id, Ir.I1)))
  | Hir.Binary (((Ast.And | Ast.Or) as op), a, b, _, _) -> lower_short s op a b
  | Hir.Binary (op, a, b, t, _) ->
      let* x = expr s a in
      let* y = expr s b in
      if List.mem op [ Ast.Eq; Ne; Lt; Le; Gt; Ge ] then (
        let id = fresh s in
        emit s (Ir.Cmp (id, cmp_for (Hir.expr_ty a) op, value_ty x, x, y));
        Ok (Ir.Local (id, ty t)))
      else Ok (emit_binary s (Hir.expr_ty a) op (ty t) x y)
  | Hir.Call (Hir.User n, args, t, _) ->
      let* vs = exprs s args in
      let c_function = List.find_opt (fun (f : Hir.func) -> f.name = n) s.c_functions in
      let av =
        List.mapi
          (fun index v ->
            let extension =
              match c_function with
              | Some f -> (
                  match List.nth_opt f.params index with
                  | Some formal -> c_extension formal.ty
                  | None -> Ir.No_extension)
              | None -> Ir.No_extension
            in
            (value_ty v, extension, v))
          vs
      in
      let ret_extension =
        match c_function with Some _ -> c_extension t | None -> Ir.No_extension
      in
      if t = Hir.Void then (
        emit s (Ir.Call (None, Ir.No_extension, Ir.Void, n, av));
        Ok (Ir.Const (Ir.I1, 0L)))
      else
        let id = fresh s in
        emit s (Ir.Call (Some id, ret_extension, ty t, n, av));
        Ok (Ir.Local (id, ty t))
  | Hir.Call (Hir.Builtin b, args, t, _) -> lower_builtin s b args t
  | Hir.Cast (kind, e, t, _) ->
      let* v = expr s e in
      let st = value_ty v and dt = ty t in
      let vector_bitcast =
        match (kind, Hir.expr_ty e, t) with
        | Ast.Bitcast, Hir.Vec _, _ | Ast.Bitcast, _, Hir.Vec _ -> true
        | _ -> false
      in
      if st = dt && not vector_bitcast then Ok v
      else
        let k =
          match (st, dt) with
          | Ir.Ptr _, Ir.Ptr _ -> ""
          | Ir.Ptr _, _ -> "ptrtoint"
          | _, Ir.Ptr _ -> "inttoptr"
          | _, Ir.I1 -> "bool"
          | Ir.I1, _ -> "zext"
          | _ -> (
              match kind with
              | Ast.Zext -> "zext"
              | Sext -> "sext"
              | Trunc -> if st = Ir.I1 then "zext" else "trunc"
              | Bitcast -> "bitcast")
        in
        if k = "" then Ok v
        else if k = "bool" then Ok (truth s v)
        else
          let id = fresh s in
          emit s (Ir.Cast (id, k, st, v, dt));
          Ok (Ir.Local (id, dt))
  | Hir.Deref (e, t, _) ->
      let* p = expr s e in
      let id = fresh s in
      emit s (Ir.Load (id, ty t, p, align s t));
      Ok (Ir.Local (id, ty t))
  | Hir.Address (e, _, _) -> address s e
  | Hir.Ptr_add (bytes, p, o, _, _) ->
      let* pv = expr s p in
      let* ov = expr s o in
      let base =
        if bytes then Ir.I8
        else match Hir.expr_ty p with Hir.Ptr t | Hir.ConstPtr t -> ty t | _ -> Ir.I8
      in
      let id = fresh s in
      emit s (Ir.Gep (id, base, pv, [ Ir.Index ov ]));
      Ok (Ir.Local (id, Ir.Ptr base))
  | Hir.Index (a, i, t, _) -> (
      match Hir.expr_ty a with
      | Hir.Vec _ ->
          let* av = expr s a in
          let* iv = expr s i in
          let iv = normalize_index s i iv in
          let id = fresh s in
          emit s (Ir.Extract (id, value_ty av, av, iv));
          Ok (Ir.Local (id, ty t))
      | _ ->
          let* p = index_address s a i in
          let id = fresh s in
          emit s (Ir.Load (id, ty t, p, align s t));
          Ok (Ir.Local (id, ty t)))
  | Hir.Field (a, _, t, off, _) ->
      let* p = field_address s a off in
      let id = fresh s in
      emit s (Ir.Load (id, ty t, p, align s t));
      Ok (Ir.Local (id, ty t))
  | Hir.Sizeof (_, n, _) | Hir.Alignof (_, n, _) | Hir.Offsetof (_, _, n, _) ->
      Ok (Ir.Const (ty (Hir.Int Hir.Usize), Int64.of_int n))
  | Hir.Ternary (c, a, b, t, _) -> lower_ternary s c a b t
  | Hir.Splat (e, t, _) ->
      let* x = expr s e in
      let vt = ty t in
      let i1 = fresh s in
      emit s (Ir.Insert (i1, vt, Ir.Undef vt, Ir.Const (Ir.I32, 0L), x));
      let i2 = fresh s in
      emit s (Ir.Shuffle_zero (i2, vt, Ir.Local (i1, vt)));
      Ok (Ir.Local (i2, vt))
  | Hir.Const_array (n, t, _) ->
      let ptr_id = fresh s in
      emit s (Ir.Global_ptr (ptr_id, n, ty t));
      let value_id = fresh s in
      emit s (Ir.Load (value_id, ty t, Ir.Local (ptr_id, Ir.Ptr (ty t)), align s t));
      Ok (Ir.Local (value_id, ty t))
  | Hir.Struct_lit (n, xs, t, sp) -> (
      match find_struct s n with
      | None -> error sp ("unknown struct `" ^ n ^ "`")
      | Some d ->
          let st = ty t in
          let slot = fresh s in
          emit s (Ir.Alloca (slot, st, d.align));
          let p = Ir.Local (slot, Ir.Ptr st) in
          let rec fields fs es =
            match (fs, es) with
            | [], [] -> Ok ()
            | f :: ft, e :: et ->
                let id = fresh s in
                emit s
                  (Ir.Gep
                     ( id,
                       Ir.I8,
                       p,
                       [ Ir.Index (Ir.Const (Ir.I64, Int64.of_int f.Hir.offset)) ] ));
                let* v = expr s e in
                emit s
                  (Ir.Store (ty f.ty, v, Ir.Local (id, Ir.Ptr (ty f.ty)), align s f.ty));
                fields ft et
            | _ -> error sp "struct literal arity mismatch"
          in
          let* () = fields d.fields xs in
          let id = fresh s in
          emit s (Ir.Load (id, st, p, d.align));
          Ok (Ir.Local (id, st)))

and exprs s xs = Result_list.map (expr s) xs

and lower_builtin s b args t =
  let* vs = exprs s args in
  let rt = ty t in
  match (b, vs) with
  | (Hir.Shl | Lshr | Ashr), [ x; n ] ->
      let xt = value_ty x in
      let n = shift_amount s xt n in
      let id = fresh s in
      emit s
        (Ir.Bin (id, (match b with Shl -> Ir.Shl | Lshr -> Lshr | _ -> Ashr), xt, x, n));
      Ok (Ir.Local (id, xt))
  | (Rotl | Rotr), [ x; n ] ->
      let n = shift_amount s rt n in
      let id = fresh s in
      let base = if b = Rotl then "llvm.fshl." else "llvm.fshr." in
      emit s
        (Ir.Call
           ( Some id,
             Ir.No_extension,
             rt,
             base ^ intrinsic_suffix rt,
             [
               (rt, Ir.No_extension, x);
               (rt, Ir.No_extension, x);
               (rt, Ir.No_extension, n);
             ] ));
      Ok (Ir.Local (id, rt))
  | Popcount, [ x ] ->
      let id = fresh s in
      emit s
        (Ir.Call
           ( Some id,
             Ir.No_extension,
             rt,
             "llvm.ctpop." ^ intrinsic_suffix rt,
             [ (rt, Ir.No_extension, x) ] ));
      Ok (Ir.Local (id, rt))
  | (Ctz | Clz), [ x ] ->
      let id = fresh s in
      let base = if b = Ctz then "llvm.cttz." else "llvm.ctlz." in
      emit s
        (Ir.Call
           ( Some id,
             Ir.No_extension,
             rt,
             base ^ intrinsic_suffix rt,
             [
               (rt, Ir.No_extension, x); (Ir.I1, Ir.No_extension, Ir.Const (Ir.I1, 0L));
             ] ));
      Ok (Ir.Local (id, rt))
  | _ -> error Span.synthetic "invalid builtin arity"

and lower_short s op a b =
  let* lv = expr s a in
  let lv = truth s lv in
  let pred = s.current.id and rhs = fresh_block s and join = fresh_block s in
  s.current.term :=
    Some
      (Ir.CondBr
         ( lv,
           (if op = Ast.And then rhs.id else join.id),
           if op = Ast.And then join.id else rhs.id ));
  s.current <- rhs;
  let* rv = expr s b in
  let rv = truth s rv in
  let rp = s.current.id in
  if open_block s then s.current.term := Some (Ir.Br join.id);
  s.current <- join;
  let id = fresh s in
  emit s
    (Ir.Phi
       ( id,
         Ir.I1,
         [
           ((if op = Ast.And then Ir.Const (Ir.I1, 0L) else Ir.Const (Ir.I1, 1L)), pred);
           (rv, rp);
         ] ));
  Ok (Ir.Local (id, Ir.I1))

and lower_ternary s c a b t =
  let* cv = expr s c in
  let cv = truth s cv in
  let tb = fresh_block s and eb = fresh_block s and join = fresh_block s in
  s.current.term := Some (Ir.CondBr (cv, tb.id, eb.id));
  s.current <- tb;
  let* tv = expr s a in
  let tp = s.current.id in
  if open_block s then s.current.term := Some (Ir.Br join.id);
  s.current <- eb;
  let* ev = expr s b in
  let ep = s.current.id in
  if open_block s then s.current.term := Some (Ir.Br join.id);
  s.current <- join;
  let id = fresh s in
  emit s (Ir.Phi (id, ty t, [ (tv, tp); (ev, ep) ]));
  Ok (Ir.Local (id, ty t))

and materialize s e =
  let* v = expr s e in
  let ht = Hir.expr_ty e in
  let id = fresh s in
  emit s (Ir.Alloca (id, ty ht, align s ht));
  let p = Ir.Local (id, Ir.Ptr (ty ht)) in
  emit s (Ir.Store (ty ht, v, p, align s ht));
  Ok p

and address s e =
  match e with
  | Hir.Local (local, sp) -> (
      match Hashtbl.find_opt s.env local.id with
      | Some v -> Ok v
      | None -> error sp ("unknown local `" ^ local.name ^ "`"))
  | Hir.Const_array (n, t, _) ->
      let id = fresh s in
      emit s (Ir.Global_ptr (id, n, ty t));
      Ok (Ir.Local (id, Ir.Ptr (ty t)))
  | Hir.Deref (p, _, _) -> expr s p
  | Hir.Index (a, i, _, _) -> index_address s a i
  | Hir.Field (a, _, _, off, _) -> field_address s a off
  | _ -> materialize s e

and index_address s a i =
  match Hir.expr_ty a with
  | Hir.Array _ ->
      let* p = address s a in
      let* iv = expr s i in
      let iv = normalize_index s i iv in
      let id = fresh s in
      emit s (Ir.Gep (id, ty (Hir.expr_ty a), p, [ Ir.Zero; Ir.Index iv ]));
      Ok (Ir.Local (id, Ir.Ptr Ir.I8))
  | Hir.Ptr elem | Hir.ConstPtr elem ->
      let* p = expr s a in
      let* iv = expr s i in
      let iv = normalize_index s i iv in
      let id = fresh s in
      emit s (Ir.Gep (id, ty elem, p, [ Ir.Index iv ]));
      Ok (Ir.Local (id, Ir.Ptr (ty elem)))
  | _ -> error (Hir.expr_span a) "cannot take index address"

and field_address s a off =
  let* p =
    if
      match a with
      | Hir.Local _ | Deref _ | Index _ | Field _ | Const_array _ -> true
      | _ -> false
    then address s a
    else materialize s a
  in
  let id = fresh s in
  emit s (Ir.Gep (id, Ir.I8, p, [ Ir.Index (Ir.Const (Ir.I64, Int64.of_int off)) ]));
  Ok (Ir.Local (id, Ir.Ptr Ir.I8))

let rec emit_defer_body s body =
  List.fold_left
    (fun r st ->
      let* () = r in
      if open_block s then stmt s st else Ok ())
    (Ok ()) body

and emit_scope_defers s idx =
  match List.nth_opt s.defer_scopes idx with
  | None -> Ok ()
  | Some ds ->
      List.fold_left
        (fun r body ->
          let* () = r in
          emit_defer_body s body)
        (Ok ()) ds

and unwind s keep =
  let count = List.length s.defer_scopes - keep in
  let scopes =
    let rec take n xs =
      if n <= 0 then [] else match xs with [] -> [] | x :: xt -> x :: take (n - 1) xt
    in
    take count s.defer_scopes
  in
  List.fold_left
    (fun r ds ->
      let* () = r in
      List.fold_left
        (fun rr body ->
          let* () = rr in
          emit_defer_body s body)
        (Ok ()) ds)
    (Ok ()) scopes

and scoped s xs =
  push_scope s;
  let* () = stmt_list s xs in
  let* () = if open_block s then emit_scope_defers s 0 else Ok () in
  pop_scope s;
  Ok ()

and stmt s = function
  | Hir.Let (local, init, _) -> (
      let id = fresh s in
      emit_entry s (Ir.Alloca (id, ty local.ty, align s local.ty));
      let p = Ir.Local (id, Ir.Ptr (ty local.ty)) in
      bind_local s local p;
      match init with
      | None -> Ok ()
      | Some e ->
          let* v = expr s e in
          emit s (Ir.Store (ty local.ty, v, p, align s local.ty));
          Ok ())
  | Hir.Assign (target, e, _) -> (
      match target with
      | Hir.AIndex (a, i) when match Hir.expr_ty a with Hir.Vec _ -> true | _ -> false
        ->
          let* p, source_ty, vt, iv = vector_lane s a i in
          let* x = expr s e in
          let loaded_id = fresh s in
          emit s (Ir.Load (loaded_id, vt, p, align s source_ty));
          let loaded = Ir.Local (loaded_id, vt) in
          let ins = fresh s in
          emit s (Ir.Insert (ins, vt, loaded, iv, x));
          emit s (Ir.Store (vt, Ir.Local (ins, vt), p, align s source_ty));
          Ok ()
      | _ ->
          let* p = target_address s target in
          let* v = expr s e in
          let t = Hir.expr_ty e in
          emit s (Ir.Store (ty t, v, p, align s t));
          Ok ())
  | Hir.Compound_assign (target, op, e, t, _) -> (
      match target with
      | Hir.AIndex (a, i) when match Hir.expr_ty a with Hir.Vec _ -> true | _ -> false
        ->
          let* p, source_ty, vt, iv = vector_lane s a i in
          let loaded_id = fresh s in
          emit s (Ir.Load (loaded_id, vt, p, align s source_ty));
          let loaded = Ir.Local (loaded_id, vt) in
          let eid = fresh s in
          emit s (Ir.Extract (eid, vt, loaded, iv));
          let it = ty t in
          let old = Ir.Local (eid, it) in
          let* rhs = expr s e in
          let value = emit_binary s t op it old rhs in
          let latest_id = fresh s in
          emit s (Ir.Load (latest_id, vt, p, align s source_ty));
          let latest = Ir.Local (latest_id, vt) in
          let ins = fresh s in
          emit s (Ir.Insert (ins, vt, latest, iv, value));
          emit s (Ir.Store (vt, Ir.Local (ins, vt), p, align s source_ty));
          Ok ()
      | _ ->
          let* p = target_address s target in
          let it = ty t in
          let id = fresh s in
          emit s (Ir.Load (id, it, p, align s t));
          let old = Ir.Local (id, it) in
          let* rhs = expr s e in
          let value = emit_binary s t op it old rhs in
          emit s (Ir.Store (it, value, p, align s t));
          Ok ())
  | Hir.Expr (e, _) ->
      let* _ = expr s e in
      Ok ()
  | Hir.Return (e, _) ->
      let* v =
        match e with
        | None -> Ok None
        | Some x ->
            let* y = expr s x in
            Ok (Some (ty (Hir.expr_ty x), y))
      in
      let* () = unwind s 0 in
      s.current.term := Some (Ir.Ret v);
      Ok ()
  | Hir.Block (xs, _) -> scoped s xs
  | Hir.If (c, a, b, _) -> lower_if s c a b
  | Hir.Defer (xs, _) -> (
      match s.defer_scopes with
      | ds :: rest ->
          s.defer_scopes <- (xs :: ds) :: rest;
          Ok ()
      | [] -> error Span.synthetic "defer outside scope")
  | Hir.While (c, b, _) -> lower_while s c b
  | Hir.For (i, c, st, b, _) -> lower_for s i c st b
  | Hir.Switch (e, arms, d, _) -> lower_switch s e arms d
  | Hir.Break sp -> (
      match s.loops with
      | l :: _ ->
          let* () = unwind s l.keep_scopes in
          s.current.term := Some (Ir.Br l.break_to);
          Ok ()
      | [] -> error sp "break outside loop or switch")
  | Hir.Continue sp -> (
      match s.loops with
      | l :: _ ->
          let* () = unwind s l.keep_scopes in
          s.current.term := Some (Ir.Br l.continue_to);
          Ok ()
      | [] -> error sp "continue outside loop")

and stmt_list s xs =
  List.fold_left
    (fun r st ->
      let* () = r in
      if open_block s then stmt s st else Ok ())
    (Ok ()) xs

and target_address s = function
  | Hir.ALocal local -> (
      match Hashtbl.find_opt s.env local.id with
      | Some p -> Ok p
      | None -> error Span.synthetic ("unknown local `" ^ local.name ^ "`"))
  | Hir.ADeref e -> expr s e
  | Hir.AIndex (a, i) -> index_address s a i
  | Hir.AField (a, _, off) -> field_address s a off

and vector_lane s aggregate index =
  let source_ty = Hir.expr_ty aggregate in
  let* pointer = address s aggregate in
  let vector_ty = ty source_ty in
  let* iv = expr s index in
  Ok (pointer, source_ty, vector_ty, normalize_index s index iv)

and lower_if s c a b =
  let* cv = expr s c in
  let cv = truth s cv in
  let tb = fresh_block s and eb = fresh_block s and join = fresh_block s in
  s.current.term := Some (Ir.CondBr (cv, tb.id, eb.id));
  s.current <- tb;
  let* () = scoped s a in
  if open_block s then s.current.term := Some (Ir.Br join.id);
  s.current <- eb;
  let* () = match b with None -> Ok () | Some xs -> scoped s xs in
  if open_block s then s.current.term := Some (Ir.Br join.id);
  s.current <- join;
  Ok ()

and lower_while s c body =
  let head = fresh_block s and bb = fresh_block s and exit = fresh_block s in
  s.current.term := Some (Ir.Br head.id);
  s.current <- head;
  let* cv = expr s c in
  let cv = truth s cv in
  s.current.term := Some (Ir.CondBr (cv, bb.id, exit.id));
  s.current <- bb;
  s.loops <-
    {
      break_to = exit.id;
      continue_to = head.id;
      keep_scopes = List.length s.defer_scopes;
    }
    :: s.loops;
  let* () = scoped s body in
  s.loops <- List.tl s.loops;
  if open_block s then s.current.term := Some (Ir.Br head.id);
  s.current <- exit;
  Ok ()

and lower_for s init cond step body =
  push_scope s;
  let lowered =
    let* () = match init with None -> Ok () | Some x -> stmt s x in
    let head = fresh_block s
    and bb = fresh_block s
    and sb = fresh_block s
    and exit = fresh_block s in
    s.current.term := Some (Ir.Br head.id);
    s.current <- head;
    let* () =
      match cond with
      | None ->
          s.current.term := Some (Ir.Br bb.id);
          Ok ()
      | Some c ->
          let* v = expr s c in
          s.current.term := Some (Ir.CondBr (truth s v, bb.id, exit.id));
          Ok ()
    in
    s.current <- bb;
    s.loops <-
      {
        break_to = exit.id;
        continue_to = sb.id;
        keep_scopes = List.length s.defer_scopes;
      }
      :: s.loops;
    let* () = scoped s body in
    s.loops <- List.tl s.loops;
    if open_block s then s.current.term := Some (Ir.Br sb.id);
    s.current <- sb;
    let* () = match step with None -> Ok () | Some x -> stmt s x in
    if open_block s then s.current.term := Some (Ir.Br head.id);
    s.current <- exit;
    let* () = if open_block s then emit_scope_defers s 0 else Ok () in
    Ok ()
  in
  pop_scope s;
  lowered

and lower_switch s e arms default =
  let* v = expr s e in
  let join = fresh_block s in
  let blocks = List.map (fun _ -> fresh_block s) arms in
  let db = fresh_block s in
  let cases =
    List.map2
      (fun (k, _) b ->
        match k with
        | Hir.EInt (n, _, _) -> (n, b.id)
        | Hir.EBool (x, _) -> ((if x then 1L else 0L), b.id)
        | _ -> (0L, b.id))
      arms blocks
  in
  s.current.term := Some (Ir.Switch (value_ty v, v, cases, db.id));
  let rec each as_ bs =
    match (as_, bs) with
    | [], [] -> Ok ()
    | (_, body) :: at, b :: bt ->
        s.current <- b;
        let* () = scoped s body in
        if open_block s then s.current.term := Some (Ir.Br join.id);
        each at bt
    | _ -> Ok ()
  in
  let* () = each arms blocks in
  s.current <- db;
  let* () = match default with None -> Ok () | Some xs -> scoped s xs in
  if open_block s then s.current.term := Some (Ir.Br join.id);
  s.current <- join;
  Ok ()

let lower_func structs strings c_functions f =
  let* () =
    Result_list.iter
      (fun (local : Hir.local) -> layout_ok structs local.ty)
      f.Hir.params
  in
  let* () = if f.Hir.ret = Hir.Void then Ok () else layout_ok structs f.Hir.ret in
  let params =
    List.map
      (fun (local : Hir.local) ->
        ({
           Ir.name = local.name;
           ty = ty local.ty;
           extension =
             (if f.linkage = Hir.External_c then c_extension local.ty
              else Ir.No_extension);
         }
          : Ir.param))
      f.Hir.params
  in
  let ret_extension =
    if f.linkage = Hir.External_c then c_extension f.ret else Ir.No_extension
  in
  match f.body with
  | Hir.Asm raw ->
      Ok
        {
          Ir.name = f.name;
          params;
          ret = ty f.ret;
          ret_extension;
          blocks = [];
          linkage = Ir.External;
          variadic = f.variadic;
          asm_body = Some raw;
        }
  | Hir.Declaration ->
      Ok
        {
          Ir.name = f.name;
          params;
          ret = ty f.ret;
          ret_extension;
          blocks = [];
          linkage = Ir.External;
          variadic = f.variadic;
          asm_body = None;
        }
  | Hir.Statements body ->
      let entry = { id = 0; instrs = Queue.create (); term = ref None } in
      let blocks = Queue.create () in
      Queue.add entry blocks;
      let s =
        {
          next_value = 0;
          next_block = 1;
          blocks;
          entry;
          current = entry;
          env = Hashtbl.create 32;
          env_scopes = [];
          ret = ty f.ret;
          structs;
          strings;
          c_functions;
          defer_scopes = [];
          loops = [];
        }
      in
      push_scope s;
      List.iter
        (fun (local : Hir.local) ->
          let id = fresh s in
          emit s (Ir.Alloca (id, ty local.ty, align s local.ty));
          let p = Ir.Local (id, Ir.Ptr (ty local.ty)) in
          bind_local s local p;
          emit s
            (Ir.Store
               (ty local.ty, Ir.Param (local.name, ty local.ty), p, align s local.ty)))
        f.params;
      let* () = scoped s body in
      let* () = if open_block s then emit_scope_defers s 0 else Ok () in
      if open_block s then
        s.current.term := Some (if s.ret = Ir.Void then Ir.Ret None else Ir.Unreachable);
      pop_scope s;
      let blocks =
        List.map
          (fun b ->
            ({
               Ir.id = b.id;
               label = "b" ^ string_of_int b.id;
               instrs = List.of_seq (Queue.to_seq b.instrs);
               terminator = Option.value ~default:Ir.Unreachable !(b.term);
             }
              : Ir.block))
          (List.of_seq (Queue.to_seq s.blocks))
      in
      Ok
        {
          Ir.name = f.name;
          params;
          ret = ty f.ret;
          ret_extension;
          blocks;
          linkage =
            (if f.name = "main" then Ir.External
             else if f.linkage = Hir.Internal then Ir.Internal
             else External);
          variadic = f.variadic;
          asm_body = None;
        }

let intrinsic_decls funcs =
  let calls =
    List.concat_map
      (fun (f : Ir.func) ->
        List.concat_map
          (fun (b : Ir.block) ->
            List.filter_map
              (function
                | Ir.Call (_, _, ret, name, args)
                  when String.starts_with ~prefix:"llvm." name ->
                    Some (name, ret, List.map (fun (ty, _, _) -> ty) args)
                | Ir.Trap -> Some ("llvm.trap", Ir.Void, [])
                | _ -> None)
              b.instrs)
          f.blocks)
      funcs
  in
  let unique = Hashtbl.create 16 in
  List.iter (fun (name, ret, args) -> Hashtbl.replace unique name (ret, args)) calls;
  Hashtbl.to_seq unique
  |> Seq.map (fun (name, (ret, args)) ->
      {
        Ir.name;
        params =
          List.mapi
            (fun i ty ->
              { Ir.name = "a" ^ string_of_int i; ty; extension = Ir.No_extension })
            args;
        ret;
        ret_extension = Ir.No_extension;
        blocks = [];
        linkage = Ir.External;
        variadic = false;
        asm_body = None;
      })
  |> List.of_seq
  |> List.sort (fun (a : Ir.func) b -> String.compare a.name b.name)

let lower (p : Hir.program) =
  let no_layout t m =
    error Span.synthetic
      (Printf.sprintf "internal error: type `%s` has no layout: %s" (Hir.ty_name t) m)
  in
  let field_size t =
    match Hir.layout p.structs t with
    | Ok (s, _) when s >= 0 -> Ok s
    | Ok _ -> no_layout t "negative object size"
    | Error m -> no_layout t m
  in
  let field_align t =
    match Hir.layout p.structs t with
    | Ok (_, a) -> (
        match Target_layout.validate_type_alignment Target_layout.current a with
        | Ok () -> Ok a
        | Error m -> no_layout t m)
    | Error m -> no_layout t m
  in
  let malformed_struct (d : Hir.struct_def) message =
    error Span.synthetic
      (Printf.sprintf "internal error: struct `%s` has malformed layout: %s" d.name
         message)
  in
  let* funcs =
    let c_functions =
      List.filter (fun (f : Hir.func) -> f.linkage = Hir.External_c) p.funcs
    in
    Result_list.map
      (fun (f : Hir.func) -> lower_func p.Hir.structs p.strings c_functions f)
      p.funcs
  in
  let* structs =
    Result_list.map
      (fun (d : Hir.struct_def) ->
        let* () =
          match Target_layout.validate_type_alignment Target_layout.current d.align with
          | Ok () -> Ok ()
          | Error message -> malformed_struct d message
        in
        let* fields, used, natural_align =
          let rec go offset out used natural_align = function
            | [] -> Ok (List.rev out, used, natural_align)
            | (f : Hir.field) :: rest ->
                let* size = field_size f.ty in
                let* align = field_align f.ty in
                if f.offset < offset then
                  malformed_struct d
                    (Printf.sprintf "field `%s` overlaps a preceding field" f.name)
                else if f.offset mod align <> 0 then
                  malformed_struct d
                    (Printf.sprintf "field `%s` is not aligned to %d" f.name align)
                else if f.offset > max_int - size then
                  malformed_struct d
                    (Printf.sprintf "field `%s` end offset overflows" f.name)
                else
                  let field_end = f.offset + size in
                  let padding = f.offset - offset in
                  let out =
                    if padding > 0 then Ir.Array (padding, Ir.I8) :: out else out
                  in
                  go field_end (ty f.ty :: out) (max used field_end)
                    (max natural_align align) rest
          in
          go 0 [] 0 1 d.fields
        in
        if d.align < natural_align then
          malformed_struct d
            (Printf.sprintf "alignment %d is less than natural alignment %d" d.align
               natural_align)
        else if d.size < used then malformed_struct d "size is smaller than its fields"
        else if d.size mod d.align <> 0 then
          malformed_struct d
            (Printf.sprintf "size %d is not a multiple of alignment %d" d.size d.align)
        else
          let fields =
            if d.align > natural_align then
              Ir.Array (0, Ir.Vector (d.align, Ir.I8)) :: fields
            else fields
          in
          Ok { Ir.name = d.name; fields; tail_padding = d.size - used })
      p.structs
  in
  let strings =
    List.mapi
      (fun i x -> Ir.String_global { name = ".str." ^ string_of_int i; bytes = x })
      p.strings
  in
  let* arrays =
    Result_list.map
      (fun (a : Hir.const_arr_def) ->
        match a.ty with
        | Hir.Array (_, e) ->
            let* align = field_align e in
            Ok
              (Ir.Array_global { name = a.name; elem_ty = ty e; elems = a.elems; align })
        | _ ->
            Ok
              (Ir.Array_global { name = a.name; elem_ty = Ir.I8; elems = []; align = 1 }))
      p.const_arrays
  in
  Ok
    {
      Ir.target_triple = Target_layout.current.triple;
      data_layout = Target_layout.current.llvm_data_layout;
      structs;
      globals = strings @ arrays;
      funcs = funcs @ intrinsic_decls funcs;
      no_inline_function = None;
    }
