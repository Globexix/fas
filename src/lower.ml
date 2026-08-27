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
  | Hir.Int Hir.U64 | Hir.Int Hir.I64 | Hir.Int Hir.Usize | Hir.Int Hir.Isize -> I64
  | Hir.Ptr t -> Ir.Ptr (ty t)
  | Hir.Vec (n, t) -> Ir.Vector (n, ty t)
  | Hir.Array (n, t) -> Ir.Array (n, ty t)
  | Hir.Struct n -> Ir.Struct n
  | Hir.Opaque n -> Ir.Struct n
  | Hir.Void -> Ir.Void

let align s t = match Hir.layout s.structs t with Ok (_, a) -> a | Error _ -> 1
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

let emit_binary s source_ty op ir_ty lhs rhs =
  let binop = bin_for source_ty op in
  match (binop, ir_ty) with
  | Ir.Srem, (Ir.I8 | Ir.I16 | Ir.I32 | Ir.I64) ->
      let is_minus_one = fresh s in
      emit s
        (Ir.Cmp (is_minus_one, Ir.Eq, ir_ty, rhs, Ir.Const (ir_ty, Int64.minus_one)));
      let special = fresh_block s and divide = fresh_block s and join = fresh_block s in
      s.current.term :=
        Some (Ir.CondBr (Ir.Local (is_minus_one, Ir.I1), special.id, divide.id));
      s.current <- special;
      if open_block s then s.current.term := Some (Ir.Br join.id);
      s.current <- divide;
      let result = fresh s in
      emit s (Ir.Bin (result, binop, ir_ty, lhs, rhs));
      let divide_value = Ir.Local (result, ir_ty) in
      let divide_pred = s.current.id in
      if open_block s then s.current.term := Some (Ir.Br join.id);
      s.current <- join;
      let selected = fresh s in
      emit s
        (Ir.Phi
           ( selected,
             ir_ty,
             [ (Ir.Const (ir_ty, 0L), special.id); (divide_value, divide_pred) ] ));
      Ir.Local (selected, ir_ty)
  | _ ->
      let result = fresh s in
      emit s (Ir.Bin (result, binop, ir_ty, lhs, rhs));
      Ir.Local (result, ir_ty)

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
  match target with
  | Ir.Vector (_, elem_ty) -> splat_scalar s target elem_ty value
  | _ -> coerce s value target

let rec intrinsic_suffix = function
  | Ir.I8 -> "i8"
  | Ir.I16 -> "i16"
  | Ir.I32 -> "i32"
  | Ir.I64 -> "i64"
  | Ir.Vector (lanes, elem) -> Printf.sprintf "v%d%s" lanes (intrinsic_suffix elem)
  | _ -> invalid_arg "intrinsic_suffix: non-integer type"

let truth s v =
  match value_ty v with
  | Ir.I1 -> v
  | t ->
      let id = fresh s in
      emit s
        (Ir.Cmp (id, Ir.Ne, t, v, match t with Ir.Ptr _ -> Ir.Null t | _ -> zero t));
      Ir.Local (id, Ir.I1)

let bind_local s name value =
  let old = Hashtbl.find_opt s.env name in
  (match s.env_scopes with
  | scope :: rest -> s.env_scopes <- ((name, old) :: scope) :: rest
  | [] -> ());
  Hashtbl.replace s.env name value

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

let rec expr s = function
  | Hir.EInt (v, t, _) -> Ok (Ir.Const (ty t, v))
  | Hir.EBool (v, _) -> Ok (Ir.Const (Ir.I1, if v then 1L else 0L))
  | Hir.Null (t, _) -> Ok (Ir.Null (ty t))
  | Hir.EString (i, sp) ->
      if i < 0 || i >= List.length s.strings then error sp "missing interned string"
      else
        let id = fresh s in
        emit s (Ir.String_ptr (id, i, String.length (List.nth s.strings i) + 1));
        Ok (Ir.Local (id, Ir.Ptr Ir.I8))
  | Hir.Local (n, t, sp) -> (
      match Hashtbl.find_opt s.env n with
      | None -> error sp ("unknown lowering local `" ^ n ^ "`")
      | Some p ->
          let id = fresh s in
          emit s (Ir.Load (id, ty t, p, align s t));
          Ok (Ir.Local (id, ty t)))
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
        Ok (Ir.Local (id, Ir.I1)))
      else Ok (emit_binary s (Hir.expr_ty a) op (ty t) x y)
  | Hir.Call (Hir.User n, args, t, _) ->
      let* vs = exprs s args in
      let av = List.map (fun v -> (value_ty v, v)) vs in
      if t = Hir.Void then (
        emit s (Ir.Call (None, Ir.Void, n, av));
        Ok (Ir.Const (Ir.I1, 0L)))
      else
        let id = fresh s in
        emit s (Ir.Call (Some id, ty t, n, av));
        Ok (Ir.Local (id, ty t))
  | Hir.Call (Hir.Builtin b, args, t, _) -> lower_builtin s b args t
  | Hir.Cast (kind, e, t, _) ->
      let* v = expr s e in
      let st = value_ty v and dt = ty t in
      if st = dt then Ok v
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
  | Hir.Deref (e, t, _) -> (
      let* p = expr s e in
      match t with
      | Hir.Array _ -> Ok p
      | _ ->
          let id = fresh s in
          emit s (Ir.Load (id, ty t, p, align s t));
          Ok (Ir.Local (id, ty t)))
  | Hir.Address (e, _, _) -> address s e
  | Hir.Ptr_add (bytes, p, o, _, _) ->
      let* pv = expr s p in
      let* ov = expr s o in
      let base =
        if bytes then Ir.I8
        else match Hir.expr_ty p with Hir.Ptr t -> ty t | _ -> Ir.I8
      in
      let id = fresh s in
      emit s (Ir.Gep (id, base, pv, [ Ir.Index ov ]));
      Ok (Ir.Local (id, Ir.Ptr base))
  | Hir.Index (a, i, t, _) -> (
      match Hir.expr_ty a with
      | Hir.Vec _ ->
          let* av = expr s a in
          let* iv = expr s i in
          let iv = coerce s iv Ir.I32 in
          let id = fresh s in
          emit s (Ir.Extract (id, value_ty av, av, iv));
          Ok (Ir.Local (id, ty t))
      | _ -> (
          let* p = index_address s a i in
          match t with
          | Hir.Array _ -> Ok p
          | _ ->
              let id = fresh s in
              emit s (Ir.Load (id, ty t, p, align s t));
              Ok (Ir.Local (id, ty t))))
  | Hir.Field (a, _, t, off, _) -> (
      let* p = field_address s a off in
      match t with
      | Hir.Array _ -> Ok p
      | _ ->
          let id = fresh s in
          emit s (Ir.Load (id, ty t, p, align s t));
          Ok (Ir.Local (id, ty t)))
  | Hir.Sizeof (_, n, _) | Hir.Alignof (_, n, _) | Hir.Offsetof (_, _, n, _) ->
      Ok (Ir.Const (Ir.I64, Int64.of_int n))
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
      let id = fresh s in
      emit s (Ir.Global_ptr (id, n, ty t));
      Ok (Ir.Local (id, Ir.Ptr (ty t)))
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
        (Ir.Call (Some id, rt, base ^ intrinsic_suffix rt, [ (rt, x); (rt, x); (rt, n) ]));
      Ok (Ir.Local (id, rt))
  | Popcount, [ x ] ->
      let id = fresh s in
      emit s (Ir.Call (Some id, rt, "llvm.ctpop." ^ intrinsic_suffix rt, [ (rt, x) ]));
      Ok (Ir.Local (id, rt))
  | (Ctz | Clz), [ x ] ->
      let id = fresh s in
      let base = if b = Ctz then "llvm.cttz." else "llvm.ctlz." in
      emit s
        (Ir.Call
           ( Some id,
             rt,
             base ^ intrinsic_suffix rt,
             [ (rt, x); (Ir.I1, Ir.Const (Ir.I1, 1L)) ] ));
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
  | Hir.Local (n, _, sp) -> (
      match Hashtbl.find_opt s.env n with
      | Some v -> Ok v
      | None -> error sp ("unknown local `" ^ n ^ "`"))
  | Hir.Const_array (n, t, _) ->
      let id = fresh s in
      emit s (Ir.Global_ptr (id, n, ty t));
      Ok (Ir.Local (id, Ir.Ptr (ty t)))
  | Hir.Deref (p, _, _) -> expr s p
  | Hir.Index (a, i, _, _) -> index_address s a i
  | Hir.Field (a, _, _, off, _) -> field_address s a off
  | _ -> materialize s e

and index_address s a i =
  let* iv = expr s i in
  match Hir.expr_ty a with
  | Hir.Array _ ->
      let* p = address s a in
      let id = fresh s in
      emit s (Ir.Gep (id, ty (Hir.expr_ty a), p, [ Ir.Zero; Ir.Index iv ]));
      Ok (Ir.Local (id, Ir.Ptr Ir.I8))
  | Hir.Ptr elem ->
      let* p = expr s a in
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
  | Hir.Let (n, t, init, _) -> (
      let id = fresh s in
      emit_entry s (Ir.Alloca (id, ty t, align s t));
      let p = Ir.Local (id, Ir.Ptr (ty t)) in
      bind_local s n p;
      match init with
      | None -> (
          match t with
          | Hir.Array _ | Hir.Struct _ | Hir.Vec _ ->
              emit s (Ir.Store (ty t, Ir.Zero (ty t), p, align s t));
              Ok ()
          | _ -> Ok ())
      | Some e ->
          let* v = expr s e in
          emit s (Ir.Store (ty t, v, p, align s t));
          Ok ())
  | Hir.Assign (target, e, _) -> (
      match target with
      | Hir.AIndex (a, i) when match Hir.expr_ty a with Hir.Vec _ -> true | _ -> false
        ->
          let* p, source_ty, vt, loaded, iv = vector_lane s a i in
          let* x = expr s e in
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
          let* p, source_ty, vt, loaded, iv = vector_lane s a i in
          let eid = fresh s in
          emit s (Ir.Extract (eid, vt, loaded, iv));
          let it = ty t in
          let old = Ir.Local (eid, it) in
          let* rhs = expr s e in
          let value = emit_binary s t op it old rhs in
          let ins = fresh s in
          emit s (Ir.Insert (ins, vt, loaded, iv, value));
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
  | Hir.ALocal n -> (
      match Hashtbl.find_opt s.env n with
      | Some p -> Ok p
      | None -> error Span.synthetic ("unknown local `" ^ n ^ "`"))
  | Hir.ADeref e -> expr s e
  | Hir.AIndex (a, i) -> index_address s a i
  | Hir.AField (a, _, off) -> field_address s a off

and vector_lane s aggregate index =
  let source_ty = Hir.expr_ty aggregate in
  let* pointer = address s aggregate in
  let vector_ty = ty source_ty in
  let loaded_id = fresh s in
  emit s (Ir.Load (loaded_id, vector_ty, pointer, align s source_ty));
  let* index = expr s index in
  Ok
    ( pointer,
      source_ty,
      vector_ty,
      Ir.Local (loaded_id, vector_ty),
      coerce s index Ir.I32 )

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
  Ok ()

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

let lower_func structs strings force_external f =
  let params =
    List.map
      (fun (n, t, noalias, align) ->
        ({ Ir.name = n; ty = ty t; noalias; align } : Ir.param))
      f.Hir.params
  in
  match f.asm_body with
  | Some raw ->
      Ok
        {
          Ir.name = f.name;
          params;
          ret = ty f.ret;
          blocks = [];
          linkage = Ir.External;
          variadic = f.variadic;
          attrs = f.attrs;
          asm_body = Some raw;
        }
  | None when f.linkage = Hir.External_c && f.body = [] ->
      Ok
        {
          Ir.name = f.name;
          params;
          ret = ty f.ret;
          blocks = [];
          linkage = Ir.External;
          variadic = f.variadic;
          attrs = f.attrs;
          asm_body = None;
        }
  | None ->
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
          defer_scopes = [];
          loops = [];
        }
      in
      push_scope s;
      List.iter
        (fun (n, t, _, a) ->
          let id = fresh s in
          emit s (Ir.Alloca (id, ty t, Option.value ~default:(align s t) a));
          let p = Ir.Local (id, Ir.Ptr (ty t)) in
          bind_local s n p;
          emit s
            (Ir.Store (ty t, Ir.Param (n, ty t), p, Option.value ~default:(align s t) a)))
        f.params;
      let* () = stmt_list s f.body in
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
          blocks;
          linkage =
            (if f.name = "main" || force_external then Ir.External
             else if f.linkage = Hir.Internal then Ir.Internal
             else External);
          variadic = f.variadic;
          attrs = f.attrs;
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
                | Ir.Call (_, ret, name, args)
                  when String.starts_with ~prefix:"llvm." name ->
                    Some (name, ret, List.map fst args)
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
              { Ir.name = "a" ^ string_of_int i; ty; noalias = false; align = None })
            args;
        ret;
        blocks = [];
        linkage = Ir.External;
        variadic = false;
        attrs = [];
        asm_body = None;
      })
  |> List.of_seq
  |> List.sort (fun (a : Ir.func) b -> String.compare a.name b.name)

let lower (p : Hir.program) =
  let asm_words =
    List.concat_map
      (fun (f : Hir.func) ->
        match f.asm_body with
        | None -> []
        | Some raw ->
            String.split_on_char ' '
              (String.map
                 (fun c ->
                   if
                     (c >= 'a' && c <= 'z')
                     || (c >= 'A' && c <= 'Z')
                     || (c >= '0' && c <= '9')
                     || c = '_'
                   then c
                   else ' ')
                 raw))
      p.funcs
  in
  let* funcs =
    Result_list.map
      (fun (f : Hir.func) ->
        lower_func p.Hir.structs p.strings (List.mem f.name asm_words) f)
      p.funcs
  in
  let structs =
    List.map
      (fun (d : Hir.struct_def) ->
        let fields =
          let rec go offset out = function
            | [] -> List.rev out
            | (f : Hir.field) :: rest ->
                let padding = f.offset - offset in
                let out =
                  if padding > 0 then Ir.Array (padding, Ir.I8) :: out else out
                in
                let size =
                  match Hir.layout p.structs f.ty with Ok (s, _) -> s | Error _ -> 0
                in
                go (f.offset + size) (ty f.ty :: out) rest
          in
          go 0 [] d.fields
        in
        (let used =
           List.fold_left
             (fun n (f : Hir.field) ->
               let sz =
                 match Hir.layout p.structs f.ty with Ok (s, _) -> s | _ -> 0
               in
               max n (f.offset + sz))
             0 d.fields
         in
         { Ir.name = d.name; fields; tail_padding = max 0 (d.size - used) }
          : Ir.struct_def))
      p.structs
  in
  let strings =
    List.mapi
      (fun i x -> Ir.String_global { name = ".str." ^ string_of_int i; bytes = x })
      p.strings
  in
  let arrays =
    List.map
      (fun (a : Hir.const_arr_def) ->
        match a.ty with
        | Hir.Array (_, e) ->
            Ir.Array_global
              {
                name = a.name;
                elem_ty = ty e;
                elems = a.elems;
                align = (match Hir.layout p.structs e with Ok (_, a) -> a | _ -> 1);
              }
        | _ -> Ir.Array_global { name = a.name; elem_ty = Ir.I8; elems = []; align = 1 })
      p.const_arrays
  in
  Ok
    {
      Ir.target_triple = "x86_64-unknown-linux-gnu";
      data_layout =
        "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128";
      structs;
      globals = strings @ arrays;
      funcs = funcs @ intrinsic_decls funcs;
    }
