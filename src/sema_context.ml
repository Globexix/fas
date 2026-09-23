open Sema_flow
open Sema_specialization
open Sema_types

let error span message = Error [ Diag.error span message ]
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

type static_index = Dynamic | Known of Hir.ty * int64
type place_info = { expr : Hir.expr; root : binding option; path : place_path option }

type checked_target = {
  target : Hir.assign_target;
  root : binding option;
  path : place_path option;
}

type signature = { params : (string * Hir.ty) list; ret : Hir.ty; variadic : bool }
type trailing_args = Reject | Promote_variadic
type top_level_kind = Top_type | Top_function | Top_const

type top_level_binding = {
  declaration_id : int;
  declaration_name : string;
  declaration_kind : top_level_kind;
}

type string_pool = {
  mutable reversed : string list;
  index : (string, int) Hashtbl.t;
  mutable next_id : int;
  mutable bytes_used : int;
  budget : int;
  budget_profile : string;
}

let create_string_pool limits =
  {
    reversed = [];
    index = Hashtbl.create 64;
    next_id = 0;
    bytes_used = 0;
    budget = limits.Limits.max_interned_string_bytes;
    budget_profile = Limits.budget_profile_name limits;
  }

type context = {
  structs : Hir.struct_def list;
  named_types : (string * named_type_kind) list;
  consts : (string * Hir.ty * int64) list;
  arrays : (string * Hir.ty * int64 list) list;
  signatures : (string * signature) list;
  templates : (string * Ast.item) list;
  top_level_bindings : top_level_binding list;
  specializations : Sema_specialization.t;
  spec_depth : int;
  spec_trace : instantiation_frame list;
  flow : Sema_flow.t;
  string_pool : string_pool;
  ret_ty : Hir.ty;
  limits : Limits.t;
}

let lookup_top_level name bindings =
  List.find_opt (fun binding -> binding.declaration_name = name) bindings

type type_node_account = {
  type_node_limits : Limits.t;
  mutable expanded_type_nodes : int;
}

let create_type_node_account limits =
  { type_node_limits = limits; expanded_type_nodes = 0 }

let rec count_expanded_type_nodes ty cap total =
  if total >= cap then cap
  else
    let total = total + 1 in
    match ty with
    | Ast.Bool | Ast.Void | Ast.Int _ | Ast.Named_type _ -> total
    | Ast.Ptr element
    | Ast.Ptr_const element
    | Ast.Array (_, element)
    | Ast.Vec (_, element) ->
        count_expanded_type_nodes element cap total
    | Ast.Applied_type (_, arguments, _) ->
        List.fold_left
          (fun total argument ->
            if total >= cap then cap
            else
              match argument with
              | Ast.Type_arg argument_ty ->
                  count_expanded_type_nodes argument_ty cap total
              | Ast.Const_arg _ | Ast.Name_arg _ -> total)
          total arguments

let rec count_expanded_expr_type_nodes expr cap total =
  if total >= cap then cap
  else
    match expr with
    | Ast.Int_lit _ | Ast.Bool_lit _ | Ast.Null _ | Ast.String_lit _ | Ast.Ident _ ->
        total
    | Ast.Unary (_, value, _)
    | Ast.Deref (value, _)
    | Ast.Addr_of (value, _)
    | Ast.Splat (value, _)
    | Ast.Field (value, _, _) ->
        count_expanded_expr_type_nodes value cap total
    | Ast.Binary (_, left, right, _)
    | Ast.Index (left, right, _)
    | Ast.Ptr_add (_, left, right, _) ->
        count_expanded_expr_type_nodes right cap
          (count_expanded_expr_type_nodes left cap total)
    | Ast.Call (callee, arguments, _) ->
        List.fold_left
          (fun total argument -> count_expanded_expr_type_nodes argument cap total)
          (count_expanded_expr_type_nodes callee cap total)
          arguments
    | Ast.Generic_args (callee, arguments, _) ->
        List.fold_left
          (fun total argument -> count_expanded_arg_type_nodes argument cap total)
          (count_expanded_expr_type_nodes callee cap total)
          arguments
    | Ast.Cast (_, ty, value, _) ->
        count_expanded_expr_type_nodes value cap
          (count_expanded_type_nodes ty cap total)
    | Ast.Sizeof (ty, _) | Ast.Alignof (ty, _) | Ast.Offsetof (ty, _, _) ->
        count_expanded_type_nodes ty cap total
    | Ast.Ternary (condition, yes, no, _) ->
        count_expanded_expr_type_nodes no cap
          (count_expanded_expr_type_nodes yes cap
             (count_expanded_expr_type_nodes condition cap total))
    | Ast.Array_lit (elements, _) ->
        List.fold_left
          (fun total element -> count_expanded_expr_type_nodes element cap total)
          total elements
    | Ast.Struct_lit (ty, elements, _) ->
        List.fold_left
          (fun total element -> count_expanded_expr_type_nodes element cap total)
          (count_expanded_type_nodes ty cap total)
          elements

and count_expanded_arg_type_nodes argument cap total =
  if total >= cap then cap
  else
    match argument with
    | Ast.Type_arg ty -> count_expanded_type_nodes ty cap total
    | Ast.Const_arg expression -> count_expanded_expr_type_nodes expression cap total
    | Ast.Name_arg _ -> total

and count_expanded_stmt_type_nodes stmt cap total =
  if total >= cap then cap
  else
    match stmt with
    | Ast.Let { ty; init; _ } -> (
        let total = count_expanded_type_nodes ty cap total in
        match init with
        | Some expression -> count_expanded_expr_type_nodes expression cap total
        | None -> total)
    | Ast.Assign (target, value, _) | Ast.Compound_assign (target, _, value, _) ->
        count_expanded_expr_type_nodes value cap
          (count_expanded_target_type_nodes target cap total)
    | Ast.Return (value, _) -> (
        match value with
        | Some expression -> count_expanded_expr_type_nodes expression cap total
        | None -> total)
    | Ast.If (condition, then_branch, else_branch, _) -> (
        let total = count_expanded_expr_type_nodes condition cap total in
        let total = count_expanded_stmts_type_nodes then_branch cap total in
        match else_branch with
        | Some statements -> count_expanded_stmts_type_nodes statements cap total
        | None -> total)
    | Ast.While (condition, body, _) ->
        count_expanded_stmts_type_nodes body cap
          (count_expanded_expr_type_nodes condition cap total)
    | Ast.Break _ | Ast.Continue _ -> total
    | Ast.Defer (body, _) | Ast.Block (body, _) ->
        count_expanded_stmts_type_nodes body cap total
    | Ast.Expr_stmt (expression, _) ->
        count_expanded_expr_type_nodes expression cap total
    | Ast.For (init, condition, step, body, _) ->
        let total =
          match init with
          | Some statement -> count_expanded_stmt_type_nodes statement cap total
          | None -> total
        in
        let total =
          match condition with
          | Some expression -> count_expanded_expr_type_nodes expression cap total
          | None -> total
        in
        let total =
          match step with
          | Some statement -> count_expanded_stmt_type_nodes statement cap total
          | None -> total
        in
        count_expanded_stmts_type_nodes body cap total
    | Ast.Switch (expression, cases, default, _) -> (
        let total = count_expanded_expr_type_nodes expression cap total in
        let total =
          List.fold_left
            (fun total (case, body) ->
              count_expanded_stmts_type_nodes body cap
                (count_expanded_expr_type_nodes case cap total))
            total cases
        in
        match default with
        | Some statements -> count_expanded_stmts_type_nodes statements cap total
        | None -> total)

and count_expanded_stmts_type_nodes statements cap total =
  List.fold_left
    (fun total statement -> count_expanded_stmt_type_nodes statement cap total)
    total statements

and count_expanded_target_type_nodes target cap total =
  if total >= cap then cap
  else
    match target with
    | Ast.Target_ident _ -> total
    | Ast.Target_deref expression | Ast.Target_field (expression, _) ->
        count_expanded_expr_type_nodes expression cap total
    | Ast.Target_index (base, index) ->
        count_expanded_expr_type_nodes index cap
          (count_expanded_expr_type_nodes base cap total)

and count_expanded_generic_param_type_nodes parameter cap total =
  if total >= cap then cap
  else
    match parameter with
    | Ast.Type_param _ -> total
    | Ast.Const_param { ty; _ } -> count_expanded_type_nodes ty cap total

and count_expanded_body_type_nodes body cap total =
  match body with
  | Ast.Declaration | Ast.Asm _ -> total
  | Ast.Statements statements -> count_expanded_stmts_type_nodes statements cap total

and count_expanded_item_type_nodes item cap total =
  if total >= cap then cap
  else
    match item with
    | Ast.Const { ty; value; _ } ->
        count_expanded_expr_type_nodes value cap
          (count_expanded_type_nodes ty cap total)
    | Ast.Struct { fields; generic_params; _ } ->
        List.fold_left
          (fun total parameter ->
            count_expanded_generic_param_type_nodes parameter cap total)
          (List.fold_left
             (fun total (field : Ast.field) ->
               count_expanded_type_nodes field.ty cap total)
             total fields)
          generic_params
    | Ast.Opaque _ -> total
    | Ast.Func { params; ret; body; generic_params; _ } ->
        let total =
          List.fold_left
            (fun total (parameter : Ast.param) ->
              count_expanded_type_nodes parameter.ty cap total)
            total params
        in
        let total = count_expanded_type_nodes ret cap total in
        let total =
          List.fold_left
            (fun total parameter ->
              count_expanded_generic_param_type_nodes parameter cap total)
            total generic_params
        in
        count_expanded_body_type_nodes body cap total

let charge_expanded_type_nodes account ~span ~where walk =
  let budget = account.type_node_limits.Limits.max_type_nodes in
  let remaining = budget - account.expanded_type_nodes in
  let cap = if remaining = max_int then max_int else remaining + 1 in
  let total = walk cap 0 in
  if total > remaining then
    error span
      (Printf.sprintf
         "cumulative expanded type nodes exceed budget max_type_nodes of %d (profile \
          %s) at %s"
         budget
         (Limits.budget_profile_name account.type_node_limits)
         where)
  else (
    account.expanded_type_nodes <- account.expanded_type_nodes + total;
    Ok ())

let charge_expanded_type account ~span ~where ty =
  charge_expanded_type_nodes account ~span ~where (fun cap total ->
      count_expanded_type_nodes ty cap total)

let charge_expanded_item account ~span ~where item =
  charge_expanded_type_nodes account ~span ~where (fun cap total ->
      count_expanded_item_type_nodes item cap total)

let specialization_application_span
    (specialization : Sema_specialization.specialization) =
  match List.rev specialization.trace with
  | frame :: _ -> frame.application_span
  | [] -> (
      match specialization.pending_frame with
      | Some pending -> pending.pending_application_span
      | None -> Span.synthetic)

let specialization_declaration_id bindings kind span name =
  match lookup_top_level name bindings with
  | Some { declaration_id; declaration_kind; _ } when declaration_kind = kind ->
      Ok declaration_id
  | _ -> error span "internal error: specialization declaration is missing"

let validate_binding_name span name =
  if Names.reserved_binding_name name then
    error span (Printf.sprintf "`%s` is reserved and cannot be used as a binding" name)
  else Ok ()

let const_params generic_params =
  List.filter_map
    (function Ast.Const_param cp -> Some cp | Ast.Type_param _ -> None)
    generic_params

let has_type_params generic_params =
  List.exists
    (function Ast.Type_param _ -> true | Ast.Const_param _ -> false)
    generic_params

let has_const_params generic_params =
  List.exists
    (function Ast.Const_param _ -> true | Ast.Type_param _ -> false)
    generic_params

let equal = Hir.ty_equal
let lookup name table = List.find_opt (fun (n, _, _) -> n = name) table
let lookup_sig name c = List.assoc_opt name c.signatures
let lookup_local name c = Sema_flow.lookup_local name c.flow
let ensure_new_local name c span = Sema_flow.ensure_new_local name c.flow span
let add_local name ty c span = Sema_flow.add_local name ty c.flow span
let push c = Sema_flow.push c.flow
let pop c = Sema_flow.pop c.flow
let set_state c binding path state = Sema_flow.set_state c.flow binding path state
let require_state binding path c span = Sema_flow.require_state binding path c.flow span

let require_place_state binding path c span =
  Sema_flow.require_place_state binding path c.flow span

let merge_maps c left right = Sema_flow.merge c.flow left right
let validate_exit_defers c keep = Sema_flow.validate_exit_defers c.flow keep
let mark_init binding c = Sema_flow.mark_init binding c.flow
