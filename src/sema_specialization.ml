type kind = Function_specialization | Struct_specialization

type arg =
  | Type_specialization_arg of string
  | Const_specialization_arg of Hir.ty * int64

type key = kind * int * arg list

type diagnostic_type =
  | Diagnostic_bool
  | Diagnostic_void
  | Diagnostic_int of Ast.int_kind
  | Diagnostic_ptr of diagnostic_type
  | Diagnostic_const_ptr of diagnostic_type
  | Diagnostic_array of string * diagnostic_type
  | Diagnostic_vec of string * diagnostic_type
  | Diagnostic_named of string
  | Diagnostic_applied of string * diagnostic_argument list

and diagnostic_argument =
  | Diagnostic_type_argument of diagnostic_type
  | Diagnostic_const_argument of Hir.ty * int64
  | Diagnostic_source_const_argument of string

type instantiation_frame = {
  template_name : string;
  arguments : diagnostic_argument list;
  application_span : Span.t;
}

type pending_diagnostic_argument =
  | Pending_diagnostic_type of diagnostic_type
  | Pending_diagnostic_const of string

type pending_instantiation_frame = {
  pending_arguments : pending_diagnostic_argument list;
  pending_application_span : Span.t;
}

type staged_arg = Staged_type_arg of string | Staged_const_arg of string

type payload =
  | Function_payload of {
      item : Ast.item;
      substitutions : (string * Ast.ty) list;
      values : (string * Hir.ty * int64) list;
      staged_args : staged_arg list option;
    }
  | Struct_payload of {
      template : Ast.item;
      substitutions : (string * Ast.ty) list;
      values : (string * Hir.ty * int64) list;
    }

type specialization = {
  key : key;
  name : string;
  depth : int;
  payload : payload;
  trace : instantiation_frame list;
  pending_frame : pending_instantiation_frame option;
}

module Cache = Hashtbl.Make (struct
  type t = key

  let equal = ( = )
  let hash = Hashtbl.hash
end)

type t = {
  cache : specialization Cache.t;
  queue : specialization Queue.t;
  by_name : (kind * string, specialization) Hashtbl.t;
}

let create () =
  { cache = Cache.create 32; queue = Queue.create (); by_name = Hashtbl.create 32 }

let request state ~limits ~depth ~span ~description specialization =
  match Cache.find_opt state.cache specialization.key with
  | Some existing -> Ok existing
  | None ->
      if depth >= limits.Limits.max_specialization_depth then
        Error [ Diag.error span (description ^ " recursion depth limit exceeded") ]
      else if Cache.length state.cache >= limits.Limits.max_specializations then
        Error [ Diag.error span (description ^ " count limit exceeded") ]
      else (
        Cache.add state.cache specialization.key specialization;
        let kind, _, _ = specialization.key in
        Hashtbl.replace state.by_name (kind, specialization.name) specialization;
        Queue.add specialization state.queue;
        Ok specialization)

let find_by_name state kind name = Hashtbl.find_opt state.by_name (kind, name)
let fold_by_name f state initial = Hashtbl.fold f state.by_name initial
let take_pending state = Queue.take_opt state.queue
let requeue_materialized state specialization = Queue.add specialization state.queue

let update_materialized state specialization =
  Cache.replace state.cache specialization.key specialization;
  let kind, _, _ = specialization.key in
  Hashtbl.replace state.by_name (kind, specialization.name) specialization
