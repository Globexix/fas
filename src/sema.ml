module IM = Map.Make (Int)

type binding = Hir.local = { name : string; ty : Hir.ty; id : int }
type selector = Field of string | Element of int
type init_state = Uninit | Full | Raw | Partial of (selector * init_state) list
type static_index = Dynamic | Known of Hir.ty * int64
type place_path = Exact of selector list | Dynamic_prefix of selector list
type place_info = { expr : Hir.expr; root : binding option; path : place_path option }

type checked_target = {
  target : Hir.assign_target;
  root : binding option;
  path : place_path option;
}

type signature = { params : (string * Hir.ty) list; ret : Hir.ty; variadic : bool }
type specialization_kind = Function_specialization | Struct_specialization

type specialization_arg =
  | Type_specialization_arg of string
  | Const_specialization_arg of Hir.ty * int64

type specialization_key = specialization_kind * string * specialization_arg list

type staged_specialization_arg =
  | Staged_type_arg of string
  | Staged_const_arg of string

type specialization_payload =
  | Function_payload of {
      item : Ast.item;
      substitutions : (string * Ast.ty) list;
      values : (string * Hir.ty * int64) list;
      staged_args : staged_specialization_arg list option;
    }
  | Struct_payload of {
      template : Ast.item;
      substitutions : (string * Ast.ty) list;
      values : (string * Hir.ty * int64) list;
    }

type specialization = {
  key : specialization_key;
  name : string;
  depth : int;
  payload : specialization_payload;
}

module Specialization_cache = Hashtbl.Make (struct
  type t = specialization_key

  let equal = ( = )
  let hash = Hashtbl.hash
end)

type specialization_state = {
  cache : specialization Specialization_cache.t;
  queue : specialization Queue.t;
}

type trailing_args = Reject | Promote_variadic
type named_type_kind = Struct_name | Generic_struct_name | Opaque_name

type context = {
  structs : Hir.struct_def list;
  named_types : (string * named_type_kind) list;
  consts : (string * Hir.ty * int64) list;
  arrays : (string * Hir.ty * int64 list) list;
  signatures : (string * signature) list;
  templates : (string * Ast.item) list;
  specializations : specialization_state;
  spec_depth : int;
  locals : (string, binding) Hashtbl.t list ref;
  mutable initialized : init_state IM.t;
  mutable next_binding_id : int;
  mutable strings : string list;
  mutable string_ids : (string * int) list;
  ret_ty : Hir.ty;
  mutable loop_depth : int;
  mutable in_defer : bool;
  mutable falls_through : bool;
  limits : Limits.t;
}

let error span message = Error [ Diag.error span message ]
let ok x = Ok x
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let request_specialization state ~limits ~depth ~span ~description specialization =
  match Specialization_cache.find_opt state.cache specialization.key with
  | Some existing -> Ok existing
  | None ->
      if depth >= limits.Limits.max_specialization_depth then
        error span (description ^ " recursion depth limit exceeded")
      else if
        Specialization_cache.length state.cache >= limits.Limits.max_specializations
      then error span (description ^ " count limit exceeded")
      else (
        Specialization_cache.add state.cache specialization.key specialization;
        Queue.add specialization state.queue;
        Ok specialization)

let reserved_builtin_names =
  [ "len"; "shl"; "lshr"; "ashr"; "rotl"; "rotr"; "popcount"; "ctz"; "clz" ]

let src_int = function
  | Ast.U8 -> Hir.U8
  | U16 -> U16
  | U32 -> U32
  | U64 -> U64
  | I8 -> I8
  | I16 -> I16
  | I32 -> I32
  | I64 -> I64
  | Usize -> Usize
  | Isize -> Isize

let rec source_ty named_types = function
  | Ast.Bool -> Ok Hir.Bool
  | Ast.Void -> Ok Hir.Void
  | Ast.Int k -> Ok (Hir.Int (src_int k))
  | Ast.Ptr t ->
      let* t = source_ty named_types t in
      Ok (Hir.Ptr t)
  | Ast.Ptr_const t ->
      let* t = source_ty named_types t in
      Ok (Hir.ConstPtr t)
  | Ast.Array (raw, t) ->
      source_aggregate named_types (fun n t -> Hir.Array (n, t)) raw t
  | Ast.Vec (raw, t) -> source_aggregate named_types (fun n t -> Hir.Vec (n, t)) raw t
  | Ast.Named_type n -> (
      match List.assoc_opt n named_types with
      | Some Struct_name -> Ok (Hir.Struct n)
      | Some Generic_struct_name ->
          Error (Printf.sprintf "generic struct `%s` requires type arguments" n)
      | Some Opaque_name -> Ok (Hir.Opaque n)
      | None -> Error (Printf.sprintf "unknown type `%s`" n))
  | Ast.Applied_type _ ->
      Error "generic type application reached ordinary type checking"

and source_aggregate named_types make raw element =
  try
    let length = int_of_string raw in
    if length < 0 then Error "negative aggregate length"
    else
      let* element = source_ty named_types element in
      Ok (make length element)
  with Failure _ -> Error "aggregate length is not a machine integer"

let source_ty_diag named_types span t =
  source_ty named_types t |> Result.map_error (fun m -> [ Diag.error span m ])

let layout_diag span structs t =
  Hir.layout structs t |> Result.map_error (fun m -> [ Diag.error span m ])

let int_bits = function
  | Hir.U8 | I8 -> 8
  | U16 | I16 -> 16
  | U32 | I32 -> 32
  | U64 | I64 -> 64
  | Usize | Isize -> Target_layout.current.pointer_size * 8

let integer_value_bit_width = function
  | Hir.Bool -> Some 1
  | Hir.Int kind -> Some (int_bits kind)
  | Hir.Vec (length, Hir.Bool) -> Some length
  | Hir.Vec (length, Hir.Int kind) ->
      let element_width = int_bits kind in
      if length > max_int / element_width then None else Some (length * element_width)
  | _ -> None

let is_int = function Hir.Int _ -> true | _ -> false

let is_unsigned = function
  | Hir.Int (Hir.U8 | U16 | U32 | U64 | Usize) -> true
  | _ -> false

let is_scalar = function
  | Hir.Bool | Hir.Int _ | Hir.Ptr _ | Hir.ConstPtr _ -> true
  | _ -> false

let is_numeric = function Hir.Int _ | Hir.Vec (_, Hir.Int _) -> true | _ -> false
let is_truthy = is_scalar

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

let validate_generic_params named_types params =
  let* () =
    Result_list.iter
      (function
        | Ast.Type_param _ -> Ok ()
        | Ast.Const_param cp ->
            let* t = source_ty_diag named_types cp.span cp.ty in
            if is_int t then Ok ()
            else error cp.span "const parameter type must be an integer")
      params
  in
  let rec dup seen = function
    | [] -> Ok ()
    | Ast.Type_param { name; span } :: rest | Ast.Const_param { name; span; _ } :: rest
      ->
        if List.mem name seen then
          error span (Printf.sprintf "duplicate generic parameter `%s`" name)
        else dup (name :: seen) rest
  in
  dup [] params

let equal = Hir.ty_equal
let ty_name = Hir.ty_name

let rec object_type (structs : Hir.struct_def list) = function
  | Hir.Void -> Error "void is not an object type"
  | Hir.Opaque n -> Error ("opaque type `" ^ n ^ "` may only be used behind a pointer")
  | Hir.Ptr _ | Hir.ConstPtr _ | Hir.Bool | Hir.Int _ -> Ok ()
  | Hir.Array (n, t) ->
      if n < 0 then Error "negative array length" else object_type structs t
  | Hir.Vec (n, t) when n > 0 -> (
      match t with
      | Hir.Int _ | Hir.Bool | Hir.Ptr _ | Hir.ConstPtr _ -> Ok ()
      | _ -> Error "vector element type must be a scalar (bool, integer, or pointer)")
  | Hir.Vec _ -> Error "vector lane count must be positive"
  | Hir.Struct n ->
      if List.exists (fun (s : Hir.struct_def) -> s.name = n) structs then Ok ()
      else Error ("unknown struct `" ^ n ^ "`")

let extern_c_value_type = function
  | Hir.Bool | Hir.Int _ | Hir.Ptr _ | Hir.ConstPtr _ -> true
  | Hir.Void | Hir.Array _ | Hir.Vec _ | Hir.Struct _ | Hir.Opaque _ -> false

let validate_extern_c_signature span params converted ret =
  let rec validate_params params converted =
    match (params, converted) with
    | [], [] -> Ok ()
    | (param : Ast.param) :: param_rest, (_, ty) :: converted_rest ->
        if extern_c_value_type ty then validate_params param_rest converted_rest
        else
          error param.span
            (Printf.sprintf
               "extern \"C\" parameter `%s` cannot use `%s` by value; use a pointer"
               param.name (Hir.ty_name ty))
    | _ -> error span "internal error: extern parameter list mismatch"
  in
  let* () = validate_params params converted in
  if ret = Hir.Void || extern_c_value_type ret then Ok ()
  else
    error span
      (Printf.sprintf "extern \"C\" cannot return `%s` by value; use an output pointer"
         (Hir.ty_name ret))

let aggregate_within_limit limits structs ty =
  let max_elements = limits.Limits.max_aggregate_elements in
  let rec check count = function
    | Hir.Array (n, t) | Hir.Vec (n, t) ->
        if n = 0 then true else n <= max_elements / count && check (count * n) t
    | Hir.Struct name -> (
        match List.find_opt (fun (s : Hir.struct_def) -> s.name = name) structs with
        | Some definition ->
            List.for_all
              (fun (field : Hir.field) -> check count field.ty)
              definition.fields
        | None -> count <= max_elements)
    | _ -> count <= max_elements
  in
  check 1 ty

let parse_integer raw =
  let clean = String.concat "" (String.split_on_char '_' raw) in
  let radix, digits =
    if
      String.length clean > 2
      && (String.sub clean 0 2 = "0x" || String.sub clean 0 2 = "0X")
    then (16, String.sub clean 2 (String.length clean - 2))
    else if
      String.length clean > 2
      && (String.sub clean 0 2 = "0b" || String.sub clean 0 2 = "0B")
    then (2, String.sub clean 2 (String.length clean - 2))
    else if
      String.length clean > 2
      && (String.sub clean 0 2 = "0o" || String.sub clean 0 2 = "0O")
    then (8, String.sub clean 2 (String.length clean - 2))
    else (10, clean)
  in
  let value = ref 0L in
  let digit c =
    match c with
    | '0' .. '9' -> Char.code c - 48
    | 'a' .. 'f' -> Char.code c - 87
    | 'A' .. 'F' -> Char.code c - 55
    | _ -> -1
  in
  if digits = "" then Error "integer literal has no digits"
  else
    let valid =
      String.for_all
        (fun c ->
          let d = digit c in
          d >= 0 && d < radix)
        digits
    in
    let limit =
      match radix with
      | 10 -> "18446744073709551615"
      | 16 -> "FFFFFFFFFFFFFFFF"
      | 8 -> "1777777777777777777777"
      | 2 -> String.make 64 '1'
      | _ -> ""
    in
    let normalized = if radix = 16 then String.uppercase_ascii digits else digits in
    let overflow =
      String.length normalized > String.length limit
      || String.length normalized = String.length limit
         && String.compare normalized limit > 0
    in
    if not valid then Error "invalid integer literal"
    else if overflow then Error "integer literal overflows 64 bits"
    else (
      String.iter
        (fun c ->
          value :=
            Int64.add (Int64.mul !value (Int64.of_int radix)) (Int64.of_int (digit c)))
        digits;
      Ok !value)

let mask_value ty value =
  match ty with
  | Hir.Bool -> if value = 0L then 0L else 1L
  | Hir.Int k ->
      let bits = int_bits k in
      if bits = 64 then value
      else Int64.logand value (Int64.sub (Int64.shift_left 1L bits) 1L)
  | Hir.Ptr _ | Hir.ConstPtr _ -> value
  | _ -> value

let literal_limit ~negative = function
  | Hir.Int k ->
      let bits = int_bits k in
      let signed =
        match k with Hir.I8 | I16 | I32 | I64 | Isize -> true | _ -> false
      in
      if signed then
        if negative then Int64.shift_left 1L (bits - 1)
        else if bits = 64 then Int64.max_int
        else Int64.sub (Int64.shift_left 1L (bits - 1)) 1L
      else if bits = 64 then Int64.minus_one
      else Int64.sub (Int64.shift_left 1L bits) 1L
  | _ -> 0L

let fits_literal ty value =
  match ty with
  | Hir.Int _ -> Int64.unsigned_compare value (literal_limit ~negative:false ty) <= 0
  | Hir.Bool -> value = 0L || value = 1L
  | _ -> false

let fits_negative_literal ty value =
  match ty with
  | Hir.Int _ -> Int64.unsigned_compare value (literal_limit ~negative:true ty) <= 0
  | Hir.Bool -> value = 0L || value = 1L
  | _ -> false

let sign_extend_bits ty value =
  match ty with
  | Hir.Int k ->
      let bits = int_bits k in
      if bits = 64 then value
      else
        let mask = Int64.sub (Int64.shift_left 1L bits) 1L in
        let value = Int64.logand value mask in
        let sign_bit = Int64.shift_left 1L (bits - 1) in
        if Int64.logand value sign_bit <> 0L then Int64.logor value (Int64.lognot mask)
        else value
  | _ -> value

let sign_extend_value ty value =
  if is_int ty && not (is_unsigned ty) then sign_extend_bits ty value else value

let fits_int ty value =
  match ty with
  | Hir.Int k ->
      let bits = int_bits k in
      let signed =
        match k with Hir.I8 | I16 | I32 | I64 | Isize -> true | _ -> false
      in
      if bits = 64 then (not signed) || value >= 0L
      else if signed then
        let range = Int64.shift_left 1L (bits - 1) in
        let min = Int64.neg range and max = Int64.sub range 1L in
        value >= min && value <= max
      else value >= 0L && value < Int64.shift_left 1L bits
  | Hir.Bool -> value = 0L || value = 1L
  | _ -> false

let lookup name table = List.find_opt (fun (n, _, _) -> n = name) table
let lookup_sig name c = List.assoc_opt name c.signatures

let resolve_aggregate_length values span length =
  match lookup length values with
  | None -> Ok length
  | Some (_, ty, value) ->
      if is_unsigned ty then
        if Int64.unsigned_compare value (Int64.of_int max_int) > 0 then
          error span "aggregate length is not a machine integer"
        else Ok (Int64.to_string value)
      else
        let value = sign_extend_value ty value in
        if value < 0L then error span "negative aggregate length"
        else if value > Int64.of_int max_int then
          error span "aggregate length is not a machine integer"
        else Ok (Int64.to_string value)

let rec source_ty_with_values named_types values span = function
  | Ast.Ptr ty ->
      let* ty = source_ty_with_values named_types values span ty in
      Ok (Hir.Ptr ty)
  | Ast.Ptr_const ty ->
      let* ty = source_ty_with_values named_types values span ty in
      Ok (Hir.ConstPtr ty)
  | Ast.Array (length, ty) -> (
      let* length = resolve_aggregate_length values span length in
      let* ty = source_ty_with_values named_types values span ty in
      try
        let length = int_of_string length in
        if length < 0 then error span "negative aggregate length"
        else Ok (Hir.Array (length, ty))
      with Failure _ -> error span "aggregate length is not a machine integer")
  | Ast.Vec (length, ty) -> (
      let* length = resolve_aggregate_length values span length in
      let* ty = source_ty_with_values named_types values span ty in
      try
        let length = int_of_string length in
        if length < 0 then error span "negative aggregate length"
        else Ok (Hir.Vec (length, ty))
      with Failure _ -> error span "aggregate length is not a machine integer")
  | ty -> source_ty_diag named_types span ty

let lookup_local name c =
  let rec go = function
    | [] -> None
    | h :: t -> ( match Hashtbl.find_opt h name with Some x -> Some x | None -> go t)
  in
  go !(c.locals)

let add_local name ty c span =
  let h = match !(c.locals) with h :: _ -> h | [] -> Hashtbl.create 8 in
  let duplicate = Hashtbl.mem h name in
  if duplicate then error span (Printf.sprintf "duplicate local `%s`" name)
  else
    let id = c.next_binding_id in
    c.next_binding_id <- id + 1;
    let binding : binding = { ty; id; name } in
    Hashtbl.replace h name binding;
    if !(c.locals) = [] then c.locals := [ h ];
    ok binding

let push c = c.locals := Hashtbl.create 8 :: !(c.locals)
let pop c = match !(c.locals) with _ :: rest -> c.locals := rest | [] -> ()

let field_info structs name field =
  match List.find_opt (fun (s : Hir.struct_def) -> s.name = name) structs with
  | None -> None
  | Some (s : Hir.struct_def) ->
      List.find_opt (fun (f : Hir.field) -> f.name = field) s.fields

let state_of c binding =
  Option.value ~default:Uninit (IM.find_opt binding.id c.initialized)

let state_usable = function Full | Raw -> true | Uninit | Partial _ -> false

let child_type c ty selector =
  match (ty, selector) with
  | Hir.Struct n, Field name ->
      Option.map (fun (f : Hir.field) -> f.ty) (field_info c.structs n name)
  | (Hir.Array (_, t) | Hir.Vec (_, t)), Element _ -> Some t
  | _ -> None

let is_vacuous_type c ty =
  let rec go seen = function
    | Hir.Struct name ->
        if List.mem name seen then false
        else
          Option.value ~default:false
            (Option.map
               (fun (s : Hir.struct_def) ->
                 List.for_all (fun (f : Hir.field) -> go (name :: seen) f.ty) s.fields)
               (List.find_opt (fun (s : Hir.struct_def) -> s.name = name) c.structs))
    | Hir.Array (n, element) -> n = 0 || go seen element
    | _ -> false
  in
  go [] ty

let rec normalize_state c ty = function
  | Uninit -> Uninit
  | (Full | Raw) as state -> state
  | Partial entries ->
      let entries =
        List.filter_map
          (fun (selector, state) ->
            match child_type c ty selector with
            | None -> None
            | Some child_ty ->
                let state = normalize_state c child_ty state in
                if state = Uninit then None else Some (selector, state))
          entries
      in
      let complete, any_raw =
        match ty with
        | Hir.Struct n -> (
            match List.find_opt (fun (s : Hir.struct_def) -> s.name = n) c.structs with
            | None -> (false, false)
            | Some s ->
                let all =
                  List.for_all
                    (fun (f : Hir.field) ->
                      match List.assoc_opt (Field f.name) entries with
                      | Some state -> state_usable state || is_vacuous_type c f.ty
                      | None -> is_vacuous_type c f.ty)
                    s.fields
                in
                let raw =
                  List.exists
                    (fun (f : Hir.field) ->
                      match List.assoc_opt (Field f.name) entries with
                      | Some Raw -> true
                      | _ -> false)
                    s.fields
                in
                (all, raw))
        | Hir.Array (n, element_ty) | Hir.Vec (n, element_ty) ->
            let all =
              n >= 0
              &&
              let rec each i =
                if i = n then true
                else
                  match List.assoc_opt (Element i) entries with
                  | Some state when state_usable state -> each (i + 1)
                  | Some _ | None ->
                      if is_vacuous_type c element_ty then each (i + 1) else false
              in
              each 0
            in
            let raw =
              List.exists
                (fun (_, state) -> match state with Raw -> true | _ -> false)
                entries
            in
            (all, raw)
        | _ -> (false, false)
      in
      if complete then if any_raw then Raw else Full
      else if entries = [] then Uninit
      else Partial entries

let rec update_state c ty state path replacement =
  match path with
  | [] -> replacement
  | selector :: rest -> (
      if state_usable state then state
      else
        let entries = match state with Partial xs -> xs | Uninit -> [] | _ -> [] in
        match child_type c ty selector with
        | None -> state
        | Some child_ty ->
            let child =
              Option.value ~default:Uninit (List.assoc_opt selector entries)
            in
            let child = update_state c child_ty child rest replacement in
            let entries =
              List.remove_assoc selector entries |> fun xs ->
              if child = Uninit then xs else (selector, child) :: xs
            in
            normalize_state c ty (Partial entries))

let set_state c binding path replacement =
  let state = update_state c binding.ty (state_of c binding) path replacement in
  if state = Uninit then c.initialized <- IM.remove binding.id c.initialized
  else c.initialized <- IM.add binding.id state c.initialized

let require_state binding path c span =
  let rec walk ty state = function
    | [] -> if state_usable state || is_vacuous_type c ty then Ok () else Error ()
    | selector :: rest -> (
        match state with
        | Full | Raw -> Ok ()
        | Partial entries -> (
            match child_type c ty selector with
            | Some child_ty ->
                let child =
                  Option.value ~default:Uninit (List.assoc_opt selector entries)
                in
                walk child_ty child rest
            | _ -> Error ())
        | Uninit -> (
            match child_type c ty selector with
            | Some child_ty -> walk child_ty Uninit rest
            | None -> Error ()))
  in
  if walk binding.ty (state_of c binding) path = Ok () then Ok ()
  else error span (Printf.sprintf "use of uninitialized local `%s`" binding.name)

let require_place_state binding path c span =
  match path with
  | Exact path | Dynamic_prefix path -> require_state binding path c span

let merge_state c ty left right =
  let rec merge ty left right =
    let merge_partial whole entries =
      Partial
        (List.filter_map
           (fun (selector, state) ->
             match child_type c ty selector with
             | Some child_ty -> (
                 match merge child_ty whole state with
                 | Uninit -> None
                 | state -> Some (selector, state))
             | None -> None)
           entries)
    in
    let result =
      match (left, right) with
      | Uninit, _ | _, Uninit -> Uninit
      | Full, Full -> Full
      | Raw, Raw -> Raw
      | Full, Raw | Raw, Full -> Raw
      | Full, Partial entries | Partial entries, Full -> merge_partial Full entries
      | Raw, Partial entries | Partial entries, Raw -> merge_partial Raw entries
      | Partial left, Partial right ->
          let selectors = List.map fst left @ List.map fst right in
          let selectors =
            List.fold_left
              (fun acc selector ->
                if List.mem selector acc then acc else selector :: acc)
              [] selectors
          in
          Partial
            (List.filter_map
               (fun selector ->
                 let left =
                   Option.value ~default:Uninit (List.assoc_opt selector left)
                 in
                 let right =
                   Option.value ~default:Uninit (List.assoc_opt selector right)
                 in
                 match child_type c ty selector with
                 | Some child_ty -> (
                     match merge child_ty left right with
                     | Uninit -> None
                     | state -> Some (selector, state))
                 | None -> None)
               selectors)
    in
    normalize_state c ty result
  in
  merge ty left right

let merge_maps c left right =
  IM.merge
    (fun id left right ->
      match (left, right) with
      | Some left, Some right ->
          let rec find = function
            | [] -> None
            | scope :: rest -> (
                match
                  Hashtbl.fold
                    (fun _ binding result ->
                      match result with
                      | Some _ -> result
                      | None -> if binding.id = id then Some binding else None)
                    scope None
                with
                | Some binding -> Some binding
                | None -> find rest)
          in
          Option.map
            (fun binding -> merge_state c binding.ty left right)
            (find !(c.locals))
      | _ -> None)
    left right

let mark_init binding c = set_state c binding [] Full

let intern_string c s =
  match List.assoc_opt s c.string_ids with
  | Some i -> i
  | None ->
      let i = List.length c.strings in
      c.strings <- c.strings @ [ s ];
      c.string_ids <- (s, i) :: c.string_ids;
      i

let compatible actual expected =
  equal actual expected
  || match (actual, expected) with Hir.Ptr a, Hir.ConstPtr b -> equal a b | _ -> false

let ensure_expected actual expected span =
  if compatible actual expected then Ok ()
  else
    error span
      (Printf.sprintf "type mismatch: expected %s, got %s" (ty_name expected)
         (ty_name actual))

let variadic_promote e =
  match Hir.expr_ty e with
  | Hir.Bool | Hir.Int (Hir.U8 | Hir.U16) ->
      Hir.Cast (Ast.Zext, e, Hir.Int Hir.I32, Hir.expr_span e)
  | Hir.Int (Hir.I8 | Hir.I16) ->
      Hir.Cast (Ast.Sext, e, Hir.Int Hir.I32, Hir.expr_span e)
  | _ -> e

let popcount64 x =
  let rec go n v = if v = 0L then n else go (n + 1) (Int64.logand v (Int64.sub v 1L)) in
  go 0 x

let trailing64 x =
  if x = 0L then 64
  else
    let rec go n v =
      if Int64.logand v 1L <> 0L then n else go (n + 1) (Int64.shift_right_logical v 1)
    in
    go 0 x

let leading64 x =
  if x = 0L then 64
  else
    let rec go n bit =
      if Int64.logand x bit <> 0L then n
      else go (n + 1) (Int64.shift_right_logical bit 1)
    in
    go 0 Int64.min_int

let rec const_expr ?(structs = []) ?(named_types = []) ?(arrays = []) consts expected
    ?(check_only = false) = function
  | Ast.Int_lit (raw, s) ->
      let* v = parse_integer raw |> Result.map_error (fun m -> [ Diag.error s m ]) in
      let ty = Option.value ~default:(Hir.Int Hir.I32) expected in
      if not (fits_literal ty v) then
        error s ("integer literal is out of range for " ^ ty_name ty)
      else Ok (ty, mask_value ty v)
  | Ast.Bool_lit (v, _) -> Ok (Hir.Bool, if v then 1L else 0L)
  | Ast.Ident (n, s) -> (
      match lookup n consts with
      | Some (_, t, v) -> Ok (t, v)
      | None -> error s "constant expression requires a known constant")
  | Ast.Unary (Ast.Neg, Ast.Int_lit (raw, is), s) ->
      let* v = parse_integer raw |> Result.map_error (fun m -> [ Diag.error is m ]) in
      let t = Option.value ~default:(Hir.Int Hir.I32) expected in
      let allowed =
        match t with
        | Hir.Int ((Hir.I8 | I16 | I32 | I64 | Isize) as k) ->
            let b = int_bits k in
            (b = 64 && v = Int64.min_int) || (b < 64 && v = Int64.shift_left 1L (b - 1))
        | _ -> false
      in
      if fits_negative_literal t v || allowed then Ok (t, mask_value t (Int64.neg v))
      else error s ("integer literal is out of range for " ^ ty_name t)
  | Ast.Unary (Ast.Neg, e, s) ->
      let* t, v =
        const_expr ~structs ~named_types ~arrays consts expected ~check_only e
      in
      if not (is_int t) then error s "unary minus requires an integer"
      else Ok (t, mask_value t (Int64.neg v))
  | Ast.Unary (Ast.Bit_not, e, s) ->
      let* t, v =
        const_expr ~structs ~named_types ~arrays consts expected ~check_only e
      in
      if not (is_int t) then error s "bitwise not requires an integer"
      else Ok (t, mask_value t (Int64.lognot v))
  | Ast.Unary (Ast.Not, e, _) ->
      let* _, v = const_expr ~structs ~named_types ~arrays consts None ~check_only e in
      Ok (Hir.Bool, if v = 0L then 1L else 0L)
  | Ast.Binary (((Ast.And | Ast.Or) as op), l, r, s) ->
      let* lt, lv =
        const_expr ~structs ~named_types ~arrays consts None ~check_only l
      in
      if not (is_truthy lt) then error s "logical operands must be scalar"
      else if
        (not check_only) && ((op = Ast.And && lv = 0L) || (op = Ast.Or && lv <> 0L))
      then
        let* _ =
          const_expr ~structs ~named_types ~arrays consts None ~check_only:true r
        in
        Ok (Hir.Bool, if op = Ast.And then 0L else 1L)
      else
        let* rt, rv =
          const_expr ~structs ~named_types ~arrays consts None ~check_only r
        in
        if not (is_truthy rt) then error s "logical operands must be scalar"
        else Ok (Hir.Bool, if rv <> 0L then 1L else 0L)
  | Ast.Binary (op, l, r, s) ->
      let* (lt, lv), (rt, rv) =
        match (l, op) with
        | ( (Ast.Int_lit _ | Ast.Unary (Ast.Neg, Ast.Int_lit _, _)),
            (Ast.Eq | Ne | Lt | Le | Gt | Ge) ) ->
            let* rt, rv =
              const_expr ~structs ~named_types ~arrays consts None ~check_only r
            in
            let* lt, lv =
              const_expr ~structs ~named_types ~arrays consts (Some rt) ~check_only l
            in
            Ok ((lt, lv), (rt, rv))
        | _ ->
            let* lt, lv =
              const_expr ~structs ~named_types ~arrays consts expected ~check_only l
            in
            let* rt, rv =
              const_expr ~structs ~named_types ~arrays consts (Some lt) ~check_only r
            in
            Ok ((lt, lv), (rt, rv))
      in
      if not (equal lt rt) then error s "constant operands have different types"
      else if (op = Ast.Div || op = Ast.Rem) && rv = 0L then
        error s "division by zero in constant expression"
      else
        let signed_lv = sign_extend_value lt lv in
        let signed_rv = sign_extend_value rt rv in
        let signed_min =
          match lt with
          | Hir.Int k ->
              let bits = int_bits k in
              if bits = 64 then Int64.min_int
              else Int64.neg (Int64.shift_left 1L (bits - 1))
          | _ -> 0L
        in
        if
          (not check_only) && op = Ast.Div
          && (not (is_unsigned lt))
          && signed_lv = signed_min && signed_rv = Int64.minus_one
        then error s "signed division overflow in constant expression"
        else
          let cmp =
            if is_unsigned lt then Int64.unsigned_compare lv rv
            else Int64.compare signed_lv signed_rv
          in
          let result =
            match op with
            | Ast.Add -> Int64.add lv rv
            | Sub -> Int64.sub lv rv
            | Mul -> Int64.mul lv rv
            | Bit_and -> Int64.logand lv rv
            | Bit_or -> Int64.logor lv rv
            | Bit_xor -> Int64.logxor lv rv
            | Div ->
                if is_unsigned lt then Int64.unsigned_div lv rv
                else Int64.div signed_lv signed_rv
            | Rem ->
                if is_unsigned lt then Int64.unsigned_rem lv rv
                else Int64.rem signed_lv signed_rv
            | Eq -> if lv = rv then 1L else 0L
            | Ne -> if lv <> rv then 1L else 0L
            | Lt -> if cmp < 0 then 1L else 0L
            | Le -> if cmp <= 0 then 1L else 0L
            | Gt -> if cmp > 0 then 1L else 0L
            | Ge -> if cmp >= 0 then 1L else 0L
            | And -> if lv <> 0L && rv <> 0L then 1L else 0L
            | Or -> if lv <> 0L || rv <> 0L then 1L else 0L
          in
          let rt =
            match op with
            | Ast.Eq | Ne | Lt | Le | Gt | Ge | And | Or -> Hir.Bool
            | _ -> lt
          in
          Ok (rt, mask_value rt result)
  | Ast.Ternary (c, a, b, _s) ->
      let* _, cv = const_expr ~structs ~named_types ~arrays consts None ~check_only c in
      if cv <> 0L then
        const_expr ~structs ~named_types ~arrays consts expected ~check_only a
      else const_expr ~structs ~named_types ~arrays consts expected ~check_only b
  | Ast.Cast (k, dst, e, s) ->
      let* dt = source_ty_with_values named_types consts s dst in
      let* st, v =
        const_expr ~structs ~named_types ~arrays consts ~check_only
          (if
             k = Ast.Bitcast
             &&
             match e with
             | Ast.Int_lit _ | Ast.Unary (Ast.Neg, Ast.Int_lit _, _) -> true
             | _ -> false
           then Some dt
           else None)
          e
      in
      let sb =
        match st with
        | Hir.Bool -> 1
        | Hir.Int q -> int_bits q
        | Hir.Ptr _ | Hir.ConstPtr _ -> Target_layout.current.pointer_size * 8
        | _ -> 0
      in
      let result =
        match k with
        | Ast.Sext when st <> Hir.Bool && sb < 64 ->
            let shift = 64 - sb in
            Int64.shift_right (Int64.shift_left v shift) shift
        | _ -> v
      in
      Ok (dt, mask_value dt result)
  | Ast.Call (Ast.Ident ("len", _), [ Ast.String_lit (cstr, v, _) ], s) ->
      if cstr && String.contains v '\000' then
        error s "C string literal cannot contain embedded NUL"
      else Ok (Hir.Int Hir.Usize, Int64.of_int (String.length v))
  | Ast.Call (Ast.Ident ("len", _), [ Ast.Ident (name, _) ], s) -> (
      match lookup name arrays with
      | Some (_, Hir.Array (n, _), _) -> Ok (Hir.Int Hir.Usize, Int64.of_int n)
      | _ -> error s "len requires a fixed array or string literal")
  | Ast.Call (Ast.Ident (name, _), args, s) -> (
      let* vals =
        Result_list.map
          (fun a ->
            const_expr ~structs ~named_types ~arrays consts expected ~check_only a)
          args
      in
      match (name, vals) with
      | ("shl" | "lshr" | "ashr" | "rotl" | "rotr"), [ (t, x); (_, n) ] ->
          let bits = match t with Hir.Int q -> int_bits q | _ -> 64 in
          let k = Int64.to_int (Int64.logand n (Int64.of_int (bits - 1))) in
          let v =
            match name with
            | "shl" -> Int64.shift_left x k
            | "lshr" -> Int64.shift_right_logical x k
            | "ashr" -> Int64.shift_right (sign_extend_bits t x) k
            | "rotl" ->
                Int64.logor (Int64.shift_left x k)
                  (Int64.shift_right_logical x (bits - k))
            | _ ->
                Int64.logor
                  (Int64.shift_right_logical x k)
                  (Int64.shift_left x (bits - k))
          in
          Ok (t, mask_value t v)
      | "popcount", [ (t, x) ] -> Ok (t, Int64.of_int (popcount64 x))
      | ("ctz" | "clz"), [ (t, 0L) ] ->
          let bits = match t with Hir.Int q -> int_bits q | _ -> 64 in
          Ok (t, Int64.of_int bits)
      | "ctz", [ (t, x) ] -> Ok (t, Int64.of_int (trailing64 x))
      | "clz", [ (t, x) ] ->
          let b = match t with Hir.Int q -> int_bits q | _ -> 64 in
          Ok (t, Int64.of_int (leading64 x - (64 - b)))
      | _ -> error s "invalid constant builtin call")
  | Ast.Sizeof (t, s) ->
      let* t = source_ty_with_values named_types consts s t in
      let* n, _ = layout_diag s structs t in
      Ok (Hir.Int Hir.Usize, Int64.of_int n)
  | Ast.Alignof (t, s) ->
      let* t = source_ty_with_values named_types consts s t in
      let* _, n = layout_diag s structs t in
      Ok (Hir.Int Hir.Usize, Int64.of_int n)
  | Ast.Offsetof (t, n, s) -> (
      let* t = source_ty_with_values named_types consts s t in
      match t with
      | Hir.Struct sn -> (
          match field_info structs sn n with
          | Some f -> Ok (Hir.Int Hir.Usize, Int64.of_int f.offset)
          | None -> error s "unknown field in offsetof")
      | _ -> error s "offsetof requires a struct")
  | expr -> error (Ast.expr_span expr) "expression is not compile-time constant"

let rec rooted_in_const_array = function
  | Hir.Const_array _ -> true
  | Hir.Index (a, _, _, _)
  | Hir.Field (a, _, _, _, _)
  | Hir.Deref (a, _, _)
  | Hir.Address (a, _, _)
  | Hir.Ptr_add (_, a, _, _, _)
  | Hir.Cast (_, a, _, _) ->
      rooted_in_const_array a
  | Hir.Ternary (_, a, b, _, _) -> rooted_in_const_array a || rooted_in_const_array b
  | _ -> false

let rec rooted_in_string_literal = function
  | Hir.EString _ -> true
  | Hir.Index (a, _, _, _)
  | Hir.Field (a, _, _, _, _)
  | Hir.Deref (a, _, _)
  | Hir.Address (a, _, _)
  | Hir.Ptr_add (_, a, _, _, _)
  | Hir.Cast (_, a, _, _) ->
      rooted_in_string_literal a
  | Hir.Ternary (_, a, b, _, _) ->
      rooted_in_string_literal a || rooted_in_string_literal b
  | _ -> false

let rec rooted_in_readonly_pointer = function
  | Hir.Deref (a, _, _) | Hir.Index (a, _, _, _) | Hir.Field (a, _, _, _, _) -> (
      match Hir.expr_ty a with
      | Hir.ConstPtr _ -> true
      | _ -> rooted_in_readonly_pointer a)
  | Hir.Address (a, _, _) | Hir.Ptr_add (_, a, _, _, _) | Hir.Cast (_, a, _, _) ->
      rooted_in_readonly_pointer a
  | Hir.Ternary (_, a, b, _, _) ->
      rooted_in_readonly_pointer a || rooted_in_readonly_pointer b
  | _ -> false

let require_place_value c span place =
  match Hir.expr_ty place.expr with
  | Hir.Ptr _ | Hir.ConstPtr _ -> (
      match (place.root, place.path) with
      | Some binding, Some path -> require_place_state binding path c span
      | _ -> Ok ())
  | _ -> Ok ()

let rec check_place (c : context) expr =
  let static_index e =
    match
      const_expr ~structs:c.structs ~named_types:c.named_types ~arrays:c.arrays c.consts
        None e
    with
    | Ok (ty, value) -> Known (ty, value)
    | Error _ -> Dynamic
  in
  match expr with
  | Ast.Ident (n, s) -> (
      match lookup_local n c with
      | Some b -> Ok { expr = Hir.Local (b, s); root = Some b; path = Some (Exact []) }
      | None -> (
          match lookup n c.arrays with
          | Some (_, t, _) ->
              Ok { expr = Hir.Const_array (n, t, s); root = None; path = None }
          | None -> error s (Printf.sprintf "unknown name `%s`" n)))
  | Ast.Index (a, i, s) -> (
      let* base = check_place c a in
      let* checked_index = check_expr c None i in
      if not (is_int (Hir.expr_ty checked_index)) then
        error s "array index must be an integer"
      else
        match Hir.expr_ty base.expr with
        | Hir.Array (length, e) | Hir.Vec (length, e) -> (
            match static_index i with
            | Known (ty, value)
              when let value = sign_extend_value ty value in
                   value < 0L || value >= Int64.of_int length ->
                error s "array index is out of bounds"
            | Known (ty, value) ->
                let value = sign_extend_value ty value in
                let index = Int64.to_int value in
                let path =
                  match (base.root, base.path) with
                  | Some _, Some (Exact path) -> Some (Exact (path @ [ Element index ]))
                  | Some _, Some (Dynamic_prefix path) -> Some (Dynamic_prefix path)
                  | _ -> None
                in
                Ok
                  {
                    expr = Hir.Index (base.expr, checked_index, e, s);
                    root = base.root;
                    path;
                  }
            | Dynamic ->
                Ok
                  {
                    expr = Hir.Index (base.expr, checked_index, e, s);
                    root = base.root;
                    path =
                      (match base.path with
                      | Some (Exact path) -> Some (Dynamic_prefix path)
                      | Some (Dynamic_prefix path) -> Some (Dynamic_prefix path)
                      | None -> None);
                  })
        | Hir.Ptr e | Hir.ConstPtr e ->
            let* () = require_place_value c (Hir.expr_span base.expr) base in
            if match e with Hir.Opaque _ -> true | _ -> false then
              error s "opaque pointers cannot be indexed"
            else if match e with Hir.Void -> true | _ -> false then
              error s "void pointers cannot be indexed"
            else
              Ok
                {
                  expr = Hir.Index (base.expr, checked_index, e, s);
                  root = None;
                  path = None;
                }
        | _ -> error s "cannot index this type")
  | Ast.Field (a, n, s) -> (
      let* base = check_place c a in
      match Hir.expr_ty base.expr with
      | Hir.Struct sn -> (
          match field_info c.structs sn n with
          | Some f ->
              Ok
                {
                  expr = Hir.Field (base.expr, n, f.ty, f.offset, s);
                  root = base.root;
                  path =
                    (match base.path with
                    | Some (Exact path) -> Some (Exact (path @ [ Field n ]))
                    | Some (Dynamic_prefix path) -> Some (Dynamic_prefix path)
                    | None -> None);
                }
          | None -> error s (Printf.sprintf "unknown field `%s`" n))
      | _ -> error s "field access requires a struct")
  | Ast.Deref (e, s) -> (
      let* x = check_expr c None e in
      match Hir.expr_ty x with
      | Hir.Ptr (Hir.Opaque _) | Hir.ConstPtr (Hir.Opaque _) ->
          error s "cannot dereference an opaque pointer"
      | Hir.Ptr Hir.Void | Hir.ConstPtr Hir.Void ->
          error s "cannot dereference a void pointer"
      | Hir.Ptr t | Hir.ConstPtr t ->
          Ok { expr = Hir.Deref (x, t, s); root = None; path = None }
      | _ -> error s "cannot dereference a non-pointer")
  | e ->
      let* checked = check_expr c None e in
      Ok { expr = checked; root = None; path = None }

and check_expr (c : context) expected = function
  | Ast.Int_lit (raw, s) ->
      let* v = parse_integer raw |> Result.map_error (fun m -> [ Diag.error s m ]) in
      let ty = Option.value ~default:(Hir.Int Hir.I32) expected in
      if not (fits_literal ty v) then
        error s ("integer literal is out of range for " ^ ty_name ty)
      else Ok (Hir.EInt (mask_value ty v, ty, s))
  | Ast.Bool_lit (v, s) -> Ok (Hir.EBool (v, s))
  | Ast.Null s -> (
      match expected with
      | Some (Hir.Ptr _ | Hir.ConstPtr _) as t -> Ok (Hir.Null (Option.get t, s))
      | _ -> error s "null requires a pointer context")
  | Ast.String_lit (cstr, v, s) ->
      if cstr && String.contains v '\000' then
        error s "C string literal cannot contain embedded NUL"
      else
        let value = if cstr then v ^ "\000" else v in
        Ok (Hir.EString (intern_string c value, s))
  | Ast.Ident (n, s) -> (
      match lookup_local n c with
      | Some b ->
          let* () = require_state b [] c s in
          Ok (Hir.Local (b, s))
      | None -> (
          match lookup n c.consts with
          | Some (_, t, v) ->
              let rt = Option.value ~default:t expected in
              if is_int rt && fits_int rt v then
                let v =
                  match (t, rt) with
                  | Hir.Int tk, Hir.Int rk when int_bits tk < int_bits rk ->
                      sign_extend_value t v
                  | _ -> mask_value rt v
                in
                Ok (Hir.EInt (v, rt, s))
              else Ok (Hir.EInt (v, t, s))
          | None -> (
              match lookup n c.arrays with
              | Some (_, t, _) -> Ok (Hir.Const_array (n, t, s))
              | None -> error s (Printf.sprintf "unknown name `%s`" n))))
  | Ast.Unary (Ast.Neg, Ast.Int_lit (raw, is), s) ->
      let* v = parse_integer raw |> Result.map_error (fun m -> [ Diag.error is m ]) in
      let t = Option.value ~default:(Hir.Int Hir.I32) expected in
      let allowed =
        match t with
        | Hir.Int ((Hir.I8 | I16 | I32 | I64 | Isize) as k) ->
            let b = int_bits k in
            (b = 64 && v = Int64.min_int) || (b < 64 && v = Int64.shift_left 1L (b - 1))
        | _ -> false
      in
      if fits_negative_literal t v || allowed then
        Ok (Hir.EInt (mask_value t (Int64.neg v), t, s))
      else error s ("integer literal is out of range for " ^ ty_name t)
  | Ast.Unary (op, e, s) -> (
      let* te =
        check_expr c
          (match op with Ast.Neg | Ast.Bit_not -> expected | Ast.Not -> None)
          e
      in
      match op with
      | Ast.Neg | Ast.Bit_not ->
          if not (is_int (Hir.expr_ty te)) then
            error s "integer unary operator requires an integer"
          else Ok (Hir.Unary (op, te, Hir.expr_ty te, s))
      | Ast.Not ->
          if not (is_truthy (Hir.expr_ty te)) then
            error s "logical not requires a scalar"
          else Ok (Hir.Unary (op, te, Hir.Bool, s)))
  | Ast.Binary (op, l, r, s) -> (
      if op = Ast.And || op = Ast.Or then (
        let* a = check_expr c None l in
        let after_left = c.initialized in
        let* b = check_expr c None r in
        let after_right = c.initialized in
        c.initialized <- merge_maps c after_left after_right;
        if is_truthy (Hir.expr_ty a) && is_truthy (Hir.expr_ty b) then
          Ok (Hir.Binary (op, a, b, Hir.Bool, s))
        else error s "logical operands must be scalar")
      else
        let* a, b =
          match l with
          | Ast.Int_lit _ | Ast.Unary (Ast.Neg, Ast.Int_lit _, _) -> (
              match expected with
              | Some t when is_int t ->
                  let* a = check_expr c (Some t) l in
                  let* b = check_expr c (Some t) r in
                  Ok (a, b)
              | _ ->
                  let* b = check_expr c None r in
                  let* a = check_expr c (Some (Hir.expr_ty b)) l in
                  Ok (a, b))
          | Ast.Null _ ->
              let* b = check_expr c None r in
              let* a = check_expr c (Some (Hir.expr_ty b)) l in
              Ok (a, b)
          | _ ->
              let* a = check_expr c expected l in
              let* b = check_expr c (Some (Hir.expr_ty a)) r in
              Ok (a, b)
        in
        let at = Hir.expr_ty a in
        let bt = Hir.expr_ty b in
        if
          (op = Ast.Div || op = Ast.Rem)
          && match b with Hir.EInt (v, _, _) -> v = 0L | _ -> false
        then error s "division by zero is not a defined runtime operation"
        else if
          not
            (equal at bt
            || ((op = Ast.Eq || op = Ast.Ne) && (compatible at bt || compatible bt at))
            )
        then error s "binary operands must have the same type"
        else
          match op with
          | Ast.Eq | Ast.Ne -> (
              match at with
              | Hir.Vec (length, (Hir.Bool | Hir.Int _)) ->
                  Ok (Hir.Binary (op, a, b, Hir.Vec (length, Hir.Bool), s))
              | _ when is_scalar at -> Ok (Hir.Binary (op, a, b, Hir.Bool, s))
              | _ -> error s "equality requires scalar or integer/bool-vector operands")
          | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> (
              if is_int at then Ok (Hir.Binary (op, a, b, Hir.Bool, s))
              else
                match at with
                | Hir.Vec (length, Hir.Int _) ->
                    Ok (Hir.Binary (op, a, b, Hir.Vec (length, Hir.Bool), s))
                | _ -> error s "ordered comparison requires integer operands")
          | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
          | Ast.Bit_xor ->
              if is_numeric at then Ok (Hir.Binary (op, a, b, at, s))
              else error s "arithmetic requires integer or vector operands"
          | Ast.And | Ast.Or -> error s "logical operators are handled separately")
  | Ast.Call (fn, args, s) -> check_call c None fn args s
  | Ast.Generic_args (_fn, _, s) ->
      error s "generic specialization is not available in this context"
  | Ast.Cast (k, t, e, s) ->
      let* t = source_ty_with_values c.named_types c.consts s t in
      let* x =
        check_expr c
          (if
             k = Ast.Bitcast
             &&
             match e with
             | Ast.Int_lit _ | Ast.Unary (Ast.Neg, Ast.Int_lit _, _) -> true
             | _ -> false
           then Some t
           else None)
          e
      in
      let from = Hir.expr_ty x in
      let bits = function
        | (Hir.Bool | Hir.Int _) as ty -> Option.get (integer_value_bit_width ty)
        | Hir.Ptr _ | Hir.ConstPtr _ -> Target_layout.current.pointer_size * 8
        | _ -> 0
      in
      let legal =
        match k with
        | Ast.Zext ->
            (from = Hir.Bool || is_int from) && is_int t && bits t >= bits from
        | Ast.Sext ->
            (from = Hir.Bool || is_int from) && is_int t && bits t >= bits from
        | Ast.Trunc ->
            (from = Hir.Bool && is_int t)
            || (is_int from && (is_int t || t = Hir.Bool) && bits t <= bits from)
        | Ast.Bitcast -> (
            match (from, t) with
            | Hir.Ptr _, Hir.Ptr _ | Hir.Ptr _, Hir.ConstPtr _ -> true
            | Hir.ConstPtr _, Hir.ConstPtr _ -> true
            | Hir.Ptr _, Hir.Int k | Hir.Int k, Hir.Ptr _ ->
                int_bits k = Target_layout.current.pointer_size * 8
            | _ -> (
                match (integer_value_bit_width from, integer_value_bit_width t) with
                | Some source_width, Some destination_width ->
                    source_width = destination_width
                | _ -> false))
      in
      if legal then Ok (Hir.Cast (k, x, t, s))
      else error s "illegal cast for source and destination widths"
  | Ast.Index (a, i, s) ->
      let* place = check_place c (Ast.Index (a, i, s)) in
      let* () =
        match (place.root, place.path) with
        | Some binding, Some path -> require_place_state binding path c s
        | _ -> Ok ()
      in
      Ok place.expr
  | Ast.Field (a, n, s) ->
      let* place = check_place c (Ast.Field (a, n, s)) in
      let* () =
        match (place.root, place.path) with
        | Some binding, Some path -> require_place_state binding path c s
        | _ -> Ok ()
      in
      Ok place.expr
  | Ast.Deref (e, s) -> (
      let* x = check_expr c None e in
      match Hir.expr_ty x with
      | Hir.Ptr (Hir.Opaque _) | Hir.ConstPtr (Hir.Opaque _) ->
          error s "cannot dereference an opaque pointer"
      | Hir.Ptr Hir.Void | Hir.ConstPtr Hir.Void ->
          error s "cannot dereference a void pointer"
      | Hir.Ptr t | Hir.ConstPtr t -> Ok (Hir.Deref (x, t, s))
      | _ -> error s "cannot dereference a non-pointer")
  | Ast.Addr_of (e, s) -> (
      let* place = check_place c e in
      (match place.root with Some binding -> set_state c binding [] Raw | None -> ());
      match place.expr with
      | Hir.Index (base, _, _, _)
        when match Hir.expr_ty base with Hir.Vec _ -> true | _ -> false ->
          error s "cannot take address of a vector lane"
      | Hir.Local _ | Hir.Deref _ | Hir.Index _ | Hir.Field _ | Hir.Const_array _ ->
          let ty =
            if
              rooted_in_const_array place.expr
              || rooted_in_string_literal place.expr
              || rooted_in_readonly_pointer place.expr
            then Hir.ConstPtr (Hir.expr_ty place.expr)
            else Hir.Ptr (Hir.expr_ty place.expr)
          in
          Ok (Hir.Address (place.expr, ty, s))
      | _ -> error s "cannot take the address of this expression")
  | Ast.Ptr_add (bytes, p, o, s) -> (
      let* tp = check_expr c None p in
      let* toff = check_expr c None o in
      if not (is_int (Hir.expr_ty toff)) then
        error s "pointer offset must be an integer"
      else
        match Hir.expr_ty tp with
        | Hir.Ptr (Hir.Opaque _) | Hir.ConstPtr (Hir.Opaque _) ->
            error s "pointer arithmetic on an opaque pointer is not allowed"
        | Hir.Ptr t -> Ok (Hir.Ptr_add (bytes, tp, toff, Hir.Ptr t, s))
        | Hir.ConstPtr t -> Ok (Hir.Ptr_add (bytes, tp, toff, Hir.ConstPtr t, s))
        | _ -> error s "pointer addition requires a pointer")
  | Ast.Sizeof (t, s) ->
      let* t = source_ty_with_values c.named_types c.consts s t in
      let* size, _ = layout_diag s c.structs t in
      Ok (Hir.Sizeof (t, size, s))
  | Ast.Alignof (t, s) ->
      let* t = source_ty_with_values c.named_types c.consts s t in
      let* _, a = layout_diag s c.structs t in
      Ok (Hir.Alignof (t, a, s))
  | Ast.Offsetof (t, n, s) -> (
      let* t = source_ty_with_values c.named_types c.consts s t in
      match t with
      | Hir.Struct sn -> (
          match field_info c.structs sn n with
          | Some f -> Ok (Hir.Offsetof (t, n, f.offset, s))
          | None -> error s (Printf.sprintf "unknown field `%s`" n))
      | _ -> error s "offsetof requires a struct type")
  | Ast.Splat (e, s) -> (
      match expected with
      | Some (Hir.Vec (n, elem)) ->
          let* x = check_expr c (Some elem) e in
          if equal (Hir.expr_ty x) elem then Ok (Hir.Splat (x, Hir.Vec (n, elem), s))
          else error s "splat element type mismatch"
      | _ -> error s "splat requires a vector type context")
  | Ast.Ternary (q, a, b, s) ->
      let* tq = check_expr c None q in
      if not (is_truthy (Hir.expr_ty tq)) then
        error s "ternary condition must be scalar"
      else
        let before_arms = c.initialized in
        let* ta = check_expr c expected a in
        let after_a = c.initialized in
        c.initialized <- before_arms;
        let* tb = check_expr c (Some (Hir.expr_ty ta)) b in
        let after_b = c.initialized in
        c.initialized <- merge_maps c after_a after_b;
        if not (equal (Hir.expr_ty ta) (Hir.expr_ty tb)) then
          error s "ternary arms have different types"
        else if Hir.expr_ty ta = Hir.Void || Hir.expr_ty tb = Hir.Void then
          error s "ternary arms cannot have void type"
        else Ok (Hir.Ternary (tq, ta, tb, Hir.expr_ty ta, s))
  | Ast.Array_lit (_, s) ->
      error s "array literals are only valid in global const declarations"
  | Ast.Struct_lit (source_type, xs, s) -> (
      let* literal_type = source_ty_with_values c.named_types c.consts s source_type in
      match literal_type with
      | Hir.Struct n -> (
          match List.find_opt (fun (d : Hir.struct_def) -> d.name = n) c.structs with
          | None -> error s (Printf.sprintf "unknown struct `%s`" n)
          | Some (d : Hir.struct_def) ->
              if List.length xs <> List.length d.fields then
                error s "wrong number of struct literal fields"
              else
                let rec go acc (fs : Hir.field list) es =
                  match (fs, es) with
                  | [], [] -> Ok (List.rev acc)
                  | f :: ft, e :: et ->
                      let* x = check_expr c (Some f.ty) e in
                      let* () =
                        ensure_expected (Hir.expr_ty x) f.ty (Ast.expr_span e)
                      in
                      go (x :: acc) ft et
                  | _ -> error s "wrong struct literal arity"
                in
                let* xs = go [] d.fields xs in
                Ok (Hir.Struct_lit (n, xs, literal_type, s)))
      | Hir.Opaque n -> error s (Printf.sprintf "opaque type `%s` is not a struct" n)
      | _ -> error s "struct literal requires a struct type")

and const_key_value (t : Hir.ty) v = Hir.ty_name t ^ ":" ^ Int64.to_string v

and mangle_specialization base values =
  base ^ "$spec$"
  ^ string_of_int (String.length base)
  ^ ":"
  ^ String.concat ";"
      (List.map
         (fun (n, t, v) ->
           string_of_int (String.length n) ^ ":" ^ n ^ "=" ^ const_key_value t v)
         values)

and function_specialization_key name values =
  ( Function_specialization,
    name,
    List.map (fun (_, t, v) -> Const_specialization_arg (t, v)) values )

and staged_specialization_identity state name values =
  let staged = ref None in
  Specialization_cache.iter
    (fun _ specialization ->
      if specialization.name = name then
        match (specialization.key, specialization.payload) with
        | ( (Function_specialization, origin, _),
            Function_payload { staged_args = Some arguments; _ } ) ->
            staged := Some (origin, arguments)
        | _ -> ())
    state.cache;
  match !staged with
  | None -> Ok None
  | Some (origin, arguments) ->
      let rec resolve acc = function
        | [] -> Ok (List.rev acc)
        | Staged_type_arg key :: rest ->
            resolve (Type_specialization_arg key :: acc) rest
        | Staged_const_arg name :: rest -> (
            match List.find_opt (fun (parameter, _, _) -> parameter = name) values with
            | Some (_, ty, value) ->
                resolve (Const_specialization_arg (ty, value) :: acc) rest
            | None ->
                error Span.synthetic
                  "internal error: staged const specialization argument is missing")
      in
      let* arguments = resolve [] arguments in
      Ok (Some (origin, arguments))

and mangle_mixed_specialization base arguments =
  let argument_name = function
    | Type_specialization_arg key -> "t" ^ string_of_int (String.length key) ^ ":" ^ key
    | Const_specialization_arg (ty, value) ->
        let key = const_key_value ty value in
        "c" ^ string_of_int (String.length key) ^ ":" ^ key
  in
  base ^ "$spec$" ^ String.concat ";" (List.map argument_name arguments)

and generic_const_argument span = function
  | Ast.Const_arg expression -> Ok expression
  | Ast.Name_arg (name, span) -> Ok (Ast.Ident (name, span))
  | Ast.Type_arg (Ast.Applied_type (name, [ argument ])) ->
      let* index = generic_const_argument span argument in
      Ok (Ast.Index (Ast.Ident (name, span), index, span))
  | Ast.Type_arg _ -> error span "expected a const argument"

and check_call c _expected fn args s =
  match fn with
  | Ast.Generic_args (Ast.Ident (name, _), generic_args, _) -> (
      match List.assoc_opt name c.templates with
      | None -> error s (Printf.sprintf "unknown generic function `%s`" name)
      | Some (Ast.Func { generic_params; _ } as item) ->
          let const_params = const_params generic_params in
          if has_type_params generic_params then
            error s "type-generic call reached ordinary type checking"
          else if List.length generic_args <> List.length const_params then
            error s (Printf.sprintf "wrong number of const arguments to `%s`" name)
          else
            let* cargs = Result_list.map (generic_const_argument s) generic_args in
            let rec eval acc cps actual =
              match (cps, actual) with
              | [], [] -> Ok (List.rev acc)
              | cp :: cs, a :: rest ->
                  let* ct = source_ty_diag c.named_types (Ast.expr_span a) cp.Ast.ty in
                  let* vt, v =
                    const_expr ~structs:c.structs ~named_types:c.named_types
                      ~arrays:c.arrays c.consts (Some ct) a
                  in
                  if equal vt ct then eval ((cp.name, ct, v) :: acc) cs rest
                  else error (Ast.expr_span a) "const argument type mismatch"
              | _ -> error s "const argument arity mismatch"
            in
            let* values = eval [] const_params cargs in
            let* staged =
              staged_specialization_identity c.specializations name values
            in
            let mangled, key =
              match staged with
              | None ->
                  ( mangle_specialization name values,
                    function_specialization_key name values )
              | Some (origin, arguments) ->
                  ( mangle_mixed_specialization origin arguments,
                    (Function_specialization, origin, arguments) )
            in
            let spec =
              {
                key;
                name = mangled;
                depth = c.spec_depth;
                payload =
                  Function_payload
                    { item; substitutions = []; values; staged_args = None };
              }
            in
            let* specialization =
              request_specialization c.specializations ~limits:c.limits
                ~depth:c.spec_depth ~span:s ~description:"const specialization" spec
            in
            let params, ret =
              match specialization.payload with
              | Function_payload
                  { item = Ast.Func { params; ret; _ }; substitutions = []; _ } ->
                  (params, ret)
              | _ -> assert false
            in
            let* ps =
              Result_list.map
                (fun (p : Ast.param) ->
                  let* t = source_ty_with_values c.named_types values p.span p.ty in
                  Ok (p.name, t))
                params
            in
            let* rt = source_ty_with_values c.named_types values s ret in
            let* checked = check_actuals c Reject s ps args in
            Ok (Hir.Call (Hir.User specialization.name, checked, rt, s))
      | Some _ -> error s "const-generic symbol is not a function")
  | Ast.Ident ("len", _) ->
      if List.length args <> 1 then error s "builtin `len` expects one argument"
      else
        let argument = List.hd args in
        let* n =
          match argument with
          | Ast.String_lit (cstr, value, _) ->
              if cstr && String.contains value '\000' then
                error s "C string literal cannot contain embedded NUL"
              else Ok (String.length value)
          | _ -> (
              let* value = check_expr c None argument in
              match Hir.expr_ty value with
              | Hir.Array (n, _) -> Ok n
              | _ -> error s "len requires a fixed array or string literal")
        in
        Ok (Hir.EInt (Int64.of_int n, Hir.Int Hir.Usize, s))
  | Ast.Ident (name, _) -> (
      let builtin =
        match name with
        | "shl" -> Some Hir.Shl
        | "lshr" -> Some Lshr
        | "ashr" -> Some Ashr
        | "rotl" -> Some Rotl
        | "rotr" -> Some Rotr
        | "popcount" -> Some Popcount
        | "ctz" -> Some Ctz
        | "clz" -> Some Clz
        | _ -> None
      in
      let check_builtin b =
        match b with
        | Hir.Popcount | Hir.Ctz | Hir.Clz ->
            if List.length args <> 1 then
              error s (Printf.sprintf "builtin `%s` expects one argument" name)
            else
              let* a = check_expr c None (List.hd args) in
              if is_int (Hir.expr_ty a) then
                Ok (Hir.Call (Hir.Builtin b, [ a ], Hir.expr_ty a, s))
              else error s "builtin argument must be an integer"
        | _ -> (
            if List.length args <> 2 then
              error s (Printf.sprintf "builtin `%s` expects two arguments" name)
            else
              let* a = check_expr c None (List.hd args) in
              let valid_operand =
                is_int (Hir.expr_ty a)
                ||
                match Hir.expr_ty a with
                | Hir.Vec (_, Hir.Int _) -> true
                | _ -> false
              in
              if not valid_operand then
                error s
                  "builtin arguments must be an integer or integer vector and an \
                   integer shift"
              else
                let count = List.hd (List.tl args) in
                match count with
                | Ast.Splat _ -> error s "vector shifts require a scalar integer count"
                | _ -> (
                    let* b2 = check_expr c None count in
                    if is_int (Hir.expr_ty b2) then
                      Ok (Hir.Call (Hir.Builtin b, [ a; b2 ], Hir.expr_ty a, s))
                    else
                      match Hir.expr_ty b2 with
                      | Hir.Vec _ ->
                          error s "vector shifts require a scalar integer count"
                      | _ ->
                          error s
                            "builtin arguments must be an integer or integer vector \
                             and an integer shift"))
      in
      match builtin with
      | Some b -> check_builtin b
      | None -> (
          match lookup_sig name c with
          | None -> (
              match List.assoc_opt name c.templates with
              | Some _ ->
                  error s
                    (Printf.sprintf "generic function `%s` requires arguments" name)
              | None -> error s (Printf.sprintf "unknown function `%s`" name))
          | Some sig_ ->
              if
                ((not sig_.variadic) && List.length args <> List.length sig_.params)
                || (sig_.variadic && List.length args < List.length sig_.params)
              then error s (Printf.sprintf "wrong number of arguments to `%s`" name)
              else
                let policy = if sig_.variadic then Promote_variadic else Reject in
                let* xs = check_actuals c policy s sig_.params args in
                Ok (Hir.Call (Hir.User name, xs, sig_.ret, s))))
  | _ -> error s "call target must be a function name"

and check_actuals c policy span formals actuals =
  let rec loop checked formals actuals =
    match (formals, actuals) with
    | [], rest ->
        let* trailing =
          match policy with
          | Reject ->
              if rest = [] then Ok [] else error span "wrong number of arguments"
          | Promote_variadic ->
              Result_list.map
                (fun expression ->
                  let* value = check_expr c None expression in
                  if is_scalar (Hir.expr_ty value) then Ok (variadic_promote value)
                  else
                    error (Ast.expr_span expression)
                      "unsupported variadic aggregate argument")
                rest
        in
        Ok (List.rev_append checked trailing)
    | (_, expected) :: formal_rest, expression :: actual_rest ->
        let* value = check_expr c (Some expected) expression in
        let* () =
          ensure_expected (Hir.expr_ty value) expected (Ast.expr_span expression)
        in
        loop (value :: checked) formal_rest actual_rest
    | _ -> error span "wrong number of arguments"
  in
  loop [] formals actuals

let check_target (c : context) = function
  | Ast.Target_ident (n, span) -> (
      match lookup_local n c with
      | Some b -> Ok { target = Hir.ALocal b; root = Some b; path = Some (Exact []) }
      | None -> error span (Printf.sprintf "unknown assignment target `%s`" n))
  | Ast.Target_deref e -> (
      let* x = check_expr c None e in
      if rooted_in_const_array x then
        error (Ast.expr_span e) "cannot modify const array"
      else
        match Hir.expr_ty x with
        | Hir.Ptr (Hir.Opaque _) | Hir.ConstPtr (Hir.Opaque _) ->
            error (Ast.expr_span e) "cannot dereference opaque pointer"
        | Hir.Ptr Hir.Void | Hir.ConstPtr Hir.Void ->
            error (Ast.expr_span e) "cannot dereference a void pointer"
        | Hir.Ptr _ -> Ok { target = Hir.ADeref x; root = None; path = None }
        | Hir.ConstPtr _ -> error (Ast.expr_span e) "cannot modify read-only pointer"
        | _ -> error (Ast.expr_span e) "deref assignment requires pointer")
  | Ast.Target_index (a, i) -> (
      let* place = check_place c (Ast.Index (a, i, Ast.expr_span a)) in
      let x = place.expr in
      match x with
      | Hir.Index (base, index, _, _) -> (
          if rooted_in_const_array x then
            error (Ast.expr_span a) "cannot modify const array"
          else
            match Hir.expr_ty base with
            | Hir.Ptr Hir.Void | Hir.ConstPtr Hir.Void ->
                error (Ast.expr_span a) "void pointers cannot be indexed"
            | Hir.Array (_, _) | Hir.Vec (_, _) | Hir.Ptr _ ->
                Ok
                  {
                    target = Hir.AIndex (base, index);
                    root = place.root;
                    path = place.path;
                  }
            | Hir.ConstPtr _ ->
                error (Ast.expr_span a) "cannot modify read-only pointer"
            | _ ->
                error (Ast.expr_span a) "index assignment requires aggregate or pointer"
          )
      | _ -> error (Ast.expr_span a) "index assignment requires aggregate or pointer")
  | Ast.Target_field (a, n) -> (
      let* place = check_place c (Ast.Field (a, n, Ast.expr_span a)) in
      let x = place.expr in
      match x with
      | Hir.Field (base, _, _, _, _) -> (
          if rooted_in_const_array x then
            error (Ast.expr_span a) "cannot modify const array"
          else if rooted_in_readonly_pointer x then
            error (Ast.expr_span a) "cannot modify read-only pointer"
          else
            match Hir.expr_ty base with
            | Hir.Struct sn -> (
                match field_info c.structs sn n with
                | Some f ->
                    Ok
                      {
                        target = Hir.AField (base, n, f.offset);
                        root = place.root;
                        path = place.path;
                      }
                | None ->
                    error (Ast.expr_span a) (Printf.sprintf "unknown field `%s`" n))
            | _ -> error (Ast.expr_span a) "field assignment requires struct")
      | _ -> error (Ast.expr_span a) "field assignment requires struct")

let target_ty c = function
  | Hir.ALocal binding -> Some binding.ty
  | Hir.ADeref expression -> (
      match Hir.expr_ty expression with
      | Hir.Ptr t | Hir.ConstPtr t -> Some t
      | _ -> None)
  | Hir.AIndex (expression, _) -> (
      match Hir.expr_ty expression with
      | Hir.Array (_, t) | Hir.Vec (_, t) | Hir.Ptr t | Hir.ConstPtr t -> Some t
      | _ -> None)
  | Hir.AField (expression, name, _) -> (
      match Hir.expr_ty expression with
      | Hir.Struct struct_name ->
          Option.map
            (fun (field : Hir.field) -> field.ty)
            (field_info c.structs struct_name name)
      | _ -> None)

let rec stmt_must_return = function
  | Hir.Return _ -> true
  | Hir.Block (body, _) -> block_must_return body
  | Hir.If (_, then_body, Some else_body, _) ->
      block_must_return then_body && block_must_return else_body
  | Hir.Switch (_, arms, Some default, _) ->
      block_must_return default
      && List.for_all (fun (_, body) -> block_must_return body) arms
  | _ -> false

and block_must_return body = List.exists stmt_must_return body

let rec stmt_terminates = function
  | Ast.Return _ | Ast.Break _ | Ast.Continue _ -> true
  | Ast.Block (body, _) -> block_terminates body
  | Ast.If (_, then_body, Some else_body, _) ->
      block_terminates then_body && block_terminates else_body
  | Ast.Switch (_, arms, Some default, _) ->
      block_terminates default
      && List.for_all (fun (_, body) -> block_terminates body) arms
  | _ -> false

and block_terminates body =
  match body with
  | [] -> false
  | statement :: rest ->
      if stmt_terminates statement then true else block_terminates rest

let rec check_block (c : context) stmts =
  push c;
  let rec go acc = function
    | [] ->
        let out = List.rev acc in
        pop c;
        Ok out
    | s :: rest ->
        let before = c.initialized in
        let* x = check_stmt c s in
        if not c.falls_through then c.initialized <- before
        else if stmt_terminates s then c.falls_through <- false;
        go (x :: acc) rest
  in
  go [] stmts

and check_stmt (c : context) = function
  | Ast.Let { name; ty; init; span } ->
      let* () =
        if
          Option.is_some (lookup name c.consts) || Option.is_some (lookup name c.arrays)
        then error span (Printf.sprintf "local `%s` shadows a const" name)
        else Ok ()
      in
      let* t = source_ty_with_values c.named_types c.consts span ty in
      let* () =
        object_type c.structs t |> Result.map_error (fun m -> [ Diag.error span m ])
      in
      let* () =
        if aggregate_within_limit c.limits c.structs t then Ok ()
        else error span "aggregate element count exceeds the configured limit"
      in
      let* binding = add_local name t c span in
      let* x =
        match init with
        | None -> Ok None
        | Some e ->
            let* v = check_expr c (Some t) e in
            let* () = ensure_expected (Hir.expr_ty v) t (Ast.expr_span e) in
            mark_init binding c;
            Ok (Some v)
      in
      Ok (Hir.Let (binding, x, span))
  | Ast.Assign (t, e, span) ->
      let* checked_target = check_target c t in
      let target = checked_target.target in
      let* expected =
        match target_ty c target with
        | Some t -> Ok t
        | None -> error span "invalid assignment target"
      in
      let* v = check_expr c (Some expected) e in
      let* () = ensure_expected (Hir.expr_ty v) expected span in
      (match (checked_target.root, checked_target.path) with
      | Some binding, Some (Exact path) -> set_state c binding path Full
      | _ -> ());
      Ok (Hir.Assign (target, v, span))
  | Ast.Compound_assign (t, op, e, span) ->
      let* checked_target = check_target c t in
      let target = checked_target.target in
      let* et =
        match target_ty c target with
        | Some t -> Ok t
        | None -> error span "invalid compound assignment target"
      in
      let* () =
        match (checked_target.root, checked_target.path) with
        | Some binding, Some path -> require_place_state binding path c span
        | _ -> Ok ()
      in
      let* v = check_expr c (Some et) e in
      let* () = ensure_expected (Hir.expr_ty v) et span in
      if not (is_numeric et) then
        error span "compound assignment requires an integer or vector"
      else (
        (match (checked_target.root, checked_target.path) with
        | Some binding, Some (Exact path) -> set_state c binding path Full
        | _ -> ());
        Ok (Hir.Compound_assign (target, op, v, et, span)))
  | Ast.Return (e, span) ->
      let* () =
        if c.in_defer then error span "return is not allowed inside defer" else Ok ()
      in
      let* x =
        match (e, c.ret_ty) with
        | None, Hir.Void -> Ok None
        | Some _, Hir.Void -> error span "void function cannot return a value"
        | Some e, t ->
            let* v = check_expr c (Some t) e in
            let* () = ensure_expected (Hir.expr_ty v) t span in
            Ok (Some v)
        | None, _ ->
            error span ("return value required (expected " ^ ty_name c.ret_ty ^ ")")
      in
      Ok (Hir.Return (x, span))
  | Ast.Expr_stmt (e, s) ->
      let* x = check_expr c None e in
      Ok (Hir.Expr (x, s))
  | Ast.Block (xs, s) ->
      let* x = check_block c xs in
      Ok (Hir.Block (x, s))
  | Ast.If (q, a, b, s) ->
      let* tq = check_expr c None q in
      if not (is_truthy (Hir.expr_ty tq)) then error s "if condition must be scalar"
      else
        let before = c.initialized in
        let before_falls = c.falls_through in
        c.falls_through <- true;
        let* ta = check_block c a in
        let ia = c.initialized in
        let fa = c.falls_through in
        c.initialized <- before;
        c.falls_through <- true;
        let* tb =
          match b with
          | None -> Ok None
          | Some xs ->
              let* x = check_block c xs in
              Ok (Some x)
        in
        let ib = c.initialized in
        let fb = c.falls_through in
        c.initialized <-
          (match (fa, fb) with
          | true, true -> merge_maps c ia ib
          | true, false -> ia
          | false, true -> ib
          | false, false -> before);
        c.falls_through <- fa || fb;
        if not before_falls then c.falls_through <- false;
        Ok (Hir.If (tq, ta, tb, s))
  | Ast.While (q, b, s) ->
      let* tq = check_expr c None q in
      if not (is_truthy (Hir.expr_ty tq)) then error s "while condition must be scalar"
      else
        let before = c.initialized in
        let before_falls = c.falls_through in
        c.loop_depth <- c.loop_depth + 1;
        c.falls_through <- true;
        let checked = check_block c b in
        c.loop_depth <- c.loop_depth - 1;
        let* tb = checked in
        c.initialized <- before;
        c.falls_through <- before_falls;
        Ok (Hir.While (tq, tb, s))
  | Ast.For (i, q, step, b, s) ->
      push c;
      let checked =
        let* ti =
          match i with
          | None -> Ok None
          | Some x ->
              let* y = check_stmt c x in
              Ok (Some y)
        in
        let* tq =
          match q with
          | None -> Ok None
          | Some x ->
              let* y = check_expr c None x in
              if is_truthy (Hir.expr_ty y) then Ok (Some y)
              else error (Ast.expr_span x) "for condition must be scalar"
        in
        let before = c.initialized in
        let before_falls = c.falls_through in
        c.loop_depth <- c.loop_depth + 1;
        c.falls_through <- true;
        let body_result = check_block c b in
        let* tb = body_result in
        c.initialized <- before;
        c.falls_through <- true;
        let* ts =
          match step with
          | None -> Ok None
          | Some x ->
              let* y = check_stmt c x in
              Ok (Some y)
        in
        c.initialized <- before;
        c.falls_through <- before_falls;
        c.loop_depth <- c.loop_depth - 1;
        Ok (Hir.For (ti, tq, ts, tb, s))
      in
      pop c;
      checked
  | Ast.Switch (e, arms, d, s) ->
      let* te = check_expr c None e in
      let et = Hir.expr_ty te in
      if not (is_int et || et = Hir.Bool) then
        error s "switch scrutinee must be an integer or bool"
      else
        let before = c.initialized and seen = ref [] and branch_states = ref [] in
        let before_falls = c.falls_through in
        let branch_falls = ref [] in
        let rec ar acc = function
          | [] -> Ok (List.rev acc)
          | (k, b) :: xs ->
              let* kt, kv =
                const_expr ~structs:c.structs ~named_types:c.named_types
                  ~arrays:c.arrays c.consts (Some et) k
                |> Result.map_error (fun _ ->
                    [
                      Diag.error (Ast.expr_span k)
                        "case label must be a compile-time constant";
                    ])
              in
              let* () = ensure_expected kt et (Ast.expr_span k) in
              if List.mem kv !seen then
                error (Ast.expr_span k) (Printf.sprintf "duplicate case label `%Ld`" kv)
              else (
                seen := kv :: !seen;
                let tk =
                  match et with
                  | Hir.Bool -> Hir.EBool (kv <> 0L, Ast.expr_span k)
                  | _ -> Hir.EInt (mask_value et kv, et, Ast.expr_span k)
                in
                c.initialized <- before;
                c.falls_through <- true;
                let* tb = check_block c b in
                if c.falls_through then branch_states := c.initialized :: !branch_states;
                branch_falls := c.falls_through :: !branch_falls;
                ar ((tk, tb) :: acc) xs)
        in
        let result = ar [] arms in
        let* ta = result in
        c.initialized <- before;
        c.falls_through <- true;
        let* td =
          match d with
          | None -> Ok None
          | Some x ->
              let* y = check_block c x in
              if c.falls_through then branch_states := c.initialized :: !branch_states;
              branch_falls := c.falls_through :: !branch_falls;
              Ok (Some y)
        in
        (match d with
        | None ->
            branch_states := before :: !branch_states;
            branch_falls := true :: !branch_falls
        | Some _ -> ());
        c.initialized <-
          (match !branch_states with
          | [] -> before
          | first :: rest -> List.fold_left (merge_maps c) first rest);
        c.falls_through <- List.exists (fun value -> value) !branch_falls;
        if not before_falls then c.falls_through <- false;
        Ok (Hir.Switch (te, ta, td, s))
  | Ast.Break s ->
      if c.in_defer then error s "break is not allowed inside defer"
      else if c.loop_depth = 0 then error s "break outside loop"
      else Ok (Hir.Break s)
  | Ast.Continue s ->
      if c.in_defer then error s "continue is not allowed inside defer"
      else if c.loop_depth = 0 then error s "continue outside loop"
      else Ok (Hir.Continue s)
  | Ast.Defer (xs, s) ->
      if c.in_defer then error s "nested defer is not allowed"
      else
        let before = c.initialized in
        let before_falls = c.falls_through in
        c.in_defer <- true;
        c.falls_through <- true;
        let checked = check_block c xs in
        c.in_defer <- false;
        c.initialized <- before;
        c.falls_through <- before_falls;
        let* body = checked in
        Ok (Hir.Defer (body, s))

let rec specialization_type_key = function
  | Ast.Bool -> "bool"
  | Ast.Void -> "void"
  | Ast.Int kind -> Ast.type_name (Ast.Int kind)
  | Ast.Ptr ty ->
      let key = specialization_type_key ty in
      "ptr" ^ string_of_int (String.length key) ^ "_" ^ key
  | Ast.Ptr_const ty ->
      let key = specialization_type_key ty in
      "cptr" ^ string_of_int (String.length key) ^ "_" ^ key
  | Ast.Array (length, ty) ->
      let length =
        try string_of_int (int_of_string length) with Failure _ -> length
      in
      let key = specialization_type_key ty in
      "arr" ^ length ^ "_" ^ string_of_int (String.length key) ^ "_" ^ key
  | Ast.Vec (length, ty) ->
      let length =
        try string_of_int (int_of_string length) with Failure _ -> length
      in
      let key = specialization_type_key ty in
      "vec" ^ length ^ "_" ^ string_of_int (String.length key) ^ "_" ^ key
  | Ast.Named_type name -> "named" ^ string_of_int (String.length name) ^ "_" ^ name
  | Ast.Applied_type (name, _) ->
      "applied" ^ string_of_int (String.length name) ^ "_" ^ name

let mangle_type_specialization base arguments =
  base ^ "$spec$"
  ^ String.concat "$"
      (List.map
         (fun ty ->
           let key = specialization_type_key ty in
           string_of_int (String.length key) ^ "_" ^ key)
         arguments)

let monomorphize_types ?eval_context ?(eager_functions = false) ~limits specializations
    program =
  let eval_structs, eval_named_types, eval_consts, eval_arrays =
    match eval_context with
    | None -> ([], [], [], [])
    | Some (structs, named_types, consts, arrays) ->
        (structs, named_types, consts, arrays)
  in
  let struct_templates =
    List.filter_map
      (function
        | Ast.Struct ({ name; generic_params = _ :: _; _ } as template) ->
            Some (name, Ast.Struct template)
        | _ -> None)
      program.Ast.items
  in
  let function_templates =
    List.filter_map
      (function
        | Ast.Func ({ name; generic_params = _ :: _; _ } as template) ->
            Some (name, Ast.Func template)
        | _ -> None)
      program.Ast.items
  in
  let struct_names =
    List.filter_map
      (function Ast.Struct { name; _ } -> Some name | _ -> None)
      program.Ast.items
  in
  let function_names =
    List.filter_map
      (function Ast.Func { name; _ } -> Some name | _ -> None)
      program.Ast.items
  in
  let named_type_names =
    List.filter_map
      (function
        | Ast.Opaque { name; _ } | Ast.Struct { name; _ } -> Some name | _ -> None)
      program.Ast.items
  in
  let generic_type_names = ref [] in
  let type_param_names generic_params =
    List.filter_map
      (function Ast.Type_param { name; _ } -> Some name | Ast.Const_param _ -> None)
      generic_params
  in
  let with_generic_type_names names f =
    let previous = !generic_type_names in
    generic_type_names := names;
    let result = f () in
    generic_type_names := previous;
    result
  in
  let generated = ref [] in
  let rec has_unresolved_application = function
    | Ast.Applied_type _ -> true
    | Ast.Ptr ty | Ast.Ptr_const ty | Ast.Array (_, ty) | Ast.Vec (_, ty) ->
        has_unresolved_application ty
    | Ast.Bool | Ast.Void | Ast.Int _ | Ast.Named_type _ -> false
  in
  let rec resolve_ty ?(values = []) ?(defer_const_structs = false) substitutions depth
      span = function
    | Ast.Bool -> Ok Ast.Bool
    | Ast.Void -> Ok Ast.Void
    | Ast.Int kind -> Ok (Ast.Int kind)
    | Ast.Ptr ty ->
        let* ty = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        Ok (Ast.Ptr ty)
    | Ast.Ptr_const ty ->
        let* ty = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        Ok (Ast.Ptr_const ty)
    | Ast.Array (length, ty) ->
        let* length = resolve_aggregate_length values span length in
        let* ty = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        Ok (Ast.Array (length, ty))
    | Ast.Vec (length, ty) ->
        let* length = resolve_aggregate_length values span length in
        let* ty = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        Ok (Ast.Vec (length, ty))
    | Ast.Named_type name -> (
        match List.assoc_opt name substitutions with
        | Some ty -> Ok ty
        | None ->
            if List.mem_assoc name struct_templates then
              error span
                (Printf.sprintf "generic struct `%s` requires type arguments" name)
            else if List.mem name named_type_names || List.mem name !generic_type_names
            then Ok (Ast.Named_type name)
            else error span (Printf.sprintf "unknown type `%s`" name))
    | Ast.Applied_type (name, arguments) -> (
        match List.assoc_opt name struct_templates with
        | None ->
            if List.mem name struct_names then
              error span (Printf.sprintf "struct `%s` is not generic" name)
            else error span (Printf.sprintf "unknown generic struct `%s`" name)
        | Some (Ast.Struct ({ generic_params; _ } as template)) ->
            if List.length arguments <> List.length generic_params then
              error span
                (Printf.sprintf "wrong number of generic arguments to `%s`" name)
            else if defer_const_structs && has_const_params generic_params then
              let rec resolve_arguments resolved params arguments =
                match (params, arguments) with
                | [], [] -> Ok (List.rev resolved)
                | Ast.Type_param _ :: params, argument :: arguments ->
                    let* ty =
                      match argument with
                      | Ast.Type_arg ty ->
                          resolve_ty ~values ~defer_const_structs substitutions depth
                            span ty
                      | Ast.Name_arg (name, _) ->
                          resolve_ty ~values ~defer_const_structs substitutions depth
                            span (Ast.Named_type name)
                      | Ast.Const_arg expression ->
                          error (Ast.expr_span expression) "expected a type argument"
                    in
                    resolve_arguments (Ast.Type_arg ty :: resolved) params arguments
                | Ast.Const_param _ :: params, argument :: arguments ->
                    let* expression = generic_const_argument span argument in
                    let* expression =
                      resolve_expr ~values ~defer_const_structs substitutions depth
                        expression
                    in
                    resolve_arguments
                      (Ast.Const_arg expression :: resolved)
                      params arguments
                | _ -> error span "generic argument arity mismatch"
              in
              let* arguments = resolve_arguments [] generic_params arguments in
              Ok (Ast.Applied_type (name, arguments))
            else
              let rec resolve_arguments resolved types bindings values_out params
                  arguments =
                match (params, arguments) with
                | [], [] ->
                    Ok
                      ( List.rev resolved,
                        List.rev types,
                        List.rev bindings,
                        List.rev values_out )
                | ( Ast.Type_param { name = parameter; _ } :: params,
                    argument :: arguments ) ->
                    let* argument =
                      match argument with
                      | Ast.Type_arg ty ->
                          resolve_ty ~values ~defer_const_structs substitutions depth
                            span ty
                      | Ast.Name_arg (name, _) ->
                          resolve_ty ~values ~defer_const_structs substitutions depth
                            span (Ast.Named_type name)
                      | Ast.Const_arg expression ->
                          error (Ast.expr_span expression) "expected a type argument"
                    in
                    resolve_arguments
                      (Type_specialization_arg (specialization_type_key argument)
                      :: resolved)
                      (argument :: types)
                      ((parameter, argument) :: bindings)
                      values_out params arguments
                | Ast.Const_param parameter :: params, argument :: arguments ->
                    let* expression = generic_const_argument span argument in
                    let* const_ty = source_ty_diag [] parameter.span parameter.ty in
                    let* actual_ty, value =
                      const_expr ~structs:eval_structs ~named_types:eval_named_types
                        ~arrays:eval_arrays (values @ eval_consts) (Some const_ty)
                        expression
                    in
                    if not (equal actual_ty const_ty) then
                      error (Ast.expr_span expression) "const argument type mismatch"
                    else
                      resolve_arguments
                        (Const_specialization_arg (const_ty, value) :: resolved)
                        types bindings
                        ((parameter.name, const_ty, value) :: values_out)
                        params arguments
                | _ -> error span "generic argument arity mismatch"
              in
              let* ordered_arguments, type_arguments, substitutions, values =
                resolve_arguments [] [] [] [] generic_params arguments
              in
              let specialization_name =
                if values = [] then mangle_type_specialization name type_arguments
                else mangle_mixed_specialization name ordered_arguments
              in
              let key = (Struct_specialization, name, ordered_arguments) in
              let specialization =
                {
                  key;
                  name = specialization_name;
                  depth;
                  payload =
                    Struct_payload
                      { template = Ast.Struct template; substitutions; values };
                }
              in
              let* specialization =
                request_specialization specializations ~limits ~depth ~span
                  ~description:"struct specialization" specialization
              in
              Ok (Ast.Named_type specialization.name)
        | Some _ -> error span "internal error: generic struct template is malformed")
  and resolve_expr ?(values = []) ?(defer_const_structs = false) substitutions depth =
    function
    | (Ast.Int_lit _ | Ast.Bool_lit _ | Ast.Null _ | Ast.String_lit _ | Ast.Ident _) as
      expression ->
        Ok expression
    | Ast.Unary (op, expression, span) ->
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.Unary (op, expression, span))
    | Ast.Binary (op, left, right, span) ->
        let* left =
          resolve_expr ~values ~defer_const_structs substitutions depth left
        in
        let* right =
          resolve_expr ~values ~defer_const_structs substitutions depth right
        in
        Ok (Ast.Binary (op, left, right, span))
    | Ast.Call (callee, arguments, span) ->
        let* callee =
          resolve_expr ~values ~defer_const_structs substitutions depth callee
        in
        let* arguments =
          Result_list.map
            (resolve_expr ~values ~defer_const_structs substitutions depth)
            arguments
        in
        Ok (Ast.Call (callee, arguments, span))
    | Ast.Generic_args (Ast.Ident (name, ident_span), arguments, span) -> (
        match List.assoc_opt name function_templates with
        | None ->
            if List.mem name function_names then
              error span (Printf.sprintf "function `%s` is not generic" name)
            else error span (Printf.sprintf "unknown generic function `%s`" name)
        | Some (Ast.Func ({ generic_params; _ } as template)) ->
            if List.length arguments <> List.length generic_params then
              let kind =
                if has_const_params generic_params then
                  if has_type_params generic_params then "generic" else "const"
                else "type"
              in
              error span
                (Printf.sprintf "wrong number of %s arguments to `%s`" kind name)
            else
              let rec resolve_arguments types bindings consts staged resolved params
                  arguments =
                match (params, arguments) with
                | [], [] ->
                    Ok
                      ( List.rev types,
                        List.rev bindings,
                        List.rev consts,
                        List.rev staged,
                        List.rev resolved )
                | ( Ast.Type_param { name = parameter; _ } :: params,
                    argument :: arguments ) ->
                    let* argument =
                      match argument with
                      | Ast.Type_arg ty ->
                          resolve_ty ~values ~defer_const_structs substitutions depth
                            span ty
                      | Ast.Name_arg (name, _) ->
                          resolve_ty ~values ~defer_const_structs substitutions depth
                            span (Ast.Named_type name)
                      | Ast.Const_arg expression ->
                          error (Ast.expr_span expression) "expected a type argument"
                    in
                    resolve_arguments (argument :: types)
                      ((parameter, argument) :: bindings)
                      consts
                      (Staged_type_arg (specialization_type_key argument) :: staged)
                      (Ast.Type_arg argument :: resolved)
                      params arguments
                | ( Ast.Const_param { name = parameter; _ } :: params,
                    argument :: arguments ) ->
                    let* expression = generic_const_argument span argument in
                    let* expression =
                      resolve_expr ~values ~defer_const_structs substitutions depth
                        expression
                    in
                    let argument = Ast.Const_arg expression in
                    resolve_arguments types bindings (argument :: consts)
                      (Staged_const_arg parameter :: staged)
                      (argument :: resolved) params arguments
                | _ -> error span "generic argument arity mismatch"
              in
              let* ( type_arguments,
                     type_substitutions,
                     const_arguments,
                     staged_args,
                     resolved_arguments ) =
                resolve_arguments [] [] [] [] [] generic_params arguments
              in
              if List.exists has_unresolved_application type_arguments then
                Ok
                  (Ast.Generic_args
                     (Ast.Ident (name, ident_span), resolved_arguments, span))
              else if type_arguments = [] then
                let* () =
                  if not eager_functions then Ok ()
                  else
                    let const_params = const_params generic_params in
                    let rec eval acc params arguments =
                      match (params, arguments) with
                      | [], [] -> Ok (List.rev acc)
                      | parameter :: params, argument :: arguments ->
                          let* expression = generic_const_argument span argument in
                          let* const_ty =
                            source_ty_diag eval_named_types parameter.Ast.span
                              parameter.ty
                          in
                          let* actual_ty, value =
                            const_expr ~structs:eval_structs
                              ~named_types:eval_named_types ~arrays:eval_arrays
                              (values @ eval_consts) (Some const_ty) expression
                          in
                          if not (equal actual_ty const_ty) then
                            error (Ast.expr_span expression)
                              "const argument type mismatch"
                          else
                            eval
                              ((parameter.name, const_ty, value) :: acc)
                              params arguments
                      | _ -> error span "const argument arity mismatch"
                    in
                    let* concrete_values = eval [] const_params const_arguments in
                    let* staged =
                      staged_specialization_identity specializations name
                        concrete_values
                    in
                    let specialization_name, key =
                      match staged with
                      | None ->
                          ( mangle_specialization name concrete_values,
                            function_specialization_key name concrete_values )
                      | Some (origin, arguments) ->
                          ( mangle_mixed_specialization origin arguments,
                            (Function_specialization, origin, arguments) )
                    in
                    let specialization =
                      {
                        key;
                        name = specialization_name;
                        depth;
                        payload =
                          Function_payload
                            {
                              item = Ast.Func template;
                              substitutions = [];
                              values = concrete_values;
                              staged_args = None;
                            };
                      }
                    in
                    let* _ =
                      request_specialization specializations ~limits ~depth ~span
                        ~description:"const specialization" specialization
                    in
                    Ok ()
                in
                Ok
                  (Ast.Generic_args (Ast.Ident (name, ident_span), const_arguments, span))
              else
                let specialization_name =
                  mangle_type_specialization name type_arguments
                in
                let key =
                  ( Function_specialization,
                    name,
                    List.map
                      (fun ty -> Type_specialization_arg (specialization_type_key ty))
                      type_arguments )
                in
                let specialization =
                  {
                    key;
                    name = specialization_name;
                    depth;
                    payload =
                      Function_payload
                        {
                          item = Ast.Func template;
                          substitutions = type_substitutions;
                          values = [];
                          staged_args =
                            (if const_arguments = [] then None else Some staged_args);
                        };
                  }
                in
                let* specialization =
                  request_specialization specializations ~limits ~depth ~span
                    ~description:"function specialization" specialization
                in
                if const_arguments = [] then
                  Ok (Ast.Ident (specialization.name, ident_span))
                else
                  Ok
                    (Ast.Generic_args
                       ( Ast.Ident (specialization.name, ident_span),
                         const_arguments,
                         span ))
        | Some _ -> error span "internal error: generic function template is malformed")
    | Ast.Generic_args (_, _, span) ->
        error span "generic call target must be a function name"
    | Ast.Cast (kind, ty, expression, span) ->
        let* ty = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.Cast (kind, ty, expression, span))
    | Ast.Index (base, index, span) ->
        let* base =
          resolve_expr ~values ~defer_const_structs substitutions depth base
        in
        let* index =
          resolve_expr ~values ~defer_const_structs substitutions depth index
        in
        Ok (Ast.Index (base, index, span))
    | Ast.Field (base, name, span) ->
        let* base =
          resolve_expr ~values ~defer_const_structs substitutions depth base
        in
        Ok (Ast.Field (base, name, span))
    | Ast.Deref (expression, span) ->
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.Deref (expression, span))
    | Ast.Addr_of (expression, span) ->
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.Addr_of (expression, span))
    | Ast.Ptr_add (bytes, pointer, offset, span) ->
        let* pointer =
          resolve_expr ~values ~defer_const_structs substitutions depth pointer
        in
        let* offset =
          resolve_expr ~values ~defer_const_structs substitutions depth offset
        in
        Ok (Ast.Ptr_add (bytes, pointer, offset, span))
    | Ast.Sizeof (ty, span) ->
        let* ty = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        Ok (Ast.Sizeof (ty, span))
    | Ast.Alignof (ty, span) ->
        let* ty = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        Ok (Ast.Alignof (ty, span))
    | Ast.Offsetof (ty, field, span) ->
        let* ty = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        Ok (Ast.Offsetof (ty, field, span))
    | Ast.Splat (expression, span) ->
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.Splat (expression, span))
    | Ast.Ternary (condition, yes, no, span) ->
        let resolve = resolve_expr ~values ~defer_const_structs substitutions depth in
        let* condition = resolve condition in
        let* yes = resolve yes in
        let* no = resolve no in
        Ok (Ast.Ternary (condition, yes, no, span))
    | Ast.Array_lit (elements, span) ->
        let* elements =
          Result_list.map
            (resolve_expr ~values ~defer_const_structs substitutions depth)
            elements
        in
        Ok (Ast.Array_lit (elements, span))
    | Ast.Struct_lit (ty, elements, span) ->
        let* ty = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        let* elements =
          Result_list.map
            (resolve_expr ~values ~defer_const_structs substitutions depth)
            elements
        in
        Ok (Ast.Struct_lit (ty, elements, span))
  and resolve_target ?(values = []) ?(defer_const_structs = false) substitutions depth =
    function
    | Ast.Target_ident _ as target -> Ok target
    | Ast.Target_deref expression ->
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.Target_deref expression)
    | Ast.Target_index (base, index) ->
        let resolve = resolve_expr ~values ~defer_const_structs substitutions depth in
        let* base = resolve base in
        let* index = resolve index in
        Ok (Ast.Target_index (base, index))
    | Ast.Target_field (base, name) ->
        let* base =
          resolve_expr ~values ~defer_const_structs substitutions depth base
        in
        Ok (Ast.Target_field (base, name))
  and resolve_stmt ?(values = []) ?(defer_const_structs = false) substitutions depth =
    function
    | Ast.Let { name; ty; init; span } ->
        let* ty = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        let* init =
          match init with
          | None -> Ok None
          | Some expression ->
              let* expression =
                resolve_expr ~values ~defer_const_structs substitutions depth expression
              in
              Ok (Some expression)
        in
        Ok (Ast.Let { name; ty; init; span })
    | Ast.Assign (target, expression, span) ->
        let* target =
          resolve_target ~values ~defer_const_structs substitutions depth target
        in
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.Assign (target, expression, span))
    | Ast.Compound_assign (target, op, expression, span) ->
        let* target =
          resolve_target ~values ~defer_const_structs substitutions depth target
        in
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.Compound_assign (target, op, expression, span))
    | Ast.Return (expression, span) ->
        let* expression =
          match expression with
          | None -> Ok None
          | Some expression ->
              let* expression =
                resolve_expr ~values ~defer_const_structs substitutions depth expression
              in
              Ok (Some expression)
        in
        Ok (Ast.Return (expression, span))
    | Ast.If (condition, yes, no, span) -> (
        let* condition =
          resolve_expr ~values ~defer_const_structs substitutions depth condition
        in
        let resolve = resolve_stmt ~values ~defer_const_structs substitutions depth in
        let known_condition =
          if values = [] then None
          else
            match
              const_expr ~structs:eval_structs ~named_types:eval_named_types
                ~arrays:eval_arrays (values @ eval_consts) None condition
            with
            | Ok (_, value) -> Some (value <> 0L)
            | Error _ -> None
        in
        match known_condition with
        | Some false ->
            let* no =
              match no with
              | None -> Ok []
              | Some statements -> Result_list.map resolve statements
            in
            Ok (Ast.If (condition, [], Some no, span))
        | Some true | None ->
            let* yes = Result_list.map resolve yes in
            let* no =
              match no with
              | None -> Ok None
              | Some statements ->
                  let* statements = Result_list.map resolve statements in
                  Ok (Some statements)
            in
            Ok (Ast.If (condition, yes, no, span)))
    | Ast.While (condition, body, span) ->
        let* condition =
          resolve_expr ~values ~defer_const_structs substitutions depth condition
        in
        let* body =
          Result_list.map
            (resolve_stmt ~values ~defer_const_structs substitutions depth)
            body
        in
        Ok (Ast.While (condition, body, span))
    | (Ast.Break _ | Ast.Continue _) as statement -> Ok statement
    | Ast.Defer (body, span) ->
        let* body =
          Result_list.map
            (resolve_stmt ~values ~defer_const_structs substitutions depth)
            body
        in
        Ok (Ast.Defer (body, span))
    | Ast.Expr_stmt (expression, span) ->
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.Expr_stmt (expression, span))
    | Ast.Block (body, span) ->
        let* body =
          Result_list.map
            (resolve_stmt ~values ~defer_const_structs substitutions depth)
            body
        in
        Ok (Ast.Block (body, span))
    | Ast.For (init, condition, step, body, span) ->
        let resolve_optional resolve = function
          | None -> Ok None
          | Some value ->
              let* value = resolve value in
              Ok (Some value)
        in
        let resolve_stmt =
          resolve_stmt ~values ~defer_const_structs substitutions depth
        in
        let resolve_expr =
          resolve_expr ~values ~defer_const_structs substitutions depth
        in
        let* init = resolve_optional resolve_stmt init in
        let* condition = resolve_optional resolve_expr condition in
        let* step = resolve_optional resolve_stmt step in
        let* body = Result_list.map resolve_stmt body in
        Ok (Ast.For (init, condition, step, body, span))
    | Ast.Switch (expression, cases, default, span) ->
        let resolve_expr =
          resolve_expr ~values ~defer_const_structs substitutions depth
        in
        let resolve_stmt =
          resolve_stmt ~values ~defer_const_structs substitutions depth
        in
        let* expression = resolve_expr expression in
        let* cases =
          Result_list.map
            (fun (value, body) ->
              let* value = resolve_expr value in
              let* body = Result_list.map resolve_stmt body in
              Ok (value, body))
            cases
        in
        let* default =
          match default with
          | None -> Ok None
          | Some body ->
              let* body = Result_list.map resolve_stmt body in
              Ok (Some body)
        in
        Ok (Ast.Switch (expression, cases, default, span))
  and resolve_function ?(values = []) substitutions depth specialization_name = function
    | Ast.Func ({ params; ret; body; generic_params; span; _ } as item) ->
        with_generic_type_names (type_param_names generic_params) (fun () ->
            let defer_const_structs = values = [] && has_const_params generic_params in
            let* params =
              Result_list.map
                (fun (parameter : Ast.param) ->
                  let* ty =
                    resolve_ty ~values ~defer_const_structs substitutions depth
                      parameter.span parameter.ty
                  in
                  Ok ({ parameter with ty } : Ast.param))
                params
            in
            let* ret =
              resolve_ty ~values ~defer_const_structs substitutions depth span ret
            in
            let* generic_params =
              Result_list.map
                (function
                  | Ast.Type_param _ -> Ok None
                  | Ast.Const_param parameter ->
                      let* ty =
                        resolve_ty ~values ~defer_const_structs substitutions depth
                          parameter.span parameter.ty
                      in
                      Ok (Some (Ast.Const_param { parameter with ty })))
                generic_params
            in
            let generic_params = List.filter_map Fun.id generic_params in
            let* body =
              match body with
              | Ast.Declaration -> Ok Ast.Declaration
              | Ast.Asm raw -> Ok (Ast.Asm raw)
              | Ast.Statements statements ->
                  let* statements =
                    Result_list.map
                      (resolve_stmt ~values ~defer_const_structs substitutions depth)
                      statements
                  in
                  Ok (Ast.Statements statements)
            in
            Ok
              (Ast.Func
                 {
                   item with
                   name = specialization_name;
                   params;
                   ret;
                   body;
                   linkage = Ast.Internal;
                   variadic = false;
                   generic_params;
                 }))
    | _ -> error Span.synthetic "internal error: function specialization is malformed"
  and resolve_item = function
    | Ast.Struct { generic_params = _ :: _; _ } as item -> Ok item
    | Ast.Struct ({ fields; _ } as item) ->
        let* fields =
          Result_list.map
            (fun (field : Ast.field) ->
              let* ty = resolve_ty [] 0 field.span field.ty in
              Ok ({ field with ty } : Ast.field))
            fields
        in
        Ok (Ast.Struct { item with fields })
    | Ast.Opaque _ as item -> Ok item
    | Ast.Const ({ ty; value; span; _ } as item) ->
        let* ty = resolve_ty [] 0 span ty in
        let* value = resolve_expr [] 0 value in
        Ok (Ast.Const { item with ty; value })
    | Ast.Func { generic_params = _ :: _; _ } as item -> Ok item
    | Ast.Func ({ params; ret; body; generic_params; span; _ } as item) ->
        let defer_const_structs = not eager_functions in
        let* params =
          Result_list.map
            (fun (parameter : Ast.param) ->
              let* ty =
                resolve_ty ~defer_const_structs [] 0 parameter.span parameter.ty
              in
              Ok ({ parameter with ty } : Ast.param))
            params
        in
        let* ret = resolve_ty ~defer_const_structs [] 0 span ret in
        let* generic_params =
          Result_list.map
            (function
              | Ast.Type_param _ as parameter -> Ok parameter
              | Ast.Const_param parameter ->
                  let* ty =
                    resolve_ty ~defer_const_structs [] 0 parameter.span parameter.ty
                  in
                  Ok (Ast.Const_param { parameter with ty }))
            generic_params
        in
        let* body =
          match body with
          | Ast.Declaration -> Ok Ast.Declaration
          | Ast.Asm raw -> Ok (Ast.Asm raw)
          | Ast.Statements statements ->
              let* statements =
                Result_list.map (resolve_stmt ~defer_const_structs [] 0) statements
              in
              Ok (Ast.Statements statements)
        in
        Ok (Ast.Func { item with params; ret; body; generic_params })
  in
  let* items = Result_list.map resolve_item program.Ast.items in
  let late_functions = ref [] in
  let rec materialize () =
    match Queue.take_opt specializations.queue with
    | None -> Ok ()
    | Some
        { payload = Struct_payload { template; substitutions; values }; name; depth; _ }
      -> (
        match template with
        | Ast.Struct { fields; align; span; generic_params; _ } ->
            let* fields =
              with_generic_type_names (type_param_names generic_params) (fun () ->
                  Result_list.map
                    (fun (field : Ast.field) ->
                      let* ty =
                        resolve_ty ~values substitutions (depth + 1) field.span field.ty
                      in
                      Ok ({ field with ty } : Ast.field))
                    fields)
            in
            generated :=
              Ast.Struct { name; generic_params = []; fields; align; span }
              :: !generated;
            materialize ()
        | _ -> error Span.synthetic "internal error: struct specialization is malformed"
        )
    | Some
        {
          payload =
            Function_payload { item; substitutions; values = []; staged_args = _ };
          name;
          depth;
          _;
        }
      when substitutions <> [] ->
        let* item = resolve_function substitutions (depth + 1) name item in
        generated := item :: !generated;
        materialize ()
    | Some
        ({
           payload =
             Function_payload { item; substitutions = []; values; staged_args = _ };
           name;
           depth;
           _;
         } as specialization)
      when eager_functions && values <> [] ->
        let* item = resolve_function ~values [] (depth + 1) name item in
        let specialization =
          {
            specialization with
            payload =
              Function_payload { item; substitutions = []; values; staged_args = None };
          }
        in
        Specialization_cache.replace specializations.cache specialization.key
          specialization;
        late_functions := specialization :: !late_functions;
        materialize ()
    | Some { payload = Function_payload _; _ } ->
        error Span.synthetic "internal error: const specialization was queued too early"
  in
  let* () = materialize () in
  List.iter
    (fun specialization -> Queue.add specialization specializations.queue)
    (List.rev !late_functions);
  Ok ({ Ast.items = items @ List.rev !generated } : Ast.program)

let check ?(limits = Limits.default) program =
  let specializations =
    { cache = Specialization_cache.create 32; queue = Queue.create () }
  in
  let rec collect_named_types seen acc = function
    | [] -> Ok (List.rev acc)
    | Ast.Opaque { name; span } :: rest ->
        if List.mem name seen then
          error span (Printf.sprintf "duplicate type `%s`" name)
        else collect_named_types (name :: seen) ((name, Opaque_name) :: acc) rest
    | Ast.Struct { name; generic_params; span; _ } :: rest ->
        if List.mem name seen then
          error span (Printf.sprintf "duplicate type `%s`" name)
        else
          let kind = if generic_params = [] then Struct_name else Generic_struct_name in
          collect_named_types (name :: seen) ((name, kind) :: acc) rest
    | _ :: rest -> collect_named_types seen acc rest
  in
  let* named_types = collect_named_types [] [] program.Ast.items in
  let validate_struct_alignment span = function
    | Some a ->
        Target_layout.validate_type_alignment Target_layout.current a
        |> Result.map_error (fun message -> [ Diag.error span message ])
    | None -> Ok ()
  in
  let* () =
    Result_list.iter
      (function
        | Ast.Struct { generic_params; _ } ->
            validate_generic_params named_types generic_params
        | Ast.Func { generic_params; _ } ->
            validate_generic_params named_types generic_params
        | _ -> Ok ())
      program.Ast.items
  in
  let base_structs_src =
    List.filter_map
      (function
        | Ast.Struct { name; generic_params = []; fields; align; _ } -> (
            let fields =
              Result_list.map
                (fun (field : Ast.field) ->
                  let* ty = source_ty named_types field.ty in
                  Ok (field.name, ty))
                fields
            in
            match fields with
            | Ok fields -> Some (name, fields, align)
            | Error _ -> None)
        | _ -> None)
      program.Ast.items
  in
  let base_structs =
    List.filter_map
      (fun (name, _, _) ->
        match Hir.compute_struct base_structs_src name with
        | Ok definition -> Some definition
        | Error _ -> None)
      base_structs_src
  in
  let early_consts =
    List.fold_left
      (fun consts -> function
        | Ast.Const { name; ty; value; _ } -> (
            match source_ty named_types ty with
            | Ok ty when is_int ty -> (
                match
                  const_expr ~structs:base_structs ~named_types consts (Some ty) value
                with
                | Ok (actual_ty, bits) when equal actual_ty ty ->
                    consts @ [ (name, ty, bits) ]
                | _ -> consts)
            | _ -> consts)
        | _ -> consts)
      [] program.Ast.items
  in
  let* program =
    monomorphize_types
      ~eval_context:(base_structs, named_types, early_consts, [])
      ~limits specializations program
  in
  let* named_types = collect_named_types [] [] program.Ast.items in
  let rec collect_structs named_types acc = function
    | [] -> Ok (List.rev acc)
    | Ast.Struct { generic_params = _ :: _; align; span; _ } :: rest ->
        let* () = validate_struct_alignment span align in
        collect_structs named_types acc rest
    | Ast.Struct { name; fields; align; span; _ } :: rest ->
        let* () = validate_struct_alignment span align in
        let rec collect_fields seen out = function
          | [] -> Ok (List.rev out)
          | (f : Ast.field) :: fields ->
              if List.mem f.name seen then
                error f.span (Printf.sprintf "duplicate field `%s`" f.name)
              else
                let* ty =
                  source_ty named_types f.ty
                  |> Result.map_error (fun message -> [ Diag.error f.span message ])
                in
                collect_fields (f.name :: seen) ((f.name, ty) :: out) fields
        in
        let* fields = collect_fields [] [] fields in
        collect_structs named_types ((name, fields, align) :: acc) rest
    | _ :: rest -> collect_structs named_types acc rest
  in
  let* structs_src = collect_structs named_types [] program.Ast.items in
  let rec build structs_src acc = function
    | [] -> Ok (List.rev acc)
    | (name, _, _) :: xs ->
        let* s =
          Hir.compute_struct structs_src name
          |> Result.map_error (fun m -> [ Diag.error Span.synthetic m ])
        in
        build structs_src (s :: acc) xs
  in
  let* structs = build structs_src [] structs_src in
  let source_obj t =
    let* t =
      source_ty named_types t
      |> Result.map_error (fun m -> [ Diag.error Span.synthetic m ])
    in
    let* () =
      object_type structs t
      |> Result.map_error (fun m -> [ Diag.error Span.synthetic m ])
    in
    if aggregate_within_limit limits structs t then Ok t
    else error Span.synthetic "aggregate element count exceeds the configured limit"
  in
  let map_params convert params =
    Result_list.map
      (fun (param : Ast.param) ->
        let* ty = convert param in
        Ok (param.name, ty))
      params
  in
  let consts = ref [] and arrays = ref [] in
  let eval_const_item = function
    | Ast.Const { name; ty; value; span } -> (
        if
          List.exists (fun (n, _, _) -> n = name) !consts
          || List.exists (fun (n, _, _) -> n = name) !arrays
        then error span (Printf.sprintf "duplicate const `%s`" name)
        else
          let* t = source_obj ty in
          match (t, value) with
          | Hir.Array (n, elem), Ast.Array_lit (xs, _) ->
              if List.length xs <> n then error span "const array length mismatch"
              else
                let rec values acc = function
                  | [] -> Ok (List.rev acc)
                  | x :: rest ->
                      let* vt, v =
                        const_expr ~structs ~named_types ~arrays:!arrays !consts
                          (Some elem) x
                      in
                      if equal vt elem then values (v :: acc) rest
                      else error (Ast.expr_span x) "const array element type mismatch"
                in
                let* vs = values [] xs in
                arrays := !arrays @ [ (name, t, vs) ];
                Ok ()
          | Hir.Array _, _ -> error span "const array needs a brace-list initializer"
          | _, Ast.Array_lit _ -> error span "brace-list requires an array type"
          | _, _ ->
              let* vt, v =
                const_expr ~structs ~named_types ~arrays:!arrays !consts (Some t) value
              in
              if equal vt t then (
                consts := !consts @ [ (name, t, v) ];
                Ok ())
              else error span "constant initializer type mismatch")
    | _ -> Ok ()
  in
  let* () = Result_list.iter eval_const_item program.items in
  let* program =
    monomorphize_types
      ~eval_context:(structs, named_types, !consts, !arrays)
      ~eager_functions:true ~limits specializations program
  in
  let* named_types = collect_named_types [] [] program.Ast.items in
  let* structs_src = collect_structs named_types [] program.Ast.items in
  let* structs = build structs_src [] structs_src in
  let source_obj t =
    let* t =
      source_ty named_types t
      |> Result.map_error (fun m -> [ Diag.error Span.synthetic m ])
    in
    let* () =
      object_type structs t
      |> Result.map_error (fun m -> [ Diag.error Span.synthetic m ])
    in
    if aggregate_within_limit limits structs t then Ok t
    else error Span.synthetic "aggregate element count exceeds the configured limit"
  in
  let source_params =
    map_params (fun (param : Ast.param) ->
        source_ty_diag named_types param.span param.ty)
  in
  let sigs = ref [] and declared_functions = ref [] in
  let* () =
    List.fold_left
      (fun r item ->
        let* () = r in
        match item with
        | Ast.Func { name; params; ret; variadic; linkage; span; generic_params; _ } ->
            let* () =
              if List.mem name reserved_builtin_names then
                error span
                  (Printf.sprintf
                     "`%s` is a reserved builtin name and cannot be used as a function \
                      name"
                     name)
              else Ok ()
            in
            if List.mem name !declared_functions then
              error span (Printf.sprintf "duplicate function `%s`" name)
            else if List.exists (fun (n, _, _) -> n = name) !arrays then
              error span (Printf.sprintf "duplicate declaration `%s`" name)
            else if name = "main" && generic_params <> [] then
              error span "entry point `main` cannot have generic parameters"
            else
              let () = declared_functions := name :: !declared_functions in
              let rec dup seen = function
                | [] -> None
                | (p : Ast.param) :: xs ->
                    if List.mem p.name seen then Some p else dup (p.name :: seen) xs
              in
              let* () =
                match dup [] params with
                | Some p ->
                    error p.span (Printf.sprintf "duplicate parameter `%s`" p.name)
                | None -> Ok ()
              in
              let* () =
                if linkage = Ast.External_c && generic_params <> [] then
                  error span
                    (if has_type_params generic_params then
                       "extern \"C\" functions cannot have type parameters"
                     else "extern \"C\" functions cannot have const parameters")
                else Ok ()
              in
              let* () = validate_generic_params named_types generic_params in
              if variadic && linkage <> Ast.External_c then
                error span "variadic functions require extern \"C\""
              else if generic_params <> [] then Ok ()
              else
                let* ps =
                  map_params (fun (param : Ast.param) -> source_obj param.ty) params
                in
                let* rt = source_ty_diag named_types span ret in
                let* () =
                  if linkage = Ast.External_c then
                    validate_extern_c_signature span params ps rt
                  else Ok ()
                in
                sigs := !sigs @ [ (name, { params = ps; ret = rt; variadic }) ];
                Ok ()
        | _ -> Ok ())
      (Ok ()) program.items
  in
  let templates =
    List.filter_map
      (fun item ->
        match item with
        | Ast.Func { name; generic_params = _ :: _; _ } -> Some (name, item)
        | _ -> None)
      program.items
  in
  let all_strings = ref [] and funcs = ref [] in
  let hir_linkage = function
    | Ast.External_c -> Hir.External_c
    | Ast.Internal -> Hir.Internal
  in
  let make_context ~extra_consts ~spec_depth ~ret_ty =
    {
      structs;
      named_types;
      consts = (if extra_consts = [] then !consts else extra_consts @ !consts);
      arrays = !arrays;
      signatures = !sigs;
      templates;
      specializations;
      spec_depth;
      locals = ref [];
      initialized = IM.empty;
      next_binding_id = 0;
      strings = !all_strings;
      string_ids = List.mapi (fun i value -> (value, i)) !all_strings;
      ret_ty;
      loop_depth = 0;
      in_defer = false;
      falls_through = true;
      limits;
    }
  in
  let hir_params params =
    List.mapi (fun id (name, ty) -> ({ Hir.name; ty; id } : Hir.local)) params
  in
  let check_function_body ~name ~description ~span ~params ~ret ~stmts ~linkage
      ~variadic ~extra_consts ~spec_depth ~require_return =
    let context = make_context ~extra_consts ~spec_depth ~ret_ty:ret in
    let* params =
      Result_list.map
        (fun (param_name, param_ty) ->
          let* binding = add_local param_name param_ty context span in
          mark_init binding context;
          Ok binding)
        params
    in
    let* body = check_block context stmts in
    let* () =
      if require_return && ret <> Hir.Void && not (block_must_return body) then
        error span (description ^ " `" ^ name ^ "` may reach the end without returning")
      else Ok ()
    in
    all_strings := context.strings;
    Ok
      ({ Hir.name; params; ret; body = Hir.Statements body; linkage; variadic }
        : Hir.func)
  in
  let add_func func = funcs := func :: !funcs in
  let check_func = function
    | Ast.Func { name; params; ret; body; linkage; variadic; generic_params = []; span }
      ->
        let* ret = source_ty_diag named_types span ret in
        let* params = source_params params in
        let linkage = hir_linkage linkage in
        let* func =
          match body with
          | Ast.Declaration ->
              Ok
                ({
                   Hir.name;
                   params = hir_params params;
                   ret;
                   body = Hir.Declaration;
                   linkage;
                   variadic;
                 }
                  : Hir.func)
          | Ast.Asm raw ->
              Ok
                ({
                   Hir.name;
                   params = hir_params params;
                   ret;
                   body = Hir.Asm raw;
                   linkage;
                   variadic;
                 }
                  : Hir.func)
          | Ast.Statements stmts ->
              check_function_body ~name ~description:"function" ~span ~params ~ret
                ~stmts ~linkage ~variadic ~extra_consts:[] ~spec_depth:0
                ~require_return:true
        in
        add_func func;
        Ok ()
    | _ -> Ok ()
  in
  let* () = Result_list.iter check_func program.items in
  let rec materialize () =
    match Queue.take_opt specializations.queue with
    | None -> Ok ()
    | Some sp -> (
        match sp.payload with
        | Function_payload
            {
              item =
                Ast.Func
                  {
                    params;
                    ret;
                    body = Ast.Statements stmts;
                    linkage;
                    variadic;
                    span;
                    _;
                  };
              substitutions = [];
              values;
              staged_args = _;
            } ->
            let* ret = source_ty_with_values named_types values span ret in
            let* params =
              Result_list.map
                (fun (parameter : Ast.param) ->
                  let* ty =
                    source_ty_with_values named_types values parameter.span parameter.ty
                  in
                  Ok (parameter.name, ty))
                params
            in
            let* func =
              check_function_body ~name:sp.name ~description:"specialized function"
                ~span ~params ~ret ~stmts ~linkage:(hir_linkage linkage) ~variadic
                ~extra_consts:values ~spec_depth:(sp.depth + 1) ~require_return:true
            in
            add_func func;
            materialize ()
        | Function_payload _ -> materialize ()
        | Struct_payload _ ->
            error Span.synthetic
              "internal error: struct specialization was queued too late")
  in
  let* () = materialize () in
  let hconsts =
    List.map
      (fun (n, t, v) -> ({ Hir.name = n; ty = t; bits = v } : Hir.const_def))
      !consts
  in
  let harrays =
    List.map
      (fun (n, t, vs) -> ({ Hir.name = n; ty = t; elems = vs } : Hir.const_arr_def))
      !arrays
  in
  Ok
    ({
       Hir.structs;
       consts = hconsts;
       const_arrays = harrays;
       funcs = List.rev !funcs;
       strings = !all_strings;
     }
      : Hir.program)
