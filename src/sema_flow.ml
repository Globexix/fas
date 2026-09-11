module State_map = Map.Make (Int)

type binding = Hir.local = { name : string; ty : Hir.ty; id : int }
type selector = Field of string | Element of int
type init_state = Uninit | Full | Raw | Partial of (selector * init_state) list
type place_path = Exact of selector list | Dynamic_prefix of selector list
type deferred_requirement = binding * selector list * Span.t

type checked_defer = {
  requirements : deferred_requirement list;
  effects : (binding * init_state) list;
  falls_through : bool;
}

type loop_init_flow = {
  keep_defer_depth : int;
  mutable break_states : init_state State_map.t list;
  mutable continue_states : init_state State_map.t list;
}

type t = {
  structs : Hir.struct_def list;
  locals : (string, binding) Hashtbl.t list ref;
  mutable initialized : init_state State_map.t;
  mutable next_binding_id : int;
  mutable loop_depth : int;
  mutable loop_init_flows : loop_init_flow list;
  mutable in_defer : bool;
  mutable collecting_defer : deferred_requirement list option;
  mutable defer_scopes : checked_defer list list;
  mutable falls_through : bool;
  mutable checking_dead : bool;
}

let error span message = Error [ Diag.error span message ]

let ( let* ) result continuation =
  match result with
  | Error diagnostics -> Error diagnostics
  | Ok value -> continuation value

let create ~initial_scope structs =
  {
    structs;
    locals = ref (if initial_scope then [ Hashtbl.create 8 ] else []);
    initialized = State_map.empty;
    next_binding_id = 0;
    loop_depth = 0;
    loop_init_flows = [];
    in_defer = false;
    collecting_defer = None;
    defer_scopes = [];
    falls_through = true;
    checking_dead = false;
  }

let lookup_local name flow =
  let rec find = function
    | [] -> None
    | scope :: rest -> (
        match Hashtbl.find_opt scope name with
        | Some binding -> Some binding
        | None -> find rest)
  in
  find !(flow.locals)

let ensure_new_local name flow span =
  let* () =
    if Names.reserved_binding_name name then
      error span
        (Printf.sprintf "`%s` is reserved and cannot be used as a binding" name)
    else Ok ()
  in
  let scope =
    match !(flow.locals) with scope :: _ -> scope | [] -> Hashtbl.create 8
  in
  if Hashtbl.mem scope name then error span (Printf.sprintf "duplicate local `%s`" name)
  else Ok ()

let add_local name ty flow span =
  let scope =
    match !(flow.locals) with scope :: _ -> scope | [] -> Hashtbl.create 8
  in
  let* () = ensure_new_local name flow span in
  let id = flow.next_binding_id in
  flow.next_binding_id <- id + 1;
  let binding : binding = { ty; id; name } in
  Hashtbl.replace scope name binding;
  if !(flow.locals) = [] then flow.locals := [ scope ];
  Ok binding

let push flow =
  flow.locals := Hashtbl.create 8 :: !(flow.locals);
  flow.defer_scopes <- [] :: flow.defer_scopes

let pop flow =
  (match !(flow.locals) with _ :: rest -> flow.locals := rest | [] -> ());
  match flow.defer_scopes with _ :: rest -> flow.defer_scopes <- rest | [] -> ()

let state_of flow binding =
  Option.value ~default:Uninit (State_map.find_opt binding.id flow.initialized)

let state_usable = function Full | Raw -> true | Uninit | Partial _ -> false

let child_type flow ty selector =
  match (ty, selector) with
  | Hir.Struct name, Field field ->
      Option.map
        (fun (field : Hir.field) -> field.ty)
        (Sema_types.field_info flow.structs name field)
  | (Hir.Array (_, element) | Hir.Vec (_, element)), Element _ -> Some element
  | _ -> None

let is_vacuous_type flow ty =
  let rec check seen = function
    | Hir.Struct name ->
        if List.mem name seen then false
        else
          Option.value ~default:false
            (Option.map
               (fun (struct_def : Hir.struct_def) ->
                 List.for_all
                   (fun (field : Hir.field) -> check (name :: seen) field.ty)
                   struct_def.fields)
               (List.find_opt
                  (fun (struct_def : Hir.struct_def) -> struct_def.name = name)
                  flow.structs))
    | Hir.Array (length, element) -> length = 0 || check seen element
    | _ -> false
  in
  check [] ty

let rec normalize_state flow ty = function
  | Uninit -> Uninit
  | (Full | Raw) as state -> state
  | Partial entries ->
      let entries =
        List.filter_map
          (fun (selector, state) ->
            match child_type flow ty selector with
            | None -> None
            | Some child_ty ->
                let state = normalize_state flow child_ty state in
                if state = Uninit then None else Some (selector, state))
          entries
      in
      let complete, any_raw =
        match ty with
        | Hir.Struct name -> (
            match
              List.find_opt
                (fun (struct_def : Hir.struct_def) -> struct_def.name = name)
                flow.structs
            with
            | None -> (false, false)
            | Some struct_def ->
                let all =
                  List.for_all
                    (fun (field : Hir.field) ->
                      match List.assoc_opt (Field field.name) entries with
                      | Some state ->
                          state_usable state || is_vacuous_type flow field.ty
                      | None -> is_vacuous_type flow field.ty)
                    struct_def.fields
                in
                let raw =
                  List.exists
                    (fun (field : Hir.field) ->
                      match List.assoc_opt (Field field.name) entries with
                      | Some Raw -> true
                      | _ -> false)
                    struct_def.fields
                in
                (all, raw))
        | Hir.Array (length, element_ty) | Hir.Vec (length, element_ty) ->
            let all =
              length >= 0
              &&
              let rec each index =
                if index = length then true
                else
                  match List.assoc_opt (Element index) entries with
                  | Some state when state_usable state -> each (index + 1)
                  | Some _ | None ->
                      if is_vacuous_type flow element_ty then each (index + 1)
                      else false
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

let rec update_state flow ty state path replacement =
  match path with
  | [] -> replacement
  | selector :: rest -> (
      if state_usable state then state
      else
        let entries =
          match state with Partial entries -> entries | Uninit -> [] | _ -> []
        in
        match child_type flow ty selector with
        | None -> state
        | Some child_ty ->
            let child =
              Option.value ~default:Uninit (List.assoc_opt selector entries)
            in
            let child = update_state flow child_ty child rest replacement in
            let entries =
              List.remove_assoc selector entries |> fun entries ->
              if child = Uninit then entries else (selector, child) :: entries
            in
            normalize_state flow ty (Partial entries))

let set_state flow binding path replacement =
  let state = update_state flow binding.ty (state_of flow binding) path replacement in
  if state = Uninit then
    flow.initialized <- State_map.remove binding.id flow.initialized
  else flow.initialized <- State_map.add binding.id state flow.initialized

let require_state binding path flow span =
  let rec walk ty state = function
    | [] -> if state_usable state || is_vacuous_type flow ty then Ok () else Error ()
    | selector :: rest -> (
        match state with
        | Full | Raw -> Ok ()
        | Partial entries -> (
            match child_type flow ty selector with
            | Some child_ty ->
                let child =
                  Option.value ~default:Uninit (List.assoc_opt selector entries)
                in
                walk child_ty child rest
            | None -> Error ())
        | Uninit -> (
            match child_type flow ty selector with
            | Some child_ty -> walk child_ty Uninit rest
            | None -> Error ()))
  in
  if walk binding.ty (state_of flow binding) path = Ok () then Ok ()
  else
    match flow.collecting_defer with
    | Some requirements ->
        flow.collecting_defer <- Some ((binding, path, span) :: requirements);
        Ok ()
    | None -> error span (Printf.sprintf "use of uninitialized local `%s`" binding.name)

let require_place_state binding path flow span =
  match path with
  | Exact path | Dynamic_prefix path -> require_state binding path flow span

let merge_state flow ty left right =
  let rec merge ty left right =
    let merge_partial whole entries =
      Partial
        (List.filter_map
           (fun (selector, state) ->
             match child_type flow ty selector with
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
              (fun accumulated selector ->
                if List.mem selector accumulated then accumulated
                else selector :: accumulated)
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
                 match child_type flow ty selector with
                 | Some child_ty -> (
                     match merge child_ty left right with
                     | Uninit -> None
                     | state -> Some (selector, state))
                 | None -> None)
               selectors)
    in
    normalize_state flow ty result
  in
  merge ty left right

let merge_maps flow left right =
  State_map.merge
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
            (fun binding -> merge_state flow binding.ty left right)
            (find !(flow.locals))
      | _ -> None)
    left right

let add_init_state flow ty left right =
  let rec add ty left right =
    match (left, right) with
    | Uninit, state | state, Uninit -> state
    | Full, _ | _, Full -> Full
    | Raw, _ | _, Raw -> Raw
    | Partial left, Partial right ->
        let selectors = List.map fst left @ List.map fst right in
        let selectors = List.sort_uniq compare selectors in
        Partial
          (List.filter_map
             (fun selector ->
               match child_type flow ty selector with
               | None -> None
               | Some child_ty ->
                   let left =
                     Option.value ~default:Uninit (List.assoc_opt selector left)
                   in
                   let right =
                     Option.value ~default:Uninit (List.assoc_opt selector right)
                   in
                   let state = add child_ty left right in
                   if state = Uninit then None else Some (selector, state))
             selectors)
  in
  normalize_state flow ty (add ty left right)

let apply_defer_effect flow (binding, deferred_state) =
  let current = state_of flow binding in
  let state = add_init_state flow binding.ty current deferred_state in
  if state = Uninit then
    flow.initialized <- State_map.remove binding.id flow.initialized
  else flow.initialized <- State_map.add binding.id state flow.initialized

let rec validate_defer_list flow = function
  | [] -> Ok true
  | deferred :: rest ->
      let* () =
        Result_list.iter
          (fun (binding, path, span) -> require_state binding path flow span)
          deferred.requirements
      in
      List.iter (apply_defer_effect flow) deferred.effects;
      if deferred.falls_through then validate_defer_list flow rest else Ok false

let validate_defer_scopes flow keep =
  let count = List.length flow.defer_scopes - keep in
  let rec run remaining = function
    | _ when remaining <= 0 -> Ok true
    | [] -> Ok true
    | scope :: rest ->
        let* falls_through = validate_defer_list flow scope in
        if falls_through then run (remaining - 1) rest else Ok false
  in
  run count flow.defer_scopes

let exit_defer_state flow keep =
  let before = flow.initialized in
  let result = validate_defer_scopes flow keep in
  let after = flow.initialized in
  flow.initialized <- before;
  let* falls_through = result in
  Ok (if falls_through then Some after else None)

let validate_exit_defers flow keep =
  let* _ = exit_defer_state flow keep in
  Ok ()

let merge_flow_states flow = function
  | [] -> None
  | state :: states -> Some (List.fold_left (merge_maps flow) state states)

let mark_init binding flow = set_state flow binding [] Full

let with_dead_check flow dead check =
  let previous = flow.checking_dead in
  flow.checking_dead <- previous || dead;
  let result = check () in
  flow.checking_dead <- previous;
  result
