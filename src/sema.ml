open Sema_numeric
open Sema_specialization
open Sema_types
open Sema_context
module String_set = Set.Make (String)
module String_map = Map.Make (String)

let error span message = Error [ Diag.error span message ]
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let phase_invariant = function
  | Ok () -> Ok ()
  | Error message -> error Span.synthetic ("internal error: " ^ message)

let validate_generic_params named_types params =
  let* () =
    Result_list.iter
      (function
        | Ast.Type_param { name; span } -> validate_binding_name span name
        | Ast.Const_param cp ->
            let* () = validate_binding_name cp.span cp.name in
            let* t = source_ty_diag named_types cp.span cp.ty in
            if t = Hir.Bool || is_int t then Ok ()
            else error cp.span "const parameter type must be a scalar integer or bool")
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

let validate_function_params generic_params params =
  let generic_names =
    List.map
      (function Ast.Type_param { name; _ } | Ast.Const_param { name; _ } -> name)
      generic_params
  in
  let rec validate seen = function
    | [] -> Ok ()
    | (parameter : Ast.param) :: rest ->
        let* () = validate_binding_name parameter.span parameter.name in
        if List.mem parameter.name generic_names then
          error parameter.span
            (Printf.sprintf "parameter `%s` conflicts with a generic parameter"
               parameter.name)
        else if List.mem parameter.name seen then
          error parameter.span
            (Printf.sprintf "duplicate parameter `%s`" parameter.name)
        else validate (parameter.name :: seen) rest
  in
  validate [] params

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

let check ?(limits = Limits.default) program =
  let* () =
    if limits.Limits.max_type_nodes >= 0 then Ok ()
    else
      error Span.synthetic
        (Printf.sprintf "budget max_type_nodes must not be negative (profile %s)"
           (Limits.budget_profile_name limits))
  in
  let type_node_account = create_type_node_account limits in
  let specializations = Sema_specialization.create () in
  let declaration = function
    | Ast.Opaque { name; span } | Ast.Struct { name; span; _ } ->
        Some (name, Top_type, span)
    | Ast.Const { name; span; _ } -> Some (name, Top_const, span)
    | Ast.Func { name; span; _ } -> Some (name, Top_function, span)
  in
  let rec validate_declarations next_id seen bindings = function
    | [] -> Ok (List.rev bindings)
    | item :: rest -> (
        match declaration item with
        | None -> validate_declarations next_id seen bindings rest
        | Some (name, kind, span) -> (
            let* () = validate_binding_name span name in
            match String_map.find_opt name seen with
            | None ->
                let binding =
                  {
                    declaration_id = next_id;
                    declaration_name = name;
                    declaration_kind = kind;
                  }
                in
                validate_declarations (next_id + 1)
                  (String_map.add name kind seen)
                  (binding :: bindings) rest
            | Some previous when previous = kind ->
                let label =
                  match kind with
                  | Top_type -> "type"
                  | Top_const -> "const"
                  | Top_function -> "function"
                in
                error span (Printf.sprintf "duplicate %s `%s`" label name)
            | Some _ -> error span (Printf.sprintf "duplicate declaration `%s`" name)))
  in
  let* top_level_bindings =
    validate_declarations 0 String_map.empty [] program.Ast.items
  in
  let* () =
    Sema_invariants.check_declarations
      (List.map
         (fun binding -> (binding.declaration_id, binding.declaration_name))
         top_level_bindings)
    |> phase_invariant
  in
  let rec collect_named_types seen acc = function
    | [] -> Ok (List.rev acc)
    | Ast.Opaque { name; span } :: rest ->
        if String_set.mem name seen then
          error span (Printf.sprintf "duplicate type `%s`" name)
        else
          collect_named_types (String_set.add name seen) ((name, Opaque_name) :: acc)
            rest
    | Ast.Struct { name; generic_params; span; _ } :: rest ->
        if String_set.mem name seen then
          error span (Printf.sprintf "duplicate type `%s`" name)
        else
          let kind = if generic_params = [] then Struct_name else Generic_struct_name in
          collect_named_types (String_set.add name seen) ((name, kind) :: acc) rest
    | _ :: rest -> collect_named_types seen acc rest
  in
  let* named_types = collect_named_types String_set.empty [] program.Ast.items in
  let* () =
    Result_list.iter
      (function
        | Ast.Struct { generic_params; _ } ->
            validate_generic_params named_types generic_params
        | Ast.Func { generic_params; params; _ } ->
            let* () = validate_generic_params named_types generic_params in
            validate_function_params generic_params params
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
  let base_struct_cache = Hir.struct_layout_cache base_structs_src in
  let base_structs =
    List.filter_map
      (fun (name, _, _) ->
        match Hir.compute_struct_cached base_struct_cache name with
        | Ok definition -> Some definition
        | Error _ -> None)
      base_structs_src
  in
  let* early_consts =
    Sema_constants.resolve_scalar_declarations ~structs:base_structs ~named_types
      ~resolve_type:(fun span ty ->
        source_ty named_types ty
        |> Result.map_error (fun message -> [ Diag.error span message ]))
      ~strict:false program.Ast.items
  in
  let* program =
    Sema_substitution.monomorphize_types ~check_expr:Sema_check.check_expr
      ~check_stmt:Sema_check.check_stmt ~check_target:Sema_check.check_target
      ~target_ty:Sema_check.target_ty
      ~generic_const_argument:Sema_check.generic_const_argument
      ~eval_context:(base_structs, named_types, early_consts, [])
      ~top_level_bindings ~limits ~type_node_account specializations program
  in
  let* named_types = collect_named_types String_set.empty [] program.Ast.items in
  let rec collect_structs named_types acc = function
    | [] -> Ok (List.rev acc)
    | Ast.Struct { generic_params = _ :: _; align; span; _ } :: rest ->
        let* () = Sema_limits.validate_struct_alignment limits span align in
        collect_structs named_types acc rest
    | Ast.Struct { name; fields; align; span; _ } :: rest ->
        let result =
          let* () = Sema_limits.validate_struct_alignment limits span align in
          let rec collect_fields seen out = function
            | [] -> Ok (List.rev out)
            | (f : Ast.field) :: fields ->
                if String_set.mem f.name seen then
                  error f.span (Printf.sprintf "duplicate field `%s`" f.name)
                else
                  let* ty =
                    source_ty named_types f.ty
                    |> Result.map_error (fun message -> [ Diag.error f.span message ])
                  in
                  collect_fields (String_set.add f.name seen) ((f.name, ty) :: out)
                    fields
          in
          collect_fields String_set.empty [] fields
        in
        let* fields =
          result
          |> trace_result specializations
               (specialization_trace specializations Struct_specialization name)
        in
        collect_structs named_types ((name, fields, align) :: acc) rest
    | _ :: rest -> collect_structs named_types acc rest
  in
  let* structs_src = collect_structs named_types [] program.Ast.items in
  let build structs_src =
    let cache = Hir.struct_layout_cache structs_src in
    let rec go acc = function
      | [] -> Ok (List.rev acc)
      | (name, _, _) :: xs ->
          let* s =
            Hir.compute_struct_cached cache name
            |> Result.map_error (fun m -> [ Diag.error Span.synthetic m ])
            |> trace_result specializations
                 (specialization_trace specializations Struct_specialization name)
          in
          go (s :: acc) xs
    in
    go [] structs_src
  in
  let* structs = build structs_src in
  let source_obj span t =
    let* t =
      source_ty named_types t |> Result.map_error (fun m -> [ Diag.error span m ])
    in
    Sema_limits.validate_object limits structs span t
  in
  let map_params convert params =
    Result_list.map
      (fun (param : Ast.param) ->
        let* ty = convert param in
        Ok (param.name, ty))
      params
  in
  let* scalar_consts =
    Sema_constants.resolve_scalar_declarations ~structs ~named_types
      ~resolve_type:source_obj ~strict:true program.Ast.items
  in
  let* consts_ordered, arrays_ordered, arrays_names =
    Sema_const_env.collect ~source_obj ~structs ~named_types ~scalar_consts program
  in
  let* () =
    Sema_invariants.check_const_environment
      ~declared:
        (List.filter_map
           (function Ast.Const { name; _ } -> Some name | _ -> None)
           program.Ast.items)
      ~early:(List.map (fun (name, _, _) -> name) scalar_consts)
      ~consts:(List.map (fun (name, _, _) -> name) consts_ordered)
      ~arrays:(List.map (fun (name, _, _) -> name) arrays_ordered)
    |> phase_invariant
  in
  let* program =
    Sema_substitution.monomorphize_types ~check_expr:Sema_check.check_expr
      ~check_stmt:Sema_check.check_stmt ~check_target:Sema_check.check_target
      ~target_ty:Sema_check.target_ty
      ~generic_const_argument:Sema_check.generic_const_argument
      ~eval_context:(structs, named_types, consts_ordered, arrays_ordered)
      ~eager_functions:true ~top_level_bindings ~limits ~type_node_account
      specializations program
  in
  let* named_types = collect_named_types String_set.empty [] program.Ast.items in
  let* structs_src = collect_structs named_types [] program.Ast.items in
  let* structs = build structs_src in
  let validate_object span t = Sema_limits.validate_object limits structs span t in
  let source_obj span t =
    let* t = source_ty_diag named_types span t in
    validate_object span t
  in
  let source_return span t =
    let* t = source_ty_diag named_types span t in
    if t = Hir.Void then Ok t else validate_object span t
  in
  let source_params =
    map_params (fun (param : Ast.param) -> source_obj param.span param.ty)
  in
  let sigs = ref [] and declared_functions = ref String_set.empty in
  let* () =
    List.fold_left
      (fun r item ->
        let* () = r in
        match item with
        | Ast.Func { name; params; ret; variadic; linkage; span; generic_params; _ } ->
            let* () = validate_binding_name span name in
            if String_set.mem name !declared_functions then
              error span (Printf.sprintf "duplicate function `%s`" name)
            else if List.mem name arrays_names then
              error span (Printf.sprintf "duplicate declaration `%s`" name)
            else if name = "main" && generic_params <> [] then
              error span "entry point `main` cannot have generic parameters"
            else
              let () = declared_functions := String_set.add name !declared_functions in
              let* () = validate_function_params generic_params params in
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
                  map_params
                    (fun (param : Ast.param) -> source_obj param.span param.ty)
                    params
                in
                let* rt = source_return span ret in
                let* () =
                  if linkage = Ast.External_c then
                    validate_extern_c_signature span params ps rt
                  else Ok ()
                in
                sigs := (name, { params = ps; ret = rt; variadic }) :: !sigs;
                Ok ()
        | _ -> Ok ())
      (Ok ()) program.items
  in
  let sigs_ordered = List.rev !sigs in
  let templates =
    List.filter_map
      (fun item ->
        match item with
        | Ast.Func { name; generic_params = _ :: _; _ } -> Some (name, item)
        | _ -> None)
      program.items
  in
  let program_strings = create_string_pool limits in
  let funcs = ref [] in
  let hir_linkage = function
    | Ast.External_c -> Hir.External_c
    | Ast.Internal -> Hir.Internal
  in
  let make_context ~extra_consts ~spec_depth ~spec_trace ~ret_ty =
    {
      structs;
      named_types;
      consts =
        (if extra_consts = [] then consts_ordered else extra_consts @ consts_ordered);
      arrays = arrays_ordered;
      signatures = sigs_ordered;
      templates;
      top_level_bindings;
      specializations;
      spec_depth;
      spec_trace;
      flow = Sema_flow.create ~initial_scope:false structs;
      string_pool = program_strings;
      ret_ty;
      limits;
    }
  in
  let hir_params params =
    List.mapi (fun id (name, ty) -> ({ Hir.name; ty; id } : Hir.local)) params
  in
  let check_function_body ~name ~diagnostic_name ~description ~span ~params ~ret ~stmts
      ~linkage ~variadic ~extra_consts ~spec_depth ~spec_trace ~require_return =
    let context = make_context ~extra_consts ~spec_depth ~spec_trace ~ret_ty:ret in
    let* params =
      Result_list.map
        (fun (param_name, param_ty) ->
          let* binding = add_local param_name param_ty context span in
          mark_init binding context;
          Ok binding)
        params
    in
    let* body = Sema_check.check_block context stmts in
    let* () =
      if require_return && ret <> Hir.Void && (Hir.block_flow body).falls_through then
        error span
          (description ^ " `" ^ diagnostic_name
         ^ "` may reach the end without returning")
      else Ok ()
    in
    Ok
      ({ Hir.name; params; ret; body = Hir.Statements body; linkage; variadic }
        : Hir.func)
  in
  let add_func func = funcs := func :: !funcs in
  let check_func = function
    | Ast.Func { name; params; ret; body; linkage; variadic; generic_params = []; span }
      ->
        let trace = specialization_trace specializations Function_specialization name in
        let diagnostic_name =
          specialization_source_name specializations Function_specialization name
        in
        let spec_depth =
          match
            Sema_specialization.find_by_name specializations Function_specialization
              name
          with
          | Some specialization -> specialization.depth + 1
          | None -> 0
        in
        let description = if trace = [] then "function" else "specialized function" in
        let result =
          let* ret = source_return span ret in
          let* params = source_params params in
          let linkage = hir_linkage linkage in
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
              check_function_body ~name ~diagnostic_name ~description ~span ~params ~ret
                ~stmts ~linkage ~variadic ~extra_consts:[] ~spec_depth ~spec_trace:trace
                ~require_return:true
        in
        let* func = result |> trace_result specializations trace in
        add_func func;
        Ok ()
    | _ -> Ok ()
  in
  let* () = Result_list.iter check_func program.items in
  let rec materialize () =
    match Sema_specialization.take_pending specializations with
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
            let result =
              let* ret = source_ty_with_values named_types values span ret in
              let* ret = if ret = Hir.Void then Ok ret else validate_object span ret in
              let* params =
                Result_list.map
                  (fun (parameter : Ast.param) ->
                    let* ty =
                      source_ty_with_values named_types values parameter.span
                        parameter.ty
                    in
                    let* ty = validate_object parameter.span ty in
                    Ok (parameter.name, ty))
                  params
              in
              check_function_body ~name:sp.name
                ~diagnostic_name:
                  (specialization_source_name specializations Function_specialization
                     sp.name)
                ~description:"specialized function" ~span ~params ~ret ~stmts
                ~linkage:(hir_linkage linkage) ~variadic ~extra_consts:values
                ~spec_depth:(sp.depth + 1) ~spec_trace:sp.trace ~require_return:true
            in
            let* func = result |> trace_result specializations sp.trace in
            add_func func;
            materialize ()
        | Function_payload _ -> materialize ()
        | Struct_payload _ ->
            error Span.synthetic
              "internal error: struct specialization was queued too late")
  in
  let* () = materialize () in
  let* () =
    Sema_invariants.check_materialization
      ~pending:(Sema_specialization.pending_count specializations)
      ~functions:(List.map (fun (func : Hir.func) -> func.name) (List.rev !funcs))
    |> phase_invariant
  in
  let hconsts =
    List.map
      (fun (n, t, v) -> ({ Hir.name = n; ty = t; bits = v } : Hir.const_def))
      consts_ordered
  in
  let harrays =
    List.filter_map
      (fun (n, t, vs) ->
        match t with
        | Hir.Array _ -> Some ({ Hir.name = n; ty = t; elems = vs } : Hir.const_arr_def)
        | _ -> None)
      arrays_ordered
  in
  Ok
    ({
       Hir.structs;
       consts = hconsts;
       const_arrays = harrays;
       funcs = List.rev !funcs;
       strings = List.rev program_strings.reversed;
     }
      : Hir.program)
