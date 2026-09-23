open Sema_constants
open Sema_numeric
open Sema_specialization
open Sema_types
open Sema_context
module String_set = Set.Make (String)

let error span message = Error [ Diag.error span message ]
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let monomorphize_types ~check_expr ~check_stmt ~check_target ~target_ty
    ~generic_const_argument ?eval_context ?(eager_functions = false) ~top_level_bindings
    ~limits ~type_node_account specializations program =
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
    |> String_set.of_list
  in
  let function_names =
    List.filter_map
      (function Ast.Func { name; _ } -> Some name | _ -> None)
      program.Ast.items
    |> String_set.of_list
  in
  let global_value_names =
    List.filter_map
      (function Ast.Const { name; _ } -> Some name | _ -> None)
      program.Ast.items
    |> String_set.of_list
  in
  let named_type_names =
    List.filter_map
      (function
        | Ast.Opaque { name; _ } | Ast.Struct { name; _ } -> Some name | _ -> None)
      program.Ast.items
    |> String_set.of_list
  in
  let validation_signatures =
    List.filter_map
      (function
        | Ast.Func { name; generic_params = []; params; ret; variadic; span; _ } -> (
            let result =
              let* params =
                Result_list.map
                  (fun (parameter : Ast.param) ->
                    let* ty =
                      source_ty_with_values eval_named_types eval_consts parameter.span
                        parameter.ty
                    in
                    Ok (parameter.name, ty))
                  params
              in
              let* ret = source_ty_with_values eval_named_types eval_consts span ret in
              Ok (name, { params; ret; variadic })
            in
            match result with Ok signature -> Some signature | Error _ -> None)
        | _ -> None)
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
  let current_trace = ref [] in
  let nearest_kind value_names type_names name =
    if String_set.mem name value_names then Some (`Value None)
    else if String_set.mem name type_names then Some (`Type None)
    else
      match lookup_top_level name top_level_bindings with
      | Some { declaration_id; declaration_kind = Top_type; _ } ->
          Some (`Type (Some declaration_id))
      | Some { declaration_id; declaration_kind = Top_function; _ } ->
          Some (`Function declaration_id)
      | Some { declaration_id; declaration_kind = Top_const; _ } ->
          Some (`Value (Some declaration_id))
      | None when String_set.mem name named_type_names -> Some (`Type None)
      | None when String_set.mem name function_names -> Some (`Function (-1))
      | None when String_set.mem name global_value_names -> Some (`Value None)
      | None -> None
  in
  let rec type_mentions names = function
    | Ast.Array (length, ty) | Ast.Vec (length, ty) ->
        List.mem length names || type_mentions names ty
    | Ast.Ptr ty | Ast.Ptr_const ty -> type_mentions names ty
    | Ast.Applied_type (_, arguments, _) ->
        List.exists (generic_argument_mentions names) arguments
    | Ast.Named_type name -> List.mem name names
    | Ast.Bool | Ast.Void | Ast.Int _ -> false
  and generic_argument_mentions names = function
    | Ast.Type_arg ty -> type_mentions names ty
    | Ast.Const_arg expression -> expression_mentions names expression
    | Ast.Name_arg (name, _) -> List.mem name names
  and expression_mentions names = function
    | Ast.Ident (name, _) -> List.mem name names
    | Ast.Unary (_, expression, _)
    | Ast.Deref (expression, _)
    | Ast.Addr_of (expression, _)
    | Ast.Splat (expression, _) ->
        expression_mentions names expression
    | Ast.Binary (_, left, right, _)
    | Ast.Index (left, right, _)
    | Ast.Ptr_add (_, left, right, _) ->
        expression_mentions names left || expression_mentions names right
    | Ast.Call (callee, arguments, _) ->
        expression_mentions names callee
        || List.exists (expression_mentions names) arguments
    | Ast.Generic_args (callee, arguments, _) ->
        expression_mentions names callee
        || List.exists (generic_argument_mentions names) arguments
    | Ast.Cast (_, ty, expression, _) ->
        type_mentions names ty || expression_mentions names expression
    | Ast.Sizeof (ty, _) | Ast.Alignof (ty, _) | Ast.Offsetof (ty, _, _) ->
        type_mentions names ty
    | Ast.Field (expression, _, _) -> expression_mentions names expression
    | Ast.Ternary (condition, yes, no, _) ->
        expression_mentions names condition
        || expression_mentions names yes || expression_mentions names no
    | Ast.Array_lit (elements, _) -> List.exists (expression_mentions names) elements
    | Ast.Struct_lit (ty, elements, _) ->
        type_mentions names ty || List.exists (expression_mentions names) elements
    | Ast.Int_lit _ | Ast.Bool_lit _ | Ast.Null _ | Ast.String_lit _ -> false
  in
  let rec validate_type_names value_names type_names span = function
    | Ast.Ptr ty | Ast.Ptr_const ty ->
        validate_type_names value_names type_names span ty
    | Ast.Array (length, ty) | Ast.Vec (length, ty) ->
        let* () =
          match parse_integer length with
          | Ok _ -> Ok ()
          | Error _ ->
              if String_set.mem length value_names then Ok ()
              else error span (Printf.sprintf "unknown name `%s`" length)
        in
        validate_type_names value_names type_names span ty
    | Ast.Named_type name -> (
        match nearest_kind value_names type_names name with
        | Some (`Type _) -> Ok ()
        | Some (`Value _) ->
            error span (Printf.sprintf "`%s` is a value, not a type" name)
        | Some (`Function _) ->
            error span (Printf.sprintf "`%s` is a function, not a type" name)
        | None -> error span (Printf.sprintf "unknown type `%s`" name))
    | Ast.Applied_type (name, arguments, application_span) ->
        if not (String_set.mem name struct_names) then
          error span (Printf.sprintf "unknown generic struct `%s`" name)
        else
          Result_list.iter
            (validate_generic_argument_names value_names type_names application_span)
            arguments
    | Ast.Bool | Ast.Void | Ast.Int _ -> Ok ()
  and validate_generic_argument_names value_names type_names fallback_span = function
    | Ast.Type_arg ty -> validate_type_names value_names type_names fallback_span ty
    | Ast.Const_arg expression ->
        validate_expression_names value_names type_names expression
    | Ast.Name_arg (name, span) ->
        if
          String_set.mem name value_names
          || String_set.mem name global_value_names
          || String_set.mem name named_type_names
          || String_set.mem name type_names
        then Ok ()
        else error span (Printf.sprintf "unknown name `%s`" name)
  and validate_expression_names value_names type_names = function
    | Ast.Ident (name, span) -> (
        match nearest_kind value_names type_names name with
        | Some (`Value _) -> Ok ()
        | Some (`Type _) ->
            error span (Printf.sprintf "`%s` is a type, not a value" name)
        | Some (`Function _) ->
            error span (Printf.sprintf "`%s` is a function, not a value" name)
        | None -> error span (Printf.sprintf "unknown name `%s`" name))
    | Ast.Unary (_, expression, _)
    | Ast.Deref (expression, _)
    | Ast.Addr_of (expression, _)
    | Ast.Splat (expression, _) ->
        validate_expression_names value_names type_names expression
    | Ast.Binary (_, left, right, _)
    | Ast.Index (left, right, _)
    | Ast.Ptr_add (_, left, right, _) ->
        let* () = validate_expression_names value_names type_names left in
        validate_expression_names value_names type_names right
    | Ast.Call (Ast.Ident (name, span), arguments, _) ->
        let* () =
          if Names.reserved_binding_name name then Ok ()
          else
            match nearest_kind value_names type_names name with
            | Some (`Function _) -> Ok ()
            | Some (`Value _) ->
                error span (Printf.sprintf "`%s` is a value, not a function" name)
            | Some (`Type _) ->
                error span (Printf.sprintf "`%s` is a type, not a function" name)
            | None -> error span (Printf.sprintf "unknown function `%s`" name)
        in
        Result_list.iter (validate_expression_names value_names type_names) arguments
    | Ast.Call (callee, arguments, _) ->
        let* () = validate_expression_names value_names type_names callee in
        Result_list.iter (validate_expression_names value_names type_names) arguments
    | Ast.Generic_args (Ast.Ident (name, span), arguments, application_span) ->
        let* () =
          match nearest_kind value_names type_names name with
          | Some (`Function _) -> Ok ()
          | Some (`Value _) ->
              error span (Printf.sprintf "`%s` is a value, not a function" name)
          | Some (`Type _) ->
              error span (Printf.sprintf "`%s` is a type, not a function" name)
          | None -> error span (Printf.sprintf "unknown generic function `%s`" name)
        in
        let* () =
          match List.assoc_opt name function_templates with
          | None -> error span (Printf.sprintf "function `%s` is not generic" name)
          | Some (Ast.Func { generic_params; _ }) ->
              if List.length arguments <> List.length generic_params then
                error application_span
                  (Printf.sprintf "wrong number of generic arguments to `%s`" name)
              else
                Result_list.iter
                  (fun (parameter, argument) ->
                    match (parameter, argument) with
                    | Ast.Type_param _, Ast.Type_arg _ -> Ok ()
                    | Ast.Type_param _, Ast.Name_arg (argument_name, argument_span) -> (
                        match nearest_kind value_names type_names argument_name with
                        | Some (`Type _) -> Ok ()
                        | Some _ -> error argument_span "expected a type argument"
                        | None -> Ok ())
                    | Ast.Const_param _, Ast.Const_arg _ -> Ok ()
                    | Ast.Const_param _, Ast.Name_arg (argument_name, argument_span)
                      -> (
                        match nearest_kind value_names type_names argument_name with
                        | Some (`Value _) -> Ok ()
                        | Some _ -> error argument_span "expected a const argument"
                        | None -> Ok ())
                    | Ast.Const_param _, Ast.Type_arg (Ast.Applied_type _) -> Ok ()
                    | Ast.Type_param _, _ ->
                        error application_span "expected a type argument"
                    | Ast.Const_param _, _ ->
                        error application_span "expected a const argument")
                  (List.combine generic_params arguments)
          | Some _ -> error span (Printf.sprintf "`%s` is not a function" name)
        in
        Result_list.iter
          (validate_generic_argument_names value_names type_names application_span)
          arguments
    | Ast.Generic_args (callee, arguments, application_span) ->
        let* () = validate_expression_names value_names type_names callee in
        Result_list.iter
          (validate_generic_argument_names value_names type_names application_span)
          arguments
    | Ast.Cast (_, ty, expression, span) ->
        let* () = validate_type_names value_names type_names span ty in
        validate_expression_names value_names type_names expression
    | Ast.Sizeof (ty, span) | Ast.Alignof (ty, span) | Ast.Offsetof (ty, _, span) ->
        validate_type_names value_names type_names span ty
    | Ast.Field (expression, _, _) ->
        validate_expression_names value_names type_names expression
    | Ast.Ternary (condition, yes, no, _) ->
        let* () = validate_expression_names value_names type_names condition in
        let* () = validate_expression_names value_names type_names yes in
        validate_expression_names value_names type_names no
    | Ast.Array_lit (elements, _) ->
        Result_list.iter (validate_expression_names value_names type_names) elements
    | Ast.Struct_lit (ty, elements, span) ->
        let* () = validate_type_names value_names type_names span ty in
        Result_list.iter (validate_expression_names value_names type_names) elements
    | Ast.Int_lit _ | Ast.Bool_lit _ | Ast.Null _ | Ast.String_lit _ -> Ok ()
  in
  let rec validate_target_names value_names type_names = function
    | Ast.Target_ident (name, span) -> (
        match nearest_kind value_names type_names name with
        | Some (`Value _) when String_set.mem name value_names -> Ok ()
        | Some (`Value _) ->
            error span (Printf.sprintf "constant `%s` is not assignable" name)
        | Some (`Type _) ->
            error span (Printf.sprintf "type `%s` is not assignable" name)
        | Some (`Function _) ->
            error span (Printf.sprintf "function `%s` is not assignable" name)
        | None -> error span (Printf.sprintf "unknown assignment target `%s`" name))
    | Ast.Target_deref expression ->
        validate_expression_names value_names type_names expression
    | Ast.Target_index (base, index) ->
        let* () = validate_expression_names value_names type_names base in
        validate_expression_names value_names type_names index
    | Ast.Target_field (base, _) ->
        validate_expression_names value_names type_names base
  and validate_statement_names value_names type_names scope_names = function
    | Ast.Let { name; ty; init; span; _ } ->
        let* () = validate_binding_name span name in
        if String_set.mem name scope_names then
          error span (Printf.sprintf "duplicate local `%s`" name)
        else
          let* () = validate_type_names value_names type_names span ty in
          let* () =
            match init with
            | None -> Ok ()
            | Some expression ->
                validate_expression_names value_names type_names expression
          in
          Ok (String_set.add name value_names, String_set.add name scope_names)
    | Ast.Assign (target, expression, _) | Ast.Compound_assign (target, _, expression, _)
      ->
        let* () = validate_target_names value_names type_names target in
        let* () = validate_expression_names value_names type_names expression in
        Ok (value_names, scope_names)
    | Ast.Return (expression, _) ->
        let* () =
          match expression with
          | None -> Ok ()
          | Some expression ->
              validate_expression_names value_names type_names expression
        in
        Ok (value_names, scope_names)
    | Ast.If (condition, yes, no, _) ->
        let* () = validate_expression_names value_names type_names condition in
        let* () = validate_statement_block_names value_names type_names yes in
        let* () =
          match no with
          | None -> Ok ()
          | Some statements ->
              validate_statement_block_names value_names type_names statements
        in
        Ok (value_names, scope_names)
    | Ast.While (condition, body, _) ->
        let* () = validate_expression_names value_names type_names condition in
        let* () = validate_statement_block_names value_names type_names body in
        Ok (value_names, scope_names)
    | Ast.Defer (body, _) | Ast.Block (body, _) ->
        let* () = validate_statement_block_names value_names type_names body in
        Ok (value_names, scope_names)
    | Ast.Expr_stmt (expression, _) ->
        let* () = validate_expression_names value_names type_names expression in
        Ok (value_names, scope_names)
    | Ast.For (init, condition, step, body, _) ->
        let* loop_names, loop_scope_names =
          match init with
          | None -> Ok (value_names, String_set.empty)
          | Some statement ->
              validate_statement_names value_names type_names String_set.empty statement
        in
        let* () =
          match condition with
          | None -> Ok ()
          | Some expression ->
              validate_expression_names loop_names type_names expression
        in
        let* () = validate_statement_block_names loop_names type_names body in
        let* _ =
          match step with
          | None -> Ok (loop_names, loop_scope_names)
          | Some statement ->
              validate_statement_names loop_names type_names loop_scope_names statement
        in
        Ok (value_names, scope_names)
    | Ast.Switch (expression, cases, default, _) ->
        let* () = validate_expression_names value_names type_names expression in
        let* () =
          Result_list.iter
            (fun (value, body) ->
              let* () = validate_expression_names value_names type_names value in
              validate_statement_block_names value_names type_names body)
            cases
        in
        let* () =
          match default with
          | None -> Ok ()
          | Some body -> validate_statement_block_names value_names type_names body
        in
        Ok (value_names, scope_names)
    | Ast.Break _ | Ast.Continue _ -> Ok (value_names, scope_names)
  and validate_statement_block_names ?(scope_names = String_set.empty) value_names
      type_names = function
    | [] -> Ok ()
    | statement :: rest ->
        let* value_names, scope_names =
          validate_statement_names value_names type_names scope_names statement
        in
        validate_statement_block_names ~scope_names value_names type_names rest
  in
  let rec validate_statement_duplicates scope_names = function
    | Ast.Let { name; span; _ } ->
        if String_set.mem name scope_names then
          error span (Printf.sprintf "duplicate local `%s`" name)
        else Ok (String_set.add name scope_names)
    | Ast.If (_, yes, no, _) ->
        let* () = validate_statement_block_duplicates yes in
        let* () =
          match no with
          | None -> Ok ()
          | Some statements -> validate_statement_block_duplicates statements
        in
        Ok scope_names
    | Ast.While (_, body, _) | Ast.Defer (body, _) | Ast.Block (body, _) ->
        let* () = validate_statement_block_duplicates body in
        Ok scope_names
    | Ast.For (init, _, step, body, _) ->
        let* loop_scope_names =
          match init with
          | None -> Ok String_set.empty
          | Some statement -> validate_statement_duplicates String_set.empty statement
        in
        let* () = validate_statement_block_duplicates body in
        let* _ =
          match step with
          | None -> Ok loop_scope_names
          | Some statement -> validate_statement_duplicates loop_scope_names statement
        in
        Ok scope_names
    | Ast.Switch (_, cases, default, _) ->
        let* () =
          Result_list.iter
            (fun (_, body) -> validate_statement_block_duplicates body)
            cases
        in
        let* () =
          match default with
          | None -> Ok ()
          | Some body -> validate_statement_block_duplicates body
        in
        Ok scope_names
    | Ast.Assign _ | Ast.Compound_assign _ | Ast.Return _ | Ast.Expr_stmt _
    | Ast.Break _ | Ast.Continue _ ->
        Ok scope_names
  and validate_statement_block_duplicates ?(scope_names = String_set.empty) = function
    | [] -> Ok ()
    | statement :: rest ->
        let* scope_names = validate_statement_duplicates scope_names statement in
        validate_statement_block_duplicates ~scope_names rest
  in
  let make_legality_context ret_ty =
    {
      structs = eval_structs;
      named_types = eval_named_types;
      consts = eval_consts;
      arrays = eval_arrays;
      signatures = validation_signatures;
      templates = function_templates;
      top_level_bindings;
      specializations;
      spec_depth = 0;
      spec_trace = [];
      flow = Sema_flow.create ~initial_scope:true eval_structs;
      string_pool = create_string_pool limits;
      ret_ty;
      limits;
    }
  in
  let rec has_generic_arguments = function
    | Ast.Generic_args _ -> true
    | Ast.Unary (_, expression, _)
    | Ast.Deref (expression, _)
    | Ast.Addr_of (expression, _)
    | Ast.Splat (expression, _)
    | Ast.Field (expression, _, _) ->
        has_generic_arguments expression
    | Ast.Binary (_, left, right, _)
    | Ast.Index (left, right, _)
    | Ast.Ptr_add (_, left, right, _) ->
        has_generic_arguments left || has_generic_arguments right
    | Ast.Call (callee, arguments, _) ->
        has_generic_arguments callee || List.exists has_generic_arguments arguments
    | Ast.Cast (_, _, expression, _) -> has_generic_arguments expression
    | Ast.Ternary (condition, yes, no, _) ->
        has_generic_arguments condition
        || has_generic_arguments yes || has_generic_arguments no
    | Ast.Array_lit (values, _) | Ast.Struct_lit (_, values, _) ->
        List.exists has_generic_arguments values
    | Ast.Sizeof _ | Ast.Alignof _ | Ast.Offsetof _ | Ast.Ident _ | Ast.Int_lit _
    | Ast.Bool_lit _ | Ast.Null _ | Ast.String_lit _ ->
        false
  in
  let rec substitute_validation_type substitutions = function
    | Ast.Named_type name as ty ->
        Option.value ~default:ty (List.assoc_opt name substitutions)
    | Ast.Ptr ty -> Ast.Ptr (substitute_validation_type substitutions ty)
    | Ast.Ptr_const ty -> Ast.Ptr_const (substitute_validation_type substitutions ty)
    | Ast.Array (length, ty) ->
        Ast.Array (length, substitute_validation_type substitutions ty)
    | Ast.Vec (length, ty) ->
        Ast.Vec (length, substitute_validation_type substitutions ty)
    | Ast.Applied_type (name, arguments, span) ->
        Ast.Applied_type
          ( name,
            List.map
              (function
                | Ast.Type_arg ty ->
                    Ast.Type_arg (substitute_validation_type substitutions ty)
                | argument -> argument)
              arguments,
            span )
    | (Ast.Bool | Ast.Void | Ast.Int _) as ty -> ty
  in
  let rec validate_non_dependent_expression c dependent expected expression =
    if
      (not (expression_mentions dependent expression))
      && not (has_generic_arguments expression)
    then
      let* checked = check_expr c expected expression in
      match expected with
      | None -> Ok ()
      | Some expected ->
          ensure_expected (Hir.expr_ty checked) expected (Ast.expr_span expression)
    else
      match expression with
      | Ast.Unary (_, value, _)
      | Ast.Deref (value, _)
      | Ast.Addr_of (value, _)
      | Ast.Splat (value, _) ->
          validate_non_dependent_expression c dependent None value
      | Ast.Binary (_, left, right, _)
      | Ast.Index (left, right, _)
      | Ast.Ptr_add (_, left, right, _) ->
          let* () = validate_non_dependent_expression c dependent None left in
          validate_non_dependent_expression c dependent None right
      | Ast.Call (Ast.Ident (name, _), arguments, span) -> (
          match List.assoc_opt name validation_signatures with
          | Some signature -> (
              if
                (not signature.variadic)
                && List.length arguments <> List.length signature.params
                || signature.variadic
                   && List.length arguments < List.length signature.params
              then error span (Printf.sprintf "wrong number of arguments to `%s`" name)
              else
                let rec validate_arguments formals arguments =
                  match (formals, arguments) with
                  | [], trailing ->
                      Result_list.iter
                        (validate_non_dependent_expression c dependent None)
                        trailing
                  | (_, formal) :: formals, argument :: arguments ->
                      let* () =
                        validate_non_dependent_expression c dependent (Some formal)
                          argument
                      in
                      validate_arguments formals arguments
                  | _ -> error span "wrong number of arguments"
                in
                let* () = validate_arguments signature.params arguments in
                match expected with
                | None -> Ok ()
                | Some expected -> ensure_expected signature.ret expected span)
          | None ->
              Result_list.iter
                (validate_non_dependent_expression c dependent None)
                arguments)
      | Ast.Call
          (Ast.Generic_args (Ast.Ident (name, _), generic_arguments, _), arguments, span)
        -> (
          match List.assoc_opt name function_templates with
          | Some (Ast.Func { generic_params; params; ret; _ }) -> (
              let rec resolve_arguments substitutions values parameters arguments =
                match (parameters, arguments) with
                | [], [] -> Ok (Some (substitutions, values))
                | Ast.Type_param { name; _ } :: parameters, argument :: arguments -> (
                    match argument with
                    | Ast.Type_arg ty when not (type_mentions dependent ty) ->
                        resolve_arguments ((name, ty) :: substitutions) values
                          parameters arguments
                    | Ast.Name_arg (argument_name, _)
                      when not (List.mem argument_name dependent) ->
                        resolve_arguments
                          ((name, Ast.Named_type argument_name) :: substitutions)
                          values parameters arguments
                    | _ -> Ok None)
                | Ast.Const_param parameter :: parameters, argument :: arguments ->
                    let* expression = generic_const_argument span argument in
                    if expression_mentions dependent expression then Ok None
                    else
                      let substituted =
                        substitute_validation_type substitutions parameter.ty
                      in
                      let* () =
                        charge_expanded_type type_node_account ~span:parameter.span
                          ~where:(Printf.sprintf "declaration `%s`" name)
                          substituted
                      in
                      let* ty =
                        source_ty_with_values eval_named_types eval_consts
                          parameter.span substituted
                      in
                      let* actual_ty, value =
                        const_expr ~structs:eval_structs ~named_types:eval_named_types
                          ~arrays:eval_arrays eval_consts (Some ty) expression
                      in
                      let* () =
                        ensure_expected actual_ty ty (Ast.expr_span expression)
                      in
                      resolve_arguments substitutions
                        ((parameter.name, ty, value) :: values)
                        parameters arguments
                | _ -> Ok None
              in
              let* resolved =
                resolve_arguments [] [] generic_params generic_arguments
              in
              match resolved with
              | None ->
                  Result_list.iter
                    (validate_non_dependent_expression c dependent None)
                    arguments
              | Some (substitutions, values) -> (
                  let* formals =
                    Result_list.map
                      (fun (parameter : Ast.param) ->
                        let substituted =
                          substitute_validation_type substitutions parameter.ty
                        in
                        let* () =
                          charge_expanded_type type_node_account ~span:parameter.span
                            ~where:(Printf.sprintf "declaration `%s`" name)
                            substituted
                        in
                        source_ty_with_values eval_named_types values parameter.span
                          substituted)
                      params
                  in
                  if List.length formals <> List.length arguments then
                    error span (Printf.sprintf "wrong number of arguments to `%s`" name)
                  else
                    let* () =
                      Result_list.iter
                        (fun (formal, argument) ->
                          validate_non_dependent_expression c dependent (Some formal)
                            argument)
                        (List.combine formals arguments)
                    in
                    let* return_ty =
                      let substituted = substitute_validation_type substitutions ret in
                      let* () =
                        charge_expanded_type type_node_account ~span
                          ~where:(Printf.sprintf "declaration `%s`" name)
                          substituted
                      in
                      source_ty_with_values eval_named_types values span substituted
                    in
                    match expected with
                    | None -> Ok ()
                    | Some expected -> ensure_expected return_ty expected span))
          | _ ->
              Result_list.iter
                (validate_non_dependent_expression c dependent None)
                arguments)
      | Ast.Call (callee, arguments, _) ->
          let* () = validate_non_dependent_expression c dependent None callee in
          Result_list.iter
            (validate_non_dependent_expression c dependent None)
            arguments
      | Ast.Generic_args (Ast.Ident _, arguments, _) ->
          Result_list.iter
            (function
              | Ast.Const_arg value ->
                  validate_non_dependent_expression c dependent None value
              | Ast.Type_arg _ | Ast.Name_arg _ -> Ok ())
            arguments
      | Ast.Generic_args (_, arguments, _) ->
          Result_list.iter
            (function
              | Ast.Const_arg value ->
                  validate_non_dependent_expression c dependent None value
              | Ast.Type_arg _ | Ast.Name_arg _ -> Ok ())
            arguments
      | Ast.Cast (kind, destination, value, span) ->
          let* () =
            if type_mentions dependent destination then Ok ()
            else
              let* destination =
                source_ty_with_values eval_named_types eval_consts span destination
              in
              let valid =
                match (kind, destination) with
                | (Ast.Zext | Ast.Sext), (Hir.Int _ | Hir.Vec (_, Hir.Int _)) -> true
                | Ast.Trunc, (Hir.Bool | Hir.Int _ | Hir.Vec (_, (Hir.Bool | Hir.Int _)))
                  ->
                    true
                | ( Ast.Bitcast,
                    ( Hir.Bool | Hir.Int _ | Hir.Ptr _ | Hir.ConstPtr _
                    | Hir.Vec (_, (Hir.Bool | Hir.Int _)) ) ) ->
                    true
                | _ -> false
              in
              if valid then Ok () else error span "illegal cast target type"
          in
          validate_non_dependent_expression c dependent None value
      | Ast.Field (value, _, _) ->
          validate_non_dependent_expression c dependent None value
      | Ast.Ternary (condition, yes, no, _) ->
          let* () = validate_non_dependent_expression c dependent None condition in
          let* () = validate_non_dependent_expression c dependent expected yes in
          validate_non_dependent_expression c dependent expected no
      | Ast.Array_lit (values, _) | Ast.Struct_lit (_, values, _) ->
          Result_list.iter (validate_non_dependent_expression c dependent None) values
      | Ast.Sizeof _ | Ast.Alignof _ | Ast.Offsetof _ | Ast.Ident _ | Ast.Int_lit _
      | Ast.Bool_lit _ | Ast.Null _ | Ast.String_lit _ ->
          Ok ()
  in
  let validate_non_dependent_condition c dependent label expression =
    if expression_mentions dependent expression then
      validate_non_dependent_expression c dependent None expression
    else
      let* checked = check_expr c None expression in
      if Hir.expr_ty checked = Hir.Bool then Ok ()
      else error (Ast.expr_span expression) (label ^ " condition must be bool")
  in
  let target_mentions names = function
    | Ast.Target_ident (name, _) -> List.mem name names
    | Ast.Target_deref expression | Ast.Target_field (expression, _) ->
        expression_mentions names expression
    | Ast.Target_index (base, index) ->
        expression_mentions names base || expression_mentions names index
  in
  let rec validate_non_dependent_statements c dependent expected_return statements =
    match statements with
    | [] -> Ok dependent
    | statement :: rest ->
        let* dependent =
          validate_non_dependent_statement c dependent expected_return statement
        in
        validate_non_dependent_statements c dependent expected_return rest
  and validate_non_dependent_block c dependent expected_return statements =
    push c;
    let result =
      validate_non_dependent_statements c dependent expected_return statements
    in
    pop c;
    let* _ = result in
    Ok ()
  and validate_non_dependent_statement c dependent expected_return = function
    | Ast.Let { name; ty; init; span; raw; _ } ->
        if type_mentions dependent ty then Ok (name :: dependent)
        else
          let* ty = source_ty_with_values eval_named_types eval_consts span ty in
          let* () =
            match init with
            | None -> Ok ()
            | Some value ->
                validate_non_dependent_expression c dependent (Some ty) value
          in
          let* binding = add_local name ty c span in
          if raw || Option.is_some init then mark_init binding c;
          Ok (List.filter (fun dependent_name -> dependent_name <> name) dependent)
    | (Ast.Assign (target, value, _) | Ast.Compound_assign (target, _, value, _)) as
      statement ->
        if target_mentions dependent target then
          let* () = validate_non_dependent_expression c dependent None value in
          Ok dependent
        else if expression_mentions dependent value || has_generic_arguments value then
          let* checked_target = check_target c target in
          let* expected =
            match target_ty c checked_target.target with
            | Some ty -> Ok ty
            | None -> error (Ast.expr_span value) "assignment target has no type"
          in
          let* () =
            validate_non_dependent_expression c dependent (Some expected) value
          in
          Ok dependent
        else
          let* () =
            let* _ = check_stmt c statement in
            Ok ()
          in
          Ok dependent
    | Ast.Return (value, _) ->
        let* () =
          match value with
          | None -> Ok ()
          | Some value ->
              validate_non_dependent_expression c dependent expected_return value
        in
        Ok dependent
    | Ast.If (condition, yes, no, _) ->
        let* () = validate_non_dependent_condition c dependent "if" condition in
        let* () = validate_non_dependent_block c dependent expected_return yes in
        let* () =
          match no with
          | None -> Ok ()
          | Some no -> validate_non_dependent_block c dependent expected_return no
        in
        Ok dependent
    | Ast.While (condition, body, _) ->
        let* () = validate_non_dependent_condition c dependent "while" condition in
        let* () = validate_non_dependent_block c dependent expected_return body in
        Ok dependent
    | Ast.Defer (body, _) | Ast.Block (body, _) ->
        let* () = validate_non_dependent_block c dependent expected_return body in
        Ok dependent
    | Ast.Expr_stmt (expression, _) ->
        let* () = validate_non_dependent_expression c dependent None expression in
        Ok dependent
    | Ast.For (init, condition, step, body, _) ->
        push c;
        let result =
          let* loop_dependent =
            match init with
            | None -> Ok dependent
            | Some init ->
                validate_non_dependent_statement c dependent expected_return init
          in
          let* () =
            match condition with
            | None -> Ok ()
            | Some condition ->
                validate_non_dependent_condition c loop_dependent "for" condition
          in
          let* () =
            validate_non_dependent_block c loop_dependent expected_return body
          in
          match step with
          | None -> Ok loop_dependent
          | Some step ->
              validate_non_dependent_statement c loop_dependent expected_return step
        in
        pop c;
        let* _ = result in
        Ok dependent
    | Ast.Switch (expression, cases, default, _) ->
        let* () = validate_non_dependent_expression c dependent None expression in
        let* () =
          Result_list.iter
            (fun (value, body) ->
              let* () = validate_non_dependent_expression c dependent None value in
              validate_non_dependent_block c dependent expected_return body)
            cases
        in
        let* () =
          match default with
          | None -> Ok ()
          | Some body -> validate_non_dependent_block c dependent expected_return body
        in
        Ok dependent
    | Ast.Break _ | Ast.Continue _ -> Ok dependent
  in
  let validate_generic_legality ~span ~ret ~params ~generic_params statements =
    let generic_value_names =
      List.map
        (fun (parameter : Ast.const_param) -> parameter.name)
        (const_params generic_params)
    in
    let dependent = type_param_names generic_params @ generic_value_names in
    let expected_return =
      if type_mentions dependent ret then None
      else
        match source_ty_with_values eval_named_types eval_consts span ret with
        | Ok ty -> Some ty
        | Error _ -> None
    in
    let context =
      make_legality_context (Option.value ~default:Hir.Void expected_return)
    in
    let rec add_parameters dependent = function
      | [] -> Ok dependent
      | (parameter : Ast.param) :: rest ->
          if type_mentions dependent parameter.ty then
            add_parameters (parameter.name :: dependent) rest
          else
            let* ty =
              source_ty_with_values eval_named_types eval_consts parameter.span
                parameter.ty
            in
            let* binding = add_local parameter.name ty context parameter.span in
            mark_init binding context;
            add_parameters
              (List.filter (fun name -> name <> parameter.name) dependent)
              rest
    in
    let* dependent = add_parameters dependent params in
    let* _ =
      validate_non_dependent_statements context dependent expected_return statements
    in
    Ok ()
  in
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
            else if
              String_set.mem name named_type_names || List.mem name !generic_type_names
            then Ok (Ast.Named_type name)
            else error span (Printf.sprintf "unknown type `%s`" name))
    | Ast.Applied_type (name, arguments, application_span) -> (
        match List.assoc_opt name struct_templates with
        | None ->
            if String_set.mem name struct_names then
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
              Ok (Ast.Applied_type (name, arguments, application_span))
            else
              let rec resolve_arguments resolved diagnostic types bindings values_out
                  source_arguments params arguments =
                match (params, arguments) with
                | [], [] ->
                    Ok
                      ( List.rev resolved,
                        List.rev diagnostic,
                        List.rev types,
                        List.rev bindings,
                        List.rev values_out,
                        List.rev source_arguments )
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
                    let diagnostic_argument =
                      Diagnostic_type_argument
                        (diagnostic_type_of_ast specializations argument)
                    in
                    resolve_arguments
                      (Type_specialization_arg (specialization_type_key argument)
                      :: resolved)
                      (diagnostic_argument :: diagnostic)
                      (argument :: types)
                      ((parameter, argument) :: bindings)
                      values_out
                      (Ast.Type_arg argument :: source_arguments)
                      params arguments
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
                        (Diagnostic_const_argument (const_ty, value) :: diagnostic)
                        types bindings
                        ((parameter.name, const_ty, value) :: values_out)
                        (Ast.Const_arg expression :: source_arguments)
                        params arguments
                | _ -> error span "generic argument arity mismatch"
              in
              let* ( ordered_arguments,
                     diagnostic_arguments,
                     type_arguments,
                     substitutions,
                     values,
                     source_arguments ) =
                resolve_arguments [] [] [] [] [] [] generic_params arguments
              in
              if List.exists has_unresolved_application type_arguments then
                Ok (Ast.Applied_type (name, source_arguments, application_span))
              else
                let specialization_name =
                  if values = [] then mangle_type_specialization name type_arguments
                  else mangle_mixed_specialization name ordered_arguments
                in
                let* declaration_id =
                  specialization_declaration_id top_level_bindings Top_type span name
                in
                let key = (Struct_specialization, declaration_id, ordered_arguments) in
                let frame =
                  {
                    template_name = name;
                    arguments = diagnostic_arguments;
                    application_span;
                  }
                in
                let specialization =
                  {
                    key;
                    name = specialization_name;
                    depth;
                    payload =
                      Struct_payload
                        { template = Ast.Struct template; substitutions; values };
                    trace = !current_trace @ [ frame ];
                    pending_frame = None;
                  }
                in
                let* specialization =
                  Sema_specialization.request specializations ~limits ~depth ~span
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
            if String_set.mem name function_names then
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
              let rec resolve_arguments types bindings consts staged diagnostic resolved
                  params arguments =
                match (params, arguments) with
                | [], [] ->
                    Ok
                      ( List.rev types,
                        List.rev bindings,
                        List.rev consts,
                        List.rev staged,
                        List.rev diagnostic,
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
                      (Pending_diagnostic_type
                         (diagnostic_type_of_ast specializations argument)
                      :: diagnostic)
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
                      (Pending_diagnostic_const parameter :: diagnostic)
                      (argument :: resolved) params arguments
                | _ -> error span "generic argument arity mismatch"
              in
              let* ( type_arguments,
                     type_substitutions,
                     const_arguments,
                     staged_args,
                     diagnostic_args,
                     resolved_arguments ) =
                resolve_arguments [] [] [] [] [] [] generic_params arguments
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
                    let* specialization_name, key =
                      match staged with
                      | None ->
                          let* declaration_id =
                            specialization_declaration_id top_level_bindings
                              Top_function span name
                          in
                          Ok
                            ( mangle_specialization name concrete_values,
                              function_specialization_key declaration_id concrete_values
                            )
                      | Some (origin_id, origin_name, arguments, _, _) ->
                          Ok
                            ( mangle_mixed_specialization origin_name arguments,
                              (Function_specialization, origin_id, arguments) )
                    in
                    let frame_name, frame_arguments, frame_span =
                      match staged with
                      | None ->
                          ( name,
                            List.map
                              (fun (_, ty, value) ->
                                Diagnostic_const_argument (ty, value))
                              concrete_values,
                            span )
                      | Some (_, origin_name, _, arguments, application_span) ->
                          (origin_name, arguments, application_span)
                    in
                    let frame =
                      {
                        template_name = frame_name;
                        arguments = frame_arguments;
                        application_span = frame_span;
                      }
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
                        trace = !current_trace @ [ frame ];
                        pending_frame = None;
                      }
                    in
                    let* _ =
                      Sema_specialization.request specializations ~limits ~depth ~span
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
                let* declaration_id =
                  specialization_declaration_id top_level_bindings Top_function span
                    name
                in
                let key =
                  ( Function_specialization,
                    declaration_id,
                    List.map
                      (fun ty -> Type_specialization_arg (specialization_type_key ty))
                      type_arguments )
                in
                let pending_frame =
                  if const_arguments = [] then None
                  else
                    Some
                      {
                        pending_arguments = diagnostic_args;
                        pending_application_span = span;
                      }
                in
                let trace =
                  if const_arguments <> [] then !current_trace
                  else
                    let arguments =
                      List.map
                        (function
                          | Pending_diagnostic_type ty -> Diagnostic_type_argument ty
                          | Pending_diagnostic_const _ -> assert false)
                        diagnostic_args
                    in
                    !current_trace
                    @ [ { template_name = name; arguments; application_span = span } ]
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
                    trace;
                    pending_frame;
                  }
                in
                let* specialization =
                  Sema_specialization.request specializations ~limits ~depth ~span
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
  and resolve_stmt ?(values = []) ?(shadowed_constants = [])
      ?(defer_const_structs = false) substitutions depth = function
    | Ast.Let { name; ty; init; raw; span } ->
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
        Ok (Ast.Let { name; ty; init; raw; span })
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
        let unresolved_condition = condition in
        let* condition =
          resolve_expr ~values ~defer_const_structs substitutions depth condition
        in
        let resolve =
          resolve_stmt ~values ~shadowed_constants ~defer_const_structs substitutions
            depth
        in
        let specialization_values =
          List.filter
            (fun (name, _, _) -> not (List.mem name shadowed_constants))
            values
        in
        let constant_environment =
          List.filter
            (fun (name, _, _) -> not (List.mem name shadowed_constants))
            (values @ eval_consts)
        in
        let specialization_names =
          List.map (fun (name, _, _) -> name) specialization_values
        in
        let known_condition =
          if not (expression_mentions specialization_names unresolved_condition) then
            None
          else
            match
              const_expr ~structs:eval_structs ~named_types:eval_named_types
                ~arrays:eval_arrays constant_environment None condition
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
            Ok (Ast.Block (no, span))
        | Some true ->
            let* yes = Result_list.map resolve yes in
            Ok (Ast.Block (yes, span))
        | None ->
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
            (resolve_stmt ~values ~shadowed_constants ~defer_const_structs substitutions
               depth)
            body
        in
        Ok (Ast.While (condition, body, span))
    | (Ast.Break _ | Ast.Continue _) as statement -> Ok statement
    | Ast.Defer (body, span) ->
        let* body =
          Result_list.map
            (resolve_stmt ~values ~shadowed_constants ~defer_const_structs substitutions
               depth)
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
            (resolve_stmt ~values ~shadowed_constants ~defer_const_structs substitutions
               depth)
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
          resolve_stmt ~values ~shadowed_constants ~defer_const_structs substitutions
            depth
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
          resolve_stmt ~values ~shadowed_constants ~defer_const_structs substitutions
            depth
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
            let body_type_names =
              type_param_names generic_params |> String_set.of_list
            in
            let body_value_names =
              List.map (fun (parameter : Ast.param) -> parameter.name) params
              @ List.map
                  (fun (parameter : Ast.const_param) -> parameter.name)
                  (const_params generic_params)
              |> String_set.of_list
            in
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
                  let* () =
                    validate_statement_block_names
                      ~scope_names:(String_set.union body_value_names body_type_names)
                      body_value_names body_type_names statements
                  in
                  let* () =
                    if (not eager_functions) || item.generic_params = [] then Ok ()
                    else
                      validate_generic_legality ~span:item.span ~ret:item.ret
                        ~params:item.params ~generic_params:item.generic_params
                        statements
                  in
                  let shadowed_constants =
                    List.map (fun (parameter : Ast.param) -> parameter.name) params
                  in
                  let* statements =
                    Result_list.map
                      (resolve_stmt ~values ~shadowed_constants ~defer_const_structs
                         substitutions depth)
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
              let body_value_names =
                List.map (fun (parameter : Ast.param) -> parameter.name) params
                |> String_set.of_list
              in
              let* () =
                validate_statement_block_duplicates ~scope_names:body_value_names
                  statements
              in
              let* statements =
                Result_list.map (resolve_stmt ~defer_const_structs [] 0) statements
              in
              Ok (Ast.Statements statements)
        in
        Ok (Ast.Func { item with params; ret; body; generic_params })
  in
  let with_current_trace trace f =
    let previous = !current_trace in
    current_trace := trace;
    let result = f () in
    current_trace := previous;
    result
  in
  let resolve_program_item = function
    | Ast.Func { name; generic_params = []; _ } as item ->
        let trace = specialization_trace specializations Function_specialization name in
        with_current_trace trace (fun () -> resolve_item item)
    | item -> resolve_item item
  in
  let* items = Result_list.map resolve_program_item program.Ast.items in
  let late_functions = ref [] in
  let rec materialize () =
    match Sema_specialization.take_pending specializations with
    | None -> Ok ()
    | Some
        ({
           payload = Struct_payload { template; substitutions; values };
           name;
           depth;
           _;
         } as specialization) ->
        let result =
          with_current_trace specialization.trace (fun () ->
              match template with
              | Ast.Struct { fields; align; span; generic_params; _ } ->
                  let* fields =
                    with_generic_type_names (type_param_names generic_params) (fun () ->
                        Result_list.map
                          (fun (field : Ast.field) ->
                            let* ty =
                              resolve_ty ~values substitutions (depth + 1) field.span
                                field.ty
                            in
                            Ok ({ field with ty } : Ast.field))
                          fields)
                  in
                  let generated_item =
                    Ast.Struct { name; generic_params = []; fields; align; span }
                  in
                  let* () =
                    charge_expanded_item type_node_account
                      ~span:(specialization_application_span specialization)
                      ~where:(Printf.sprintf "struct specialization `%s`" name)
                      generated_item
                  in
                  generated := generated_item :: !generated;
                  Ok ()
              | _ ->
                  error Span.synthetic
                    "internal error: struct specialization is malformed")
          |> trace_result specializations specialization.trace
        in
        let* () = result in
        materialize ()
    | Some
        ({
           payload =
             Function_payload { item; substitutions; values = []; staged_args = _ };
           name;
           depth;
           _;
         } as specialization)
      when substitutions <> [] ->
        let* item =
          with_current_trace specialization.trace (fun () ->
              resolve_function substitutions (depth + 1) name item)
          |> trace_result specializations specialization.trace
        in
        let* () =
          charge_expanded_item type_node_account
            ~span:(specialization_application_span specialization)
            ~where:(Printf.sprintf "function specialization `%s`" name)
            item
        in
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
        let* item =
          with_current_trace specialization.trace (fun () ->
              resolve_function ~values [] (depth + 1) name item)
          |> trace_result specializations specialization.trace
        in
        let* () =
          charge_expanded_item type_node_account
            ~span:(specialization_application_span specialization)
            ~where:(Printf.sprintf "function specialization `%s`" name)
            item
        in
        let specialization =
          {
            specialization with
            payload =
              Function_payload { item; substitutions = []; values; staged_args = None };
          }
        in
        Sema_specialization.update_materialized specializations specialization;
        late_functions := specialization :: !late_functions;
        materialize ()
    | Some { payload = Function_payload _; _ } ->
        error Span.synthetic "internal error: const specialization was queued too early"
  in
  let* () = materialize () in
  List.iter
    (Sema_specialization.requeue_materialized specializations)
    (List.rev !late_functions);
  Ok ({ Ast.items = items @ List.rev !generated } : Ast.program)
