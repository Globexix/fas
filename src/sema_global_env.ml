open Sema_constants

let error span message = Error [ Diag.error span message ]

let ( let* ) result next =
  match result with Ok value -> next value | Error _ as e -> e

let collect ~source_obj ~structs ~named_types ~consts ~arrays ~global_names
    ~address_value ~readonly_names items =
  let rec value span ty expression =
    let array_items, struct_items =
      match expression with
      | Ast.Array_lit (items, _) -> (Some items, Some (None, items))
      | Ast.Struct_lit (Ast.Named_type name, items, _) -> (None, Some (Some name, items))
      | _ -> (None, None)
    in
    let map wrap ty items =
      let* items = Result_list.map (value span ty) items in
      Ok (wrap items)
    in
    match ty with
    | (Hir.Addr | Hir.Handle _) when expression <> Ast.Null (Ast.expr_span expression)
      ->
        address_value ty expression
    | Hir.Bool | Hir.Int _ | Hir.Addr | Hir.Handle _ ->
        let* actual, bits =
          const_expr ~structs ~named_types ~arrays ~globals:global_names consts
            (Some ty) expression
        in
        if not (Hir.ty_equal actual ty) then
          error span "constant initializer type mismatch"
        else
          Ok
            (match ty with
            | Hir.Bool -> Hir.Global_bool (bits <> 0L)
            | Hir.Addr | Hir.Handle _ -> Hir.Global_null
            | _ -> Hir.Global_int bits)
    | Hir.Vec _ ->
        let* actual, values =
          vector_const_expr ~structs ~named_types ~arrays ~globals:global_names consts
            (Some ty) expression
        in
        if not (Hir.ty_equal actual ty) then
          error span "constant initializer type mismatch"
        else Ok (Hir.Global_vector values)
    | Hir.Array (length, element) -> (
        match array_items with
        | Some items when List.length items = length ->
            map (fun xs -> Hir.Global_array xs) element items
        | Some _ -> error span "wrong number of array literal elements"
        | None -> error span "global initializer must be a constant expression")
    | Hir.Struct name -> (
        let items =
          match struct_items with
          | Some (None, items) -> Some items
          | Some (Some actual, items) when actual = name -> Some items
          | _ -> None
        in
        match
          ( items,
            List.find_opt
              (fun (definition : Hir.struct_def) -> definition.name = name)
              structs )
        with
        | Some items, Some definition
          when List.length items = List.length definition.fields ->
            let* values =
              Result_list.map
                (fun ((field : Hir.field), item) -> value span field.ty item)
                (List.combine definition.fields items)
            in
            Ok (Hir.Global_struct values)
        | Some _, Some _ -> error span "wrong number of struct literal fields"
        | _, None -> error span (Printf.sprintf "unknown struct `%s`" name)
        | _ -> error span "global initializer must be a constant expression")
    | Hir.Opaque _ | Hir.Void ->
        error span "global type does not have a supported object initializer"
  in
  let evaluate span ty expression =
    match value span ty expression with
    | Error [ { Diag.issue = Diag.Not_constant; _ } ] ->
        error (Ast.expr_span expression)
          "global initializer must be a constant expression"
    | result -> result
  in
  let rec globals acc = function
    | [] -> Ok (List.rev acc)
    | Ast.Global { name; ty; init; linkage; span } :: rest ->
        let* ty = source_obj span ty in
        let* init_value =
          match init with
          | None -> Ok None
          | Some expression ->
              let* value = evaluate span ty expression in
              Ok (Some value)
        in
        globals
          ({
             Hir.name;
             ty;
             init_value;
             linkage;
             readonly = List.mem name readonly_names;
           }
          :: acc)
          rest
    | _ :: rest -> globals acc rest
  in
  globals [] items
