open Hir

type record = { name : string; spelling : string; header : string option }

let includes =
  "#include <stdbool.h>\n\
   #include <stdalign.h>\n\
   #include <assert.h>\n\
   #include <stdint.h>\n\
   #include <stddef.h>\n"

let declarations records (program : program) =
  let find name = List.find (fun (s : struct_def) -> s.name = name) program.structs in
  let rec invalid = function
    | Array (0, _) -> Some "a zero-length array"
    | Array (_, ty) -> invalid ty
    | Struct name ->
        let s = find name in
        if s.fields = [] then Some "an empty struct"
        else List.find_map (fun (f : field) -> invalid f.ty) s.fields
    | _ -> None
  in
  let exports =
    List.filter_map
      (fun (g : global) ->
        if g.linkage = Ast.Export_c && Option.is_some g.init_value then
          Some (g.name, [ g.ty ], `Global g)
        else None)
      program.globals
    @ List.filter_map
        (fun (f : func) ->
          if f.linkage = External_c && f.body <> Declaration then
            Some
              (f.name, f.ret :: List.map (fun (p : local) -> p.ty) f.params, `Function f)
          else None)
        program.funcs
    |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)
  in
  let errors, exports =
    List.partition
      (fun (_, types, _) -> Option.is_some (List.find_map invalid types))
      exports
  in
  let errors =
    List.map
      (fun (name, types, _) ->
        let ty, reason =
          List.find_map
            (fun ty -> Option.map (fun reason -> (ty, reason)) (invalid ty))
            types
          |> Option.get
        in
        Printf.sprintf "export `%s` has no C declaration: %s contains %s" name
          (ty_name ty) reason)
      errors
  in
  let lines = ref [] and headers = ref [] and seen = Hashtbl.create 32 in
  let add text = lines := text :: !lines in
  let rec base = function
    | Bool -> "bool"
    | Int Usize -> "size_t"
    | Int Isize -> "ptrdiff_t"
    | Int kind ->
        let name = ty_name (Int kind) in
        (if name.[0] = 'u' then "uint" else "int")
        ^ String.sub name 1 (String.length name - 1)
        ^ "_t"
    | Void -> "void"
    | Addr -> "void *"
    | Handle name -> (
        match List.find_opt (fun r -> r.name = name) records with
        | Some r ->
            if
              String.contains r.spelling ' '
              && not (Hashtbl.mem seen ("handle:" ^ name))
            then (
              Hashtbl.add seen ("handle:" ^ name) ();
              add (r.spelling ^ ";\n"));
            Option.iter (fun h -> headers := h :: !headers) r.header;
            r.spelling ^ " *"
        | None ->
            if not (Hashtbl.mem seen ("handle:" ^ name)) then (
              Hashtbl.add seen ("handle:" ^ name) ();
              add ("struct " ^ name ^ ";\n"));
            "struct " ^ name ^ " *")
    | Vec (n, ty) ->
        let name = Printf.sprintf "fas_vec_%d_%s" n (ty_name ty) in
        if not (Hashtbl.mem seen name) then (
          Hashtbl.add seen name ();
          add
            (Printf.sprintf "typedef %s %s __attribute__((ext_vector_type(%d)));\n"
               (base ty) name n);
          checks name (Vec (n, ty)));
        name
    | Struct name ->
        if not (Hashtbl.mem seen ("struct:" ^ name)) then (
          Hashtbl.add seen ("struct:" ^ name) ();
          let s = find name in
          let fields =
            List.mapi
              (fun i (f : field) ->
                Printf.sprintf "  %s%s;\n"
                  (if i = 0 then Printf.sprintf "alignas(%d) " s.align else "")
                  (declarator f.ty f.name))
              s.fields
          in
          add ("struct " ^ name ^ " {\n" ^ String.concat "" fields ^ "};\n");
          checks ("struct " ^ name) (Struct name);
          List.iter
            (fun (f : field) ->
              add
                (Printf.sprintf
                   "static_assert(offsetof(struct %s, %s) == %d, \"%s.%s offset\");\n"
                   name f.name f.offset name f.name))
            s.fields);
        "struct " ^ name
    | Array _ | Opaque _ -> invalid_arg "internal error: C declaration base type"
  and declarator ty name =
    match ty with
    | Array (n, ty) -> declarator ty (Printf.sprintf "%s[%d]" name n)
    | _ -> base ty ^ " " ^ name
  and checks name ty =
    let size, align =
      match layout program.structs ty with
      | Ok x -> x
      | Error e -> failwith ("internal error: " ^ e)
    in
    add
      (Printf.sprintf
         "static_assert(sizeof(%s) == %d, \"%s size\");\n\
          static_assert(alignof(%s) == %d, \"%s alignment\");\n"
         name size name name align name)
  in
  let decls =
    List.map
      (fun (_, _, export) ->
        match export with
        | `Global g -> "extern " ^ declarator g.ty g.name ^ ";\n"
        | `Function f ->
            let params =
              List.map (fun (p : local) -> declarator p.ty p.name) f.params
            in
            base f.ret ^ " " ^ f.name ^ "("
            ^ (if params = [] then "void" else String.concat ", " params)
            ^ ");\n")
      exports
  in
  (String.concat "" (List.rev !lines @ decls), List.sort_uniq compare !headers, errors)

let guard name =
  String.uppercase_ascii name
  |> String.map (fun c ->
      if (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_')

let header ~name ~headers declarations =
  let guard = "FAS_" ^ guard name ^ "_H" in
  "#ifndef " ^ guard ^ "\n#define " ^ guard ^ "\n" ^ includes ^ String.concat "" headers
  ^ "#ifdef __cplusplus\nextern \"C\" {\n#endif\n" ^ declarations
  ^ "#ifdef __cplusplus\n}\n#endif\n#endif\n"
