type header = { spelling : Ast.c_header; span : Span.t }

type static_function = {
  name : string;
  return_type : string;
  parameter_types : string list;
  variadic : bool;
  void_result : bool;
  return_function_pointer : bool;
  signature : string;
  span : Span.t;
}

type adapter = {
  c_name : string;
  symbol : string;
  code : string;
  file : string;
  line : int;
  signature : string;
}

let include_line source fragment_path = function
  | { spelling = Ast.C_quoted path; _ } ->
      let path =
        if Filename.is_relative path then Filename.concat (Filename.dirname source) path
        else path
      in
      Printf.sprintf "#include %S\n" path
  | { spelling = Ast.C_system path; _ } -> "#include <" ^ path ^ ">\n"
  | { spelling = Ast.C_fragment _; _ } ->
      Printf.sprintf "#include %S\n" (Option.get fragment_path)

let find_text text needle start =
  let limit = String.length text - String.length needle in
  let rec find index =
    if index > limit then None
    else if String.sub text index (String.length needle) = needle then Some index
    else find (index + 1)
  in
  find start

let first_error output =
  String.split_on_char '\n' output
  |> List.find_opt (fun line -> Option.is_some (find_text line "error:" 0))

let error_location line =
  match find_text line "error:" 0 with
  | None -> None
  | Some error_at -> (
      let error_at =
        if error_at >= 6 && String.sub line (error_at - 6) 6 = "fatal " then
          error_at - 6
        else error_at
      in
      let prefix = String.trim (String.sub line 0 error_at) in
      let prefix =
        if String.ends_with ~suffix:":" prefix then
          String.sub prefix 0 (String.length prefix - 1)
        else prefix
      in
      match String.rindex_opt prefix ':' with
      | None -> None
      | Some column_end -> (
          let before_column = String.sub prefix 0 column_end in
          match String.rindex_opt before_column ':' with
          | None -> None
          | Some line_end ->
              let file = String.sub before_column 0 line_end in
              let line =
                String.sub before_column (line_end + 1)
                  (String.length before_column - line_end - 1)
              and column =
                String.sub prefix (column_end + 1)
                  (String.length prefix - column_end - 1)
              in
              Option.bind (int_of_string_opt line) (fun line ->
                  Option.map
                    (fun column -> (file, line, column))
                    (int_of_string_opt column))))

let compilation_error ?(prefix = "C compilation failed") fallback output =
  let line = Option.value ~default:(String.trim output) (first_error output) in
  let message =
    match find_text line "error:" 0 with
    | None -> line
    | Some start ->
        String.trim (String.sub line (start + 6) (String.length line - start - 6))
  in
  let span, notes =
    match error_location line with
    | Some (file, line, column) when Filename.check_suffix file ".fas" ->
        (Span.make ~file ~start_offset:0 ~end_offset:0 ~line ~column, [])
    | Some (file, line, column) ->
        (fallback, [ Printf.sprintf "%s:%d:%d" file line column ])
    | None -> (fallback, [])
  in
  Diag.error ~notes span (if message = "" then prefix else prefix ^ ": " ^ message)

let unit_line unit_path output =
  match find_text output (unit_path ^ ":") 0 with
  | None -> None
  | Some start -> (
      let digit = start + String.length unit_path + 1 in
      let finish = try String.index_from output digit ':' with Not_found -> digit in
      try Some (int_of_string (String.sub output digit (finish - digit)))
      with Failure _ -> None)

let error_span (headers : header list) line =
  Option.bind line (fun line -> List.nth_opt headers (line - 1)) |> function
  | None -> (List.hd headers).span
  | Some header -> header.span

let macro_definitions text names =
  let wanted = Hashtbl.create 16 and definitions = Hashtbl.create 16 in
  List.iter (fun name -> Hashtbl.replace wanted name ()) names;
  let real_file = ref false in
  let take prefix line =
    if not (String.starts_with ~prefix line) then None
    else
      let start = String.length prefix and stop = ref (String.length prefix) in
      while
        !stop < String.length line
        &&
        match line.[!stop] with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
        | _ -> false
      do
        incr stop
      done;
      if !stop = start then None
      else
        Some
          ( String.sub line start (!stop - start),
            !stop < String.length line && line.[!stop] = '(' )
  in
  String.split_on_char '\n' text
  |> List.iter (fun line ->
      if String.starts_with ~prefix:"# " line then
        real_file :=
          match String.index_opt line '"' with
          | None -> false
          | Some first -> (
              match String.index_from_opt line (first + 1) '"' with
              | Some last ->
                  not
                    (String.starts_with ~prefix:"<"
                       (String.sub line (first + 1) (last - first - 1)))
              | None -> false)
      else if !real_file then
        match take "#define " line with
        | Some (name, function_like) when Hashtbl.mem wanted name ->
            Hashtbl.replace definitions name function_like
        | _ -> (
            match take "#undef " line with
            | Some (name, _) -> Hashtbl.remove definitions name
            | None -> ()));
  List.filter_map
    (fun name ->
      Option.map (fun kind -> (name, kind)) (Hashtbl.find_opt definitions name))
    names

let macro_type = function
  | (1 | 2) as code ->
      Some ((if code = 1 then "signed char" else "unsigned char"), 8, code = 2)
  | (3 | 4) as code ->
      Some ((if code = 3 then "short" else "unsigned short"), 16, code = 4)
  | (5 | 6) as code -> Some ((if code = 5 then "int" else "unsigned int"), 32, code = 6)
  | (7 | 8) as code ->
      Some ((if code = 7 then "long" else "unsigned long"), 64, code = 8)
  | (9 | 10) as code ->
      Some ((if code = 9 then "long long" else "unsigned long long"), 64, code = 10)
  | 11 -> Some ("_Bool", 8, true)
  | 12 -> Some ("char", 8, false)
  | _ -> None

let macro_probe_types =
  "signed char:1, unsigned char:2, short:3, unsigned short:4, int:5, "
  ^ "unsigned int:6, long:7, unsigned long:8, long long:9, "
  ^ "unsigned long long:10, _Bool:11, char:12, default:0"

let llvm_integer ir name =
  let key = "@" ^ name in
  String.split_on_char '\n' ir
  |> List.find_map (fun line ->
      if Option.is_none (find_text line key 0) then None
      else
        let rec value = function
          | "constant" :: ty :: literal :: _ when String.starts_with ~prefix:"i" ty ->
              let literal = String.trim literal in
              Some
                ( ty,
                  if String.ends_with ~suffix:"," literal then
                    String.sub literal 0 (String.length literal - 1)
                  else literal )
          | _ :: rest -> value rest
          | [] -> None
        in
        value (String.split_on_char ' ' line |> List.filter (( <> ) "")))

let fas_macro_value width unsigned literal =
  let value = Int64.of_string literal in
  if unsigned && width < 64 then
    Int64.logand value (Int64.sub (Int64.shift_left 1L width) 1L) |> Int64.to_string
  else if unsigned && value < 0L then Printf.sprintf "0x%Lx" value
  else Int64.to_string value

let macro_probe_source path stem candidates =
  let out = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr out)
    (fun () ->
      Printf.fprintf out "#include %S\n" stem;
      List.iteri
        (fun i (name, _) ->
          let prefix = "__fas_mv_" ^ Digest.to_hex (Digest.string stem) ^ "_" in
          Printf.fprintf out
            "static const __typeof__((%s)) %s%d __attribute__((used)) = (%s);\n\
             static const int %st_%d __attribute__((used)) = _Generic((%s), %s);\n"
            name prefix i name prefix i name macro_probe_types)
        candidates)

let imported_macro_nodes ~cc ~c_flags ~source ~unit_path ~paths ~macro_names =
  if macro_names = [] then Ok []
  else
    let common =
      [ "-x"; "c"; "--target=x86_64-unknown-linux-gnu" ]
      @ c_flags
      @ [ "-iquote"; Filename.dirname source ]
    in
    let pp = Array.of_list ([ cc; "-E"; "-dD" ] @ common @ [ unit_path ]) in
    match Process.run pp with
    | Error e -> Error e.stderr
    | Ok (text, _) ->
        let definitions = macro_definitions text macro_names in
        let candidates = List.filter (fun (_, fn) -> not fn) definitions in
        let probe = Filename.temp_file "fas-c-macro-probe-" ".c" in
        paths := probe :: !paths;
        let prefix = "__fas_mv_" ^ Digest.to_hex (Digest.string unit_path) ^ "_" in
        let run xs =
          macro_probe_source probe unit_path xs;
          let argv =
            Array.of_list
              ([ cc; "-S"; "-emit-llvm"; "-o"; "-" ]
              @ common
              @ [ "-Xclang=-skip-function-bodies"; probe ])
          in
          match Process.run argv with
          | Ok (ir, _) -> Ok (ir, xs)
          | Error e ->
              let bad =
                String.split_on_char '\n' e.stderr
                |> List.filter_map (fun line ->
                    Option.bind (error_location line) (fun (file, n, _) ->
                        if file = probe && n >= 2 then
                          Option.map fst (List.nth_opt xs ((n - 2) / 2))
                        else None))
                |> List.sort_uniq compare
              in
              if bad = [] then Error e.stderr
              else Error ("bad macros: " ^ String.concat "\n" bad)
        in
        let probed =
          match run candidates with
          | Error message when String.starts_with ~prefix:"bad macros: " message ->
              let bad =
                String.sub message 12 (String.length message - 12)
                |> String.split_on_char '\n'
              in
              run (List.filter (fun (name, _) -> not (List.mem name bad)) candidates)
          | result -> result
        in
        Result.map
          (fun (ir, candidates) ->
            let integer (i, (name, _)) =
              match
                ( llvm_integer ir (prefix ^ string_of_int i),
                  llvm_integer ir (prefix ^ "t_" ^ string_of_int i) )
              with
              | Some (ty, literal), Some (_, id) when String.starts_with ~prefix:"i" ty
                ->
                  let width =
                    int_of_string_opt (String.sub ty 1 (String.length ty - 1))
                  in
                  Option.bind width (fun width ->
                      Option.bind (int_of_string_opt id) (fun id ->
                          Option.bind (macro_type id) (fun (c_ty, bits, unsigned) ->
                              if width <> bits then None
                              else
                                Some
                                  (name, c_ty, id, fas_macro_value bits unsigned literal))))
              | _ -> None
            in
            let imported =
              List.filter_map integer (List.mapi (fun i item -> (i, item)) candidates)
            in
            let node kind name extra =
              C_import_json.Obj
                (("kind", C_import_json.Str kind)
                :: ("name", C_import_json.Str name)
                :: extra)
            in
            List.map
              (fun (name, ty, code, value) ->
                node "FasIntegerMacro" name
                  [
                    ("macroType", C_import_json.Str ty);
                    ("macroTypeCode", C_import_json.Str (string_of_int code));
                    ("value", C_import_json.Str value);
                  ])
              imported
            @ List.filter_map
                (fun (name, _) ->
                  if List.exists (fun (n, _, _, _) -> n = name) imported then None
                  else Some (node "FasInvisibleMacro" name []))
                definitions)
          probed

let imported_enum_nodes ~cc ~c_flags ~source ~unit_path ~paths declarations =
  let field name node = C_import_json.field name node in
  let text name node = Option.bind (field name node) C_import_json.string in
  let children node =
    Option.fold ~none:[] ~some:C_import_json.array (field "inner" node)
  in
  let rec enum_decl_id node =
    match text "kind" node with
    | Some "EnumType" -> Option.bind (field "decl" node) (text "id")
    | _ -> List.find_map enum_decl_id (children node)
  in
  let name node =
    match text "name" node with Some name when name <> "" -> Some name | _ -> None
  in
  let enums =
    List.filter (fun node -> text "kind" node = Some "EnumDecl") declarations
  in
  let aliases =
    List.filter (fun node -> text "kind" node = Some "TypedefDecl") declarations
  in
  let candidates =
    List.filter_map
      (fun node ->
        match text "id" node with
        | None -> None
        | Some id ->
            let c_type =
              match name node with
              | Some name -> Some ("enum " ^ name)
              | None ->
                  List.find_map
                    (fun alias ->
                      if enum_decl_id alias = Some id then name alias else None)
                    aliases
            in
            Option.map (fun c_type -> (id, c_type)) c_type)
      enums
    |> List.sort_uniq compare
  in
  if candidates = [] then Ok []
  else
    let common =
      [ "-x"; "c"; "--target=x86_64-unknown-linux-gnu" ]
      @ c_flags
      @ [ "-iquote"; Filename.dirname source ]
    in
    let probe = Filename.temp_file "fas-c-enum-probe-" ".c" in
    paths := probe :: !paths;
    let prefix = "__fas_enumty_" ^ Digest.to_hex (Digest.string unit_path) ^ "_" in
    let write_probe candidates =
      let out = open_out_bin probe in
      Fun.protect
        ~finally:(fun () -> close_out_noerr out)
        (fun () ->
          Printf.fprintf out "#include %S\n" unit_path;
          List.iteri
            (fun i (_, c_type) ->
              Printf.fprintf out
                "static const int %s%d __attribute__((used)) = _Generic((%s)0, %s); \
                 static const unsigned long long %sa_%d __attribute__((used)) = \
                 _Alignof(%s);\n"
                prefix i c_type macro_probe_types prefix i c_type)
            candidates)
    in
    let rec run candidates =
      write_probe candidates;
      let argv =
        Array.of_list
          ([ cc; "-S"; "-emit-llvm"; "-o"; "-" ]
          @ common
          @ [ "-Xclang=-skip-function-bodies"; probe ])
      in
      match Process.run argv with
      | Ok (ir, _) -> Ok (ir, candidates)
      | Error failure ->
          let bad =
            String.split_on_char '\n' failure.stderr
            |> List.filter_map (fun line ->
                Option.bind (error_location line) (fun (file, line, _) ->
                    if file = probe && line >= 2 then
                      Option.map fst (List.nth_opt candidates (line - 2))
                    else None))
            |> List.sort_uniq compare
          in
          if bad = [] then Error failure.stderr
          else
            let remaining =
              List.filter (fun (id, _) -> not (List.mem id bad)) candidates
            in
            if remaining = [] then Ok ("", []) else run remaining
    in
    Result.map
      (fun (ir, candidates) ->
        List.filter_map
          (fun (i, (id, _)) ->
            Option.bind
              (llvm_integer ir (prefix ^ string_of_int i))
              (fun (_, code) ->
                Option.bind (int_of_string_opt code) (fun code ->
                    Option.bind (macro_type code) (fun _ ->
                        Option.map
                          (fun (_, align) ->
                            C_import_json.Obj
                              [
                                ("kind", C_import_json.Str "FasEnumType");
                                ("enumId", C_import_json.Str id);
                                ( "underlyingCode",
                                  C_import_json.Str (string_of_int code) );
                                ("align", C_import_json.Str align);
                              ])
                          (llvm_integer ir (prefix ^ "a_" ^ string_of_int i))))))
          (List.mapi (fun i candidate -> (i, candidate)) candidates))
      (run candidates)

type builtin_info = Integer of Ast.ty | Unsupported_integer | Floating of int

let builtin_info_of_name = function
  | "_Bool" | "bool" -> Some (Integer Ast.Bool)
  | "void" -> Some (Integer Ast.Void)
  | "char" | "signed char" -> Some (Integer (Ast.Int Ast.I8))
  | "unsigned char" -> Some (Integer (Ast.Int Ast.U8))
  | "short" -> Some (Integer (Ast.Int Ast.I16))
  | "unsigned short" -> Some (Integer (Ast.Int Ast.U16))
  | "int" -> Some (Integer (Ast.Int Ast.I32))
  | "unsigned int" -> Some (Integer (Ast.Int Ast.U32))
  | "long" | "long long" -> Some (Integer (Ast.Int Ast.I64))
  | "unsigned long" | "unsigned long long" -> Some (Integer (Ast.Int Ast.U64))
  | "__int128" | "unsigned __int128" -> Some Unsupported_integer
  | "float" | "double" | "long double" | "__fp16" | "_Float16" | "_Float128"
  | "__float128" | "__bf16" ->
      Some (Floating 0)
  | _ -> None

let imported_structured_type_nodes ~cc ~c_flags ~source ~unit_path ~paths declarations =
  let field key node = C_import_json.field key node in
  let string key node = Option.bind (field key node) C_import_json.string in
  let children node =
    Option.fold ~none:[] ~some:C_import_json.array (field "inner" node)
  in
  let id node = string "id" node in
  let name node = string "name" node in
  let kind node = string "kind" node in
  let source_offset node =
    let loc =
      Option.map
        (fun loc -> Option.value ~default:loc (field "expansionLoc" loc))
        (field "loc" node)
    in
    Option.bind loc (fun loc ->
        Option.bind (field "offset" loc) (function
          | C_import_json.Num value -> int_of_string_opt value
          | C_import_json.Str value -> int_of_string_opt value
          | _ -> None))
  in
  let source_key node =
    let loc =
      Option.map
        (fun loc -> Option.value ~default:loc (field "expansionLoc" loc))
        (field "loc" node)
    in
    let file =
      Option.bind loc (fun loc ->
          match string "presumedFile" loc with
          | Some _ as file -> file
          | None -> string "file" loc)
    in
    Option.bind file (fun file ->
        Option.map
          (fun offset -> "field:" ^ file ^ ":" ^ string_of_int offset)
          (source_offset node))
  in
  let rec direct_record_id node =
    match kind node with
    | Some "RecordType" -> Option.bind (field "decl" node) (string "id")
    | Some
        ( "ElaboratedType" | "AttributedType" | "ParenType" | "TypedefType" | "QualType"
        | "MacroQualifiedType" ) ->
        children node |> List.find_map direct_record_id
    | _ -> children node |> List.find_map direct_record_id
  in
  let rec collect_records node =
    (if kind node = Some "RecordDecl" then [ node ] else [])
    @ List.concat_map collect_records (children node)
  in
  let rec flatten node = node :: List.concat_map flatten (children node) in
  let all_nodes = List.concat_map flatten declarations in
  let top_declarations =
    List.filter
      (fun node ->
        List.mem (kind node)
          [ Some "FunctionDecl"; Some "VarDecl"; Some "EnumConstantDecl" ]
        && Option.is_some (name node)
        && field "isImplicit" node <> Some (C_import_json.Bool true))
      all_nodes
  in
  let alias_names_by_record = Hashtbl.create 32 in
  List.iter
    (fun node ->
      if kind node = Some "TypedefDecl" then
        match (name node, direct_record_id node) with
        | Some alias, Some record_id ->
            Hashtbl.replace alias_names_by_record record_id
              (alias
              :: Option.value ~default:[]
                   (Hashtbl.find_opt alias_names_by_record record_id))
        | _ -> ())
    declarations;
  let roots = List.concat_map collect_records declarations in
  let probes = Hashtbl.create (List.length top_declarations + 64) in
  let add_probe target expression =
    Option.iter (fun target -> Hashtbl.replace probes target expression) target
  in
  let has_restrict_qualifier value =
    String.map
      (function ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> c | _ -> ' ')
      value
    |> String.split_on_char ' '
    |> List.exists (fun qualifier ->
        List.mem qualifier [ "restrict"; "__restrict"; "__restrict__" ])
  in
  List.iter
    (fun node ->
      let c_name = Option.value ~default:"" (name node) in
      add_probe (Some ("decl:" ^ c_name)) c_name)
    top_declarations;
  List.iter
    (fun node ->
      match (kind node, name node) with
      | Some "FunctionDecl", Some function_name ->
          children node
          |> List.filter (fun child -> kind child = Some "ParmVarDecl")
          |> List.iteri (fun index parameter ->
              Option.iter
                (fun parameter_type ->
                  if has_restrict_qualifier parameter_type then
                    add_probe
                      (Some (Printf.sprintf "parameter:%s:%d" function_name index))
                      parameter_type)
                (Option.bind (field "type" parameter) (string "qualType")))
      | _ -> ())
    top_declarations;
  let collision_names = Hashtbl.create 16 in
  List.iter
    (fun root ->
      if field "completeDefinition" root = Some (C_import_json.Bool true) then
        let root_id = id root in
        let base =
          match (string "tagUsed" root, name root, root_id) with
          | Some tag, Some record_name, _ -> Some (tag ^ " " ^ record_name, record_name)
          | _, None, Some id -> (
              match Hashtbl.find_opt alias_names_by_record id with
              | Some (alias :: _) -> Some (alias, alias)
              | _ -> None)
          | _ -> None
        in
        Option.iter
          (fun (base_type, root_name) ->
            let fields = ref [] in
            let rec collect record =
              let nested_records =
                children record
                |> List.filter (fun child -> kind child = Some "RecordDecl")
              in
              children record
              |> List.iter (fun child ->
                  if kind child = Some "FieldDecl" then
                    match name child with
                    | Some field_name
                      when field "isBitfield" child <> Some (C_import_json.Bool true) ->
                        Option.iter
                          (fun target ->
                            fields :=
                              ( field_name,
                                target,
                                "((" ^ base_type ^ " *)0)->" ^ field_name )
                              :: !fields)
                          (source_key child)
                    | None ->
                        Option.iter collect
                          (List.find_opt
                             (fun nested -> source_offset nested = source_offset child)
                             nested_records)
                    | _ -> ())
            in
            collect root;
            let by_name = Hashtbl.create 16 in
            List.iter
              (fun (field_name, _, _) ->
                Hashtbl.replace by_name field_name
                  (1 + Option.value ~default:0 (Hashtbl.find_opt by_name field_name)))
              !fields;
            let duplicates =
              Hashtbl.fold
                (fun field_name count acc ->
                  if count > 1 then field_name :: acc else acc)
                by_name []
            in
            if duplicates <> [] then Hashtbl.replace collision_names root_name ();
            List.iter
              (fun (field_name, target, expression) ->
                if not (List.mem field_name duplicates) then
                  add_probe (Some target) expression)
              !fields)
          base)
    roots;
  let candidates =
    Hashtbl.fold (fun target expression acc -> (target, expression) :: acc) probes []
  in
  let prefix = "__fas_type_probe_" ^ Digest.to_hex (Digest.string unit_path) ^ "_" in
  let indexed =
    List.mapi (fun index (target, expression) -> (index, target, expression)) candidates
  in
  let append_type_probes () =
    let out = open_out_gen [ Open_append; Open_binary ] 0o600 unit_path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr out)
      (fun () ->
        List.iter
          (fun (index, _, expression) ->
            Printf.fprintf out "typedef __typeof__(%s) %s%d;\n" expression prefix index)
          indexed)
  in
  let common =
    [ "-x"; "c"; "--target=x86_64-unknown-linux-gnu" ]
    @ c_flags
    @ [ "-iquote"; Filename.dirname source ]
  in
  append_type_probes ();
  let ast_path = Filename.temp_file "fas-c-type-probe-" ".json" in
  paths := ast_path :: !paths;
  let ast_argv =
    Array.of_list
      ([
         cc;
         "-fsyntax-only";
         "-Xclang";
         "-ast-dump=json";
         "-Xclang";
         "-skip-function-bodies";
       ]
      @ common @ [ unit_path ])
  in
  match Process.run_to_file ast_argv ast_path with
  | Error failure -> Error failure.stderr
  | Ok _ ->
      let ast_channel = open_in_bin ast_path in
      let ast_nodes =
        Fun.protect
          ~finally:(fun () -> close_in_noerr ast_channel)
          (fun () -> C_import_json.declarations ast_channel)
      in
      let declarations =
        List.filter
          (fun node ->
            not
              (kind node = Some "TypedefDecl"
              && Option.fold ~none:false ~some:(String.starts_with ~prefix) (name node)
              ))
          ast_nodes
      in
      let is_type_node node =
        Option.fold ~none:false ~some:(String.ends_with ~suffix:"Type") (kind node)
      in
      let probe_types =
        List.filter_map
          (fun node ->
            match (kind node, name node) with
            | Some "TypedefDecl", Some probe_name
              when String.starts_with ~prefix probe_name ->
                let index =
                  String.sub probe_name (String.length prefix)
                    (String.length probe_name - String.length prefix)
                in
                Option.bind (int_of_string_opt index) (fun index ->
                    match
                      ( List.nth_opt indexed index,
                        List.find_opt is_type_node (children node) )
                    with
                    | Some (_, target, _), Some type_of_expr ->
                        Option.map
                          (fun tree ->
                            C_import_json.Obj
                              [
                                ("kind", C_import_json.Str "FasTypeProbe");
                                ("target", C_import_json.Str target);
                                ("tree", tree);
                              ])
                          (List.find_opt is_type_node
                             (List.rev (children type_of_expr)))
                    | _ -> None)
            | _ -> None)
          ast_nodes
      in
      Ok
        (declarations @ probe_types
        @ List.map
            (fun record_name ->
              C_import_json.Obj
                [
                  ("kind", C_import_json.Str "FasTypeCollision");
                  ("name", C_import_json.Str record_name);
                ])
            (Hashtbl.fold
               (fun record_name () acc -> record_name :: acc)
               collision_names []))

let imported_typedef_layout_nodes ~cc ~c_flags ~source ~unit_path ~paths declarations =
  let field name node = C_import_json.field name node in
  let text name node = Option.bind (field name node) C_import_json.string in
  let children node =
    Option.fold ~none:[] ~some:C_import_json.array (field "inner" node)
  in
  let candidates =
    List.concat_map
      (fun node ->
        match (text "kind" node, text "name" node) with
        | Some "TypedefDecl", Some name
          when List.exists
                 (fun child -> text "kind" child = Some "AlignedAttr")
                 (children node) ->
            [ ("typedef", name, name, []) ]
        | Some "RecordDecl", Some name
          when field "completeDefinition" node = Some (C_import_json.Bool true)
               && List.exists
                    (fun child ->
                      List.mem (text "kind" child)
                        [ Some "PackedAttr"; Some "AlignedAttr" ])
                    (children node) ->
            let tag = Option.value ~default:"struct" (text "tagUsed" node) in
            let fields =
              children node
              |> List.filter (fun child ->
                  text "kind" child = Some "FieldDecl"
                  && field "isBitfield" child <> Some (C_import_json.Bool true))
              |> List.filter_map (text "name")
            in
            [ ("record", name, tag ^ " " ^ name, fields) ]
        | _ -> [])
      declarations
    |> List.sort_uniq compare
  in
  if candidates = [] then Ok []
  else
    let probe = Filename.temp_file "fas-c-typedef-layout-" ".c" in
    paths := probe :: !paths;
    let prefix = "__fas_tdlay_" ^ Digest.to_hex (Digest.string unit_path) ^ "_" in
    let write_probe candidates =
      let out = open_out_bin probe in
      Fun.protect
        ~finally:(fun () -> close_out_noerr out)
        (fun () ->
          Printf.fprintf out "#include %S\n" unit_path;
          List.iteri
            (fun i (_, _, c_type, fields) ->
              Printf.fprintf out
                "static const unsigned long long %ss_%d __attribute__((used)) = \
                 sizeof(%s); static const unsigned long long %sa_%d \
                 __attribute__((used)) = _Alignof(%s);"
                prefix i c_type prefix i c_type;
              List.iteri
                (fun j field ->
                  Printf.fprintf out
                    " static const unsigned long long %so_%d_%d __attribute__((used)) \
                     = __builtin_offsetof(%s, %s);"
                    prefix i j c_type field)
                fields;
              output_char out '\n')
            candidates)
    in
    let rec run candidates =
      write_probe candidates;
      let argv =
        Array.of_list
          ([
             cc;
             "-S";
             "-emit-llvm";
             "-o";
             "-";
             "-x";
             "c";
             "--target=x86_64-unknown-linux-gnu";
           ]
          @ c_flags
          @ [
              "-iquote"; Filename.dirname source; "-Xclang=-skip-function-bodies"; probe;
            ])
      in
      match Process.run argv with
      | Ok (ir, _) -> Ok (ir, candidates)
      | Error failure ->
          let bad =
            String.split_on_char '\n' failure.stderr
            |> List.filter_map (fun line ->
                Option.bind (error_location line) (fun (file, line, _) ->
                    if file = probe && line >= 2 then
                      Option.map
                        (fun (_, name, _, _) -> name)
                        (List.nth_opt candidates (line - 2))
                    else None))
            |> List.sort_uniq compare
          in
          if bad = [] then Error failure.stderr
          else
            let remaining =
              List.filter (fun (_, name, _, _) -> not (List.mem name bad)) candidates
            in
            if remaining = [] then Ok ("", []) else run remaining
    in
    Result.map
      (fun (ir, candidates) ->
        List.filter_map
          (fun (i, (kind, name, _, fields)) ->
            match
              ( llvm_integer ir (prefix ^ "s_" ^ string_of_int i),
                llvm_integer ir (prefix ^ "a_" ^ string_of_int i) )
            with
            | Some (_, size), Some (_, align) -> (
                match (int_of_string_opt size, int_of_string_opt align) with
                | Some size, Some align ->
                    let extra =
                      if kind = "typedef" then []
                      else
                        let offsets =
                          List.mapi
                            (fun j field ->
                              Option.map
                                (fun (_, value) -> field ^ ":" ^ value)
                                (llvm_integer ir
                                   (prefix ^ "o_" ^ string_of_int i ^ "_"
                                  ^ string_of_int j)))
                            fields
                          |> List.filter_map Fun.id |> String.concat ","
                        in
                        [
                          ("recordName", C_import_json.Str name);
                          ("offsets", C_import_json.Str offsets);
                        ]
                    in
                    Some
                      (C_import_json.Obj
                         ([
                            ( "kind",
                              C_import_json.Str
                                (if kind = "typedef" then "FasTypedefLayout"
                                 else "FasRecordLayout") );
                            ("size", C_import_json.Str (string_of_int size));
                            ("align", C_import_json.Str (string_of_int align));
                          ]
                         @
                         if kind = "typedef" then [ ("name", C_import_json.Str name) ]
                         else extra))
                | _ -> None)
            | _ -> None)
          (List.mapi (fun i candidate -> (i, candidate)) candidates))
      (run candidates)

let has_c_name name node =
  C_import_json.field "kind" node = Some (C_import_json.Str "FunctionDecl")
  && C_import_json.field "name" node = Some (C_import_json.Str name)

let import ~cc ~debug ~keep ?(retain = false) ?(c_flags = []) ?(macro_names = []) source
    headers =
  let unit_path = Filename.temp_file "fas-c-import-" ".c" in
  let json_path = Filename.temp_file "fas-c-import-" ".json" in
  let fragment_paths = ref [] in
  let macro_paths = ref [] in
  let completed = ref false in
  let cleanup () =
    let remove path = try Sys.remove path with Sys_error _ -> () in
    if (not keep) && ((not retain) || not !completed) then
      List.iter remove (unit_path :: !fragment_paths);
    List.iter remove !macro_paths;
    remove json_path
  in
  Fun.protect ~finally:cleanup (fun () ->
      let channel = open_out_bin unit_path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr channel)
        (fun () ->
          List.iter
            (fun header ->
              let fragment_path =
                match header.spelling with
                | Ast.C_fragment fragment ->
                    let path = Filename.temp_file "fas-c-fragment-" ".c" in
                    fragment_paths := path :: !fragment_paths;
                    let fragment_channel = open_out_bin path in
                    Fun.protect
                      ~finally:(fun () -> close_out_noerr fragment_channel)
                      (fun () ->
                        Printf.fprintf fragment_channel "#line %d %S\n%s\n"
                          (header.span.Span.line + 1) source fragment.text);
                    Some path
                | Ast.C_quoted _ | Ast.C_system _ -> None
              in
              output_string channel (include_line source fragment_path header))
            headers);
      let argv =
        Array.of_list
          ([
             cc;
             "-x";
             "c";
             "-fsyntax-only";
             "-Xclang";
             "-ast-dump=json";
             "-Xclang";
             "-skip-function-bodies";
             "-H";
             "--target=x86_64-unknown-linux-gnu";
           ]
          @ c_flags
          @ [ "-iquote"; Filename.dirname source; unit_path ])
      in
      if debug || keep then
        prerr_endline
          ("fas: Clang import command: " ^ String.concat " " (Array.to_list argv));
      match Process.run_to_file argv json_path with
      | Ok trace -> (
          try
            let channel = open_in_bin json_path in
            let declarations =
              Fun.protect
                ~finally:(fun () -> close_in_noerr channel)
                (fun () -> C_import_json.declarations channel)
            in
            let origins = Hashtbl.create 32 and root = ref "" in
            String.split_on_char '\n' trace
            |> List.iter (fun line ->
                let depth = ref 0 in
                while !depth < String.length line && line.[!depth] = '.' do
                  incr depth
                done;
                if !depth > 0 && !depth < String.length line && line.[!depth] = ' ' then (
                  let path =
                    String.sub line (!depth + 1) (String.length line - !depth - 1)
                  in
                  if !depth = 1 then root := path;
                  Hashtbl.replace origins path !root));
            let annotate_declarations declarations =
              List.map
                (fun node ->
                  let open C_import_json in
                  let origin =
                    Option.bind (field "loc" node) (fun loc ->
                        Option.bind (field "file" loc) string)
                  in
                  match (node, Option.bind origin (Hashtbl.find_opt origins)) with
                  | Obj fields, Some header -> Obj (("fasHeader", Str header) :: fields)
                  | _ -> node)
                declarations
            in
            let declarations = annotate_declarations declarations in
            match
              imported_structured_type_nodes ~cc ~c_flags ~source ~unit_path
                ~paths:macro_paths declarations
            with
            | Error message ->
                Error
                  [
                    Diag.error (List.hd headers).span
                      ("internal error: C structured type import failed: " ^ message);
                  ]
            | Ok structured_declarations -> (
                let declarations = annotate_declarations structured_declarations in
                let layout_argv =
                  Array.of_list
                    ([
                       cc;
                       "-x";
                       "c";
                       "-fsyntax-only";
                       "-Xclang";
                       "-skip-function-bodies";
                       "-Xclang";
                       "-fdump-record-layouts-complete";
                       "--target=x86_64-unknown-linux-gnu";
                     ]
                    @ c_flags
                    @ [ "-iquote"; Filename.dirname source; unit_path ])
                in
                if debug || keep then
                  prerr_endline
                    ("fas: Clang layout command: "
                    ^ String.concat " " (Array.to_list layout_argv));
                match Process.run layout_argv with
                | Error failure ->
                    Error [ compilation_error (List.hd headers).span failure.stderr ]
                | Ok (layouts, _) -> (
                    match
                      imported_enum_nodes ~cc ~c_flags ~source ~unit_path
                        ~paths:macro_paths declarations
                    with
                    | Error message ->
                        Error
                          [
                            Diag.error (List.hd headers).span
                              ("internal error: C enum type import failed: " ^ message);
                          ]
                    | Ok enum_types -> (
                        match
                          imported_typedef_layout_nodes ~cc ~c_flags ~source ~unit_path
                            ~paths:macro_paths declarations
                        with
                        | Error message ->
                            Error
                              [
                                Diag.error (List.hd headers).span
                                  ("internal error: C typedef layout import failed: "
                                 ^ message);
                              ]
                        | Ok typedef_layouts -> (
                            let macro_names =
                              List.filter
                                (fun name ->
                                  not (List.exists (has_c_name name) declarations))
                                macro_names
                            in
                            match
                              imported_macro_nodes ~cc ~c_flags ~source ~unit_path
                                ~paths:macro_paths ~macro_names
                            with
                            | Error message ->
                                Error
                                  [
                                    Diag.error (List.hd headers).span
                                      ("internal error: C macro import failed: "
                                     ^ message);
                                  ]
                            | Ok macros ->
                                completed := true;
                                Ok
                                  ( declarations @ enum_types @ typedef_layouts
                                    @ C_import_json.Obj
                                        [
                                          ("kind", C_import_json.Str "FasLayoutDump");
                                          ("value", C_import_json.Str layouts);
                                        ]
                                      :: macros,
                                    (if keep then
                                       Some (unit_path :: List.rev !fragment_paths)
                                     else None),
                                    if retain then unit_path :: List.rev !fragment_paths
                                    else [] )))))
          with Failure message ->
            Error
              [
                Diag.error (List.hd headers).span
                  ("internal error: C import JSON reader: " ^ message);
              ])
      | Error failure ->
          Error
            [
              compilation_error
                (error_span headers (unit_line unit_path failure.stderr))
                failure.stderr;
            ])

let get name = C_import_json.field name
let text = function C_import_json.Str value -> Some value | _ -> None
let string name value = Option.bind (get name value) text
let children value = Option.fold ~none:[] ~some:C_import_json.array (get "inner" value)

let rec trim value =
  if value = "" then value
  else
    match value.[0] with
    | ' ' | '\t' | '\n' | '\r' -> trim (String.sub value 1 (String.length value - 1))
    | _ -> (
        let last = String.length value - 1 in
        match value.[last] with
        | ' ' | '\t' | '\n' | '\r' -> trim (String.sub value 0 last)
        | _ -> value)

let builtin_integer_type = function Integer ty -> Some ty | _ -> None
let c_type_name node = Option.bind (get "type" node) (string "qualType")

let function_result_spelling raw =
  match find_text raw "(*(" 0 with
  | None -> None
  | Some pointer ->
      let open_index = pointer + 2 in
      let rec close depth index =
        if index = String.length raw then None
        else
          let depth =
            depth + if raw.[index] = '(' then 1 else if raw.[index] = ')' then -1 else 0
          in
          if depth = 0 then Some index else close depth (index + 1)
      in
      Option.map
        (fun close ->
          String.sub raw 0 open_index
          ^ String.sub raw (close + 1) (String.length raw - close - 1))
        (close 0 open_index)

let c_function_result raw fallback =
  Option.fold ~none:fallback ~some:trim (function_result_spelling raw)

let c_named_type raw name =
  match find_text raw "(*" 0 with
  | None -> raw ^ " " ^ name
  | Some pointer -> (
      match String.index_from_opt raw (pointer + 2) ')' with
      | None -> raw ^ " " ^ name
      | Some close ->
          String.sub raw 0 close
          ^ (if close = pointer + 2 || raw.[close - 1] = '*' || raw.[close - 1] = ' '
             then ""
             else " ")
          ^ name
          ^ String.sub raw close (String.length raw - close))

let type_node node =
  children node
  |> List.find_opt (fun child ->
      Option.fold ~none:false
        ~some:(String.ends_with ~suffix:"Type")
        (string "kind" child))

let type_node_id node = Option.bind (get "decl" node) (string "id")

let record_name node =
  match string "name" node with Some name when name <> "" -> Some name | _ -> None

let type_qualifier_names node =
  Option.value ~default:"" (string "qualifiers" node)
  |> String.map (function
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> c
    | _ -> ' ')
  |> String.split_on_char ' '
  |> List.filter (( <> ) "")

let rec type_qualifiers node =
  type_qualifier_names node @ List.concat_map type_qualifiers (children node)

let rec type_has_address_space node =
  let own =
    Option.fold ~none:false
      ~some:(String.starts_with ~prefix:"__attribute__((address_space(")
      (string "qualifiers" node)
  in
  own || List.exists type_has_address_space (children node)

let rec type_has_vector node =
  List.mem (string "kind" node)
    [ Some "VectorType"; Some "ExtVectorType"; Some "DependentSizedExtVectorType" ]
  || List.exists type_has_vector (children node)

let rec type_has_function ~alias_type_node seen node =
  match string "kind" node with
  | Some ("FunctionProtoType" | "FunctionNoProtoType") -> true
  | Some "TypedefType" -> (
      match type_node_id node with
      | Some id when not (List.mem id seen) ->
          Option.fold ~none:false
            ~some:(type_has_function ~alias_type_node (id :: seen))
            (alias_type_node id)
      | _ -> false)
  | _ -> List.exists (type_has_function ~alias_type_node seen) (children node)

let rec direct_record_decl_id node =
  match string "kind" node with
  | Some "RecordType" -> type_node_id node
  | Some
      ( "ElaboratedType" | "ParenType" | "MacroQualifiedType" | "AttributedType"
      | "TypeOfType" | "TypeOfExprType" | "PredefinedSugarType" ) ->
      List.find_map direct_record_decl_id (children node)
  | _ -> None

let record_decl_id node =
  let rec find tree =
    match string "kind" tree with
    | Some "RecordType" -> type_node_id tree
    | _ -> List.find_map find (children tree)
  in
  Option.bind (type_node node) find

let rec enum_decl_id node =
  match string "kind" node with
  | Some "EnumType" -> type_node_id node
  | _ -> List.find_map enum_decl_id (children node)

let add_one_decimal value =
  let negative = String.starts_with ~prefix:"-" value in
  let digits =
    if negative then String.sub value 1 (String.length value - 1) else value
  in
  let adjust amount =
    let chars = Bytes.of_string digits in
    let carry = ref amount in
    let index = ref (Bytes.length chars - 1) in
    while !index >= 0 && !carry <> 0 do
      let digit = Char.code (Bytes.get chars !index) - Char.code '0' in
      let next = digit + !carry in
      if next >= 10 then (
        Bytes.set chars !index (Char.chr (Char.code '0' + next - 10));
        carry := 1)
      else if next < 0 then (
        Bytes.set chars !index (Char.chr (Char.code '0' + next + 10));
        carry := -1)
      else (
        Bytes.set chars !index (Char.chr (Char.code '0' + next));
        carry := 0);
      decr index
    done;
    if !carry > 0 then "1" ^ Bytes.to_string chars
    else if !carry < 0 then Bytes.to_string chars
    else Bytes.to_string chars
  in
  if negative then
    let next = adjust (-1) |> String.trim in
    if String.for_all (( = ) '0') next then "0" else "-" ^ next
  else adjust 1

let type_result ?(allow_arrays = false) ~alias_name ~alias_type_node ~resolve_alias
    ~record_ids ~record_nodes_by_id ~enum_types ~allow_record ctype =
  let child_type node =
    children node
    |> List.find_opt (fun child ->
        Option.fold ~none:false
          ~some:(String.ends_with ~suffix:"Type")
          (string "kind" child))
  in
  let rec record_value = function
    | Ast.Named_type _ -> true
    | Ast.Array (_, ty) -> record_value ty
    | _ -> false
  in
  let contains_array = function Ast.Array _ -> true | _ -> false in
  let rec resolve seen node =
    if type_has_address_space node then Error "C address spaces are not supported"
    else if type_has_vector node then Error "vector types are not supported by value"
    else
      match string "kind" node with
      | Some "QualType" ->
          Option.fold ~none:(Error "unsupported C type") ~some:(resolve seen)
            (child_type node)
      | Some "TypedefType" -> (
          match type_node_id node with
          | Some id when not (List.mem id seen) -> (
              match alias_name id with
              | Some name -> resolve_alias name
              | None -> Error "typedef has no canonical type")
          | Some _ -> Error "recursive C typedef is not supported"
          | None -> Error "typedef has no canonical type")
      | Some "PredefinedSugarType" -> (
          match Option.bind (get "type" node) (string "qualType") with
          | Some "__size_t" -> Ok (Ast.Int Ast.Usize)
          | Some "__ptrdiff_t" -> Ok (Ast.Int Ast.Isize)
          | _ ->
              Option.fold ~none:(Error "unsupported C type") ~some:(resolve seen)
                (child_type node))
      | Some
          ( "ElaboratedType" | "ParenType" | "MacroQualifiedType" | "AdjustedType"
          | "DecayedType" | "AttributedType" | "TypeOfType" | "TypeOfExprType" ) ->
          let children =
            children node
            |> List.filter (fun child ->
                Option.fold ~none:false
                  ~some:(String.ends_with ~suffix:"Type")
                  (string "kind" child))
          in
          Option.fold ~none:(Error "unsupported C type") ~some:(resolve seen)
            (List.find_opt (fun _ -> true) (List.rev children))
      | Some "BuiltinType" -> (
          match
            Option.bind (get "type" node) (fun ty ->
                Option.bind (string "qualType" ty) builtin_info_of_name)
          with
          | None -> Error "unsupported C builtin type"
          | Some (Integer ty) -> Ok ty
          | Some Unsupported_integer -> Error "`__int128` has no Fas type"
          | Some (Floating _) -> Error "floating-point types are not supported")
      | Some "ComplexType" -> Error "floating-point types are not supported"
      | Some "BitIntType" -> Error "`_BitInt` has no Fas type"
      | Some "EnumType" -> (
          match type_node_id node with
          | Some id ->
              Option.value ~default:(Error "enum representation is not supported")
                (Hashtbl.find_opt enum_types id)
          | None -> Error "enum representation is not supported")
      | Some "RecordType" -> (
          match type_node_id node with
          | Some id ->
              Option.fold ~none:(Error "anonymous records are not supported")
                ~some:(fun name -> Ok (Ast.Named_type name))
                (Hashtbl.find_opt record_ids id)
          | None -> Error "anonymous records are not supported")
      | Some ("PointerType" | "BlockPointerType" | "ObjCObjectPointerType") -> (
          let pointee = child_type node in
          match pointee with
          | None -> Error "pointer target type is not supported"
          | Some pointee when type_has_function ~alias_type_node [] pointee ->
              Ok Ast.Addr
          | Some pointee when type_has_address_space pointee ->
              Error "C address spaces are not supported"
          | Some pointee when type_has_vector pointee ->
              Error "vector types are not supported by value"
          | Some pointee ->
              let rec target seen node =
                match string "kind" node with
                | Some "QualType" ->
                    Option.fold ~none:(Error "pointer target type is not supported")
                      ~some:(target seen) (child_type node)
                | Some "TypedefType" -> (
                    match type_node_id node with
                    | Some id when not (List.mem id seen) ->
                        Option.fold ~none:(Error "pointer target type is not supported")
                          ~some:(target (id :: seen))
                          (alias_type_node id)
                    | _ -> Error "recursive C typedef is not supported")
                | Some "RecordType" -> (
                    match type_node_id node with
                    | Some id -> (
                        match Hashtbl.find_opt record_ids id with
                        | Some name -> Ok (Ast.Handle (Ast.Named_type name))
                        | None ->
                            Option.fold
                              ~none:
                                (Error "anonymous record pointers are not supported")
                              ~some:(fun record ->
                                if Option.is_some (record_name record) then Ok Ast.Addr
                                else Error "anonymous record pointers are not supported")
                              (Hashtbl.find_opt record_nodes_by_id id))
                    | None -> Error "anonymous record pointers are not supported")
                | Some
                    ( "BuiltinType" | "BitIntType" | "EnumType" | "PointerType"
                    | "BlockPointerType" | "ConstantArrayType" | "IncompleteArrayType"
                    | "VariableArrayType" | "DependentSizedArrayType" ) ->
                    Ok Ast.Addr
                | Some ("FunctionProtoType" | "FunctionNoProtoType") -> Ok Ast.Addr
                | Some
                    ( "ElaboratedType" | "ParenType" | "MacroQualifiedType"
                    | "AttributedType" | "TypeOfType" | "TypeOfExprType"
                    | "PredefinedSugarType" ) ->
                    Option.fold ~none:(Error "pointer target type is not supported")
                      ~some:(target seen) (child_type node)
                | _ -> Error "pointer target type is not supported"
              in
              target [] pointee)
      | Some "ConstantArrayType" -> (
          let size =
            match get "size" node with
            | Some (C_import_json.Num value) -> int_of_string_opt value
            | Some (C_import_json.Str value) -> int_of_string_opt value
            | _ -> None
          in
          match (size, child_type node) with
          | Some size, Some element when size >= 0 ->
              Result.map
                (fun ty -> Ast.Array (string_of_int size, ty))
                (resolve seen element)
          | _ -> Error "array types are not supported by value")
      | Some "IncompleteArrayType" -> Error "arrays of unknown size are not supported"
      | Some ("VariableArrayType" | "DependentSizedArrayType") ->
          Error "array types are not supported by value"
      | Some ("FunctionProtoType" | "FunctionNoProtoType") -> Ok Ast.Addr
      | _ ->
          Error
            ("unsupported C type "
            ^ Option.value ~default:""
                (Option.bind (get "type" node) (string "qualType")))
  in
  match resolve [] ctype with
  | Ok ty when (not allow_arrays) && contains_array ty ->
      Error "array types are not supported by value"
  | Ok ty when (not allow_record) && record_value ty ->
      Error "struct and union values are not supported"
  | result -> result

type mapped = {
  items : Ast.item list;
  aliases : (string * Ast.ty) list;
  unsupported : (string * string) list;
  identities : (string * string) list;
  manifest : string list;
  static_functions : static_function list;
  records : (string * string * string option) list;
  record_types : (string * string * string option) list;
  incomplete_arrays : (string * Ast.ty) list;
  container_names : string list;
}

let adapter_symbol source name =
  "__fas_c_adapter_" ^ Digest.to_hex (Digest.string source) ^ "_" ^ name

let make_adapter ~occupied source (static : static_function) =
  if static.variadic then
    Error ("static variadic function `" ^ static.name ^ "` needs an adapter")
  else
    let base = adapter_symbol source static.name in
    let rec choose suffix =
      let symbol = if suffix = 0 then base else base ^ "_" ^ string_of_int suffix in
      if List.mem symbol occupied then choose (suffix + 1) else symbol
    in
    let symbol = choose 0 in
    let params =
      List.mapi
        (fun i ty -> c_named_type ty ("fas_arg" ^ string_of_int i))
        static.parameter_types
    in
    let call_args =
      List.mapi (fun i _ -> "fas_arg" ^ string_of_int i) static.parameter_types
    in
    let params = if params = [] then "void" else String.concat ", " params in
    let call = static.name ^ "(" ^ String.concat ", " call_args ^ ")" in
    let body = if static.void_result then call ^ ";" else "return " ^ call ^ ";" in
    let return_type, return_typedef =
      if static.return_function_pointer then
        let alias = symbol ^ "_result" in
        (alias, "typedef " ^ c_named_type static.return_type alias ^ ";\n")
      else (static.return_type, "")
    in
    Ok
      {
        c_name = static.name;
        symbol;
        code =
          Printf.sprintf
            "#line %d %S\n%s__attribute__((visibility(\"hidden\"))) %s %s(%s) { %s }\n"
            static.span.Span.line source return_typedef return_type symbol params body;
        file = source;
        line = static.span.Span.line;
        signature = static.signature;
      }

let append_adapters path adapters =
  let channel = open_out_gen [ Open_append; Open_binary ] 0o600 path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () ->
      List.iter (fun (adapter : adapter) -> output_string channel adapter.code) adapters)

let adapter_manifest (adapter : adapter) =
  Printf.sprintf "%s\tadapter for %s\t%s\t\t%s:%d" adapter.symbol adapter.c_name
    adapter.signature adapter.file adapter.line

let item_name = function
  | Ast.Opaque { name; _ }
  | Ast.Struct { name; _ }
  | Ast.Const { name; _ }
  | Ast.Global { name; _ }
  | Ast.Func { name; _ } ->
      name
  | _ -> ""

type clang_layout = {
  layout_name : string;
  size : int;
  align : int;
  offsets : (string * int) list;
  members : (string * int) list;
  direct_members : (string * int) list;
  direct_offsets : (string * int) list;
}

let clang_layouts text =
  let lines = String.split_on_char '\n' text in
  let after marker line =
    match find_text line marker 0 with
    | None -> None
    | Some at ->
        let start = at + String.length marker in
        let finish = ref start in
        while
          !finish < String.length line && line.[!finish] >= '0' && line.[!finish] <= '9'
        do
          incr finish
        done;
        int_of_string_opt (String.sub line start (!finish - start))
  in
  let index value items =
    let rec find i = function
      | [] -> None
      | item :: rest -> if item = value then Some i else find (i + 1) rest
    in
    find 0 items
  in
  let rec groups current acc = function
    | [] -> List.rev (finish current acc)
    | line :: rest when find_text line "*** Dumping AST Record Layout" 0 = Some 0 ->
        groups [ line ] (finish current acc) rest
    | line :: rest -> groups (line :: current) acc rest
  and finish group acc =
    match List.rev group with
    | [] -> acc
    | lines -> (
        let header =
          List.find_map
            (fun line ->
              match String.index_opt line '|' with
              | None -> None
              | Some bar ->
                  let right =
                    String.sub line (bar + 1) (String.length line - bar - 1)
                  in
                  let body = trim right in
                  if
                    String.length right - String.length body = 1
                    && (String.starts_with ~prefix:"struct " body
                       || String.starts_with ~prefix:"union " body)
                  then Some (line, body)
                  else None)
            lines
        in
        let summary =
          List.find_opt (fun line -> Option.is_some (after "sizeof=" line)) lines
        in
        match (header, summary) with
        | Some (header, name), Some summary -> (
            match (after "sizeof=" summary, after "align=" summary) with
            | Some size, Some align ->
                let header_at = Option.get (index header lines) in
                let summary_at = Option.get (index summary lines) in
                let members =
                  List.mapi (fun i line -> (i, line)) lines
                  |> List.filter_map (fun (i, line) ->
                      if i <= header_at || i >= summary_at then None
                      else
                        match String.index_opt line '|' with
                        | None -> None
                        | Some bar ->
                            let left = trim (String.sub line 0 bar) in
                            let right =
                              String.sub line (bar + 1) (String.length line - bar - 1)
                            in
                            let body = trim right in
                            if String.length right - String.length body < 3 then None
                            else
                              Option.map
                                (fun offset -> (body, offset))
                                (int_of_string_opt left))
                in
                let direct_members =
                  List.mapi (fun i line -> (i, line)) lines
                  |> List.filter_map (fun (i, line) ->
                      if i <= header_at || i >= summary_at then None
                      else
                        match String.index_opt line '|' with
                        | None -> None
                        | Some bar ->
                            let left = trim (String.sub line 0 bar) in
                            let right =
                              String.sub line (bar + 1) (String.length line - bar - 1)
                            in
                            let rec indentation count =
                              if count < String.length right && right.[count] = ' ' then
                                indentation (count + 1)
                              else count
                            in
                            let body = trim right in
                            if indentation 0 <> 3 then None
                            else
                              Option.map
                                (fun offset -> (body, offset))
                                (int_of_string_opt left))
                in
                let offsets =
                  List.mapi (fun i line -> (i, line)) lines
                  |> List.filter_map (fun (i, line) ->
                      if i <= header_at || i >= summary_at then None
                      else
                        match String.index_opt line '|' with
                        | None -> None
                        | Some bar ->
                            let left = trim (String.sub line 0 bar) in
                            let right =
                              String.sub line (bar + 1) (String.length line - bar - 1)
                            in
                            let body = trim right in
                            if String.length right - String.length body < 3 then None
                            else
                              let field =
                                match
                                  List.rev
                                    (String.split_on_char ' ' body
                                    |> List.filter (( <> ) ""))
                                with
                                | name :: _ -> Some name
                                | [] -> None
                              in
                              Option.bind (int_of_string_opt left) (fun offset ->
                                  Option.map (fun field -> (field, offset)) field))
                in
                let direct_offsets =
                  List.filter_map
                    (fun (body, offset) ->
                      match
                        List.rev
                          (String.split_on_char ' ' body |> List.filter (( <> ) ""))
                      with
                      | field :: _ -> Some (field, offset)
                      | [] -> None)
                    direct_members
                in
                {
                  layout_name = name;
                  size;
                  align;
                  offsets;
                  members;
                  direct_members;
                  direct_offsets;
                }
                :: acc
            | _ -> acc)
        | _ -> acc)
  in
  groups [] [] lines

let map_declarations ?(container = false) ~span declarations =
  let layout_dump =
    List.find_map
      (function
        | C_import_json.Obj fields
          when List.assoc_opt "kind" fields = Some (C_import_json.Str "FasLayoutDump")
          ->
            Option.bind (List.assoc_opt "value" fields) C_import_json.string
        | _ -> None)
      declarations
  in
  let layouts = Option.fold ~none:[] ~some:clang_layouts layout_dump in
  let typedef_layouts = Hashtbl.create 16 in
  List.iter
    (fun node ->
      if string "kind" node = Some "FasTypedefLayout" then
        match (string "name" node, string "size" node, string "align" node) with
        | Some name, Some size, Some align -> (
            match (int_of_string_opt size, int_of_string_opt align) with
            | Some size, Some align -> Hashtbl.replace typedef_layouts name (size, align)
            | _ -> ())
        | _ -> ())
    declarations;
  let structured_nodes =
    List.filter
      (fun node ->
        List.mem (string "kind" node)
          [
            Some "FasTypeProbe";
            Some "FasTypeCollision";
            Some "FasEnumType";
            Some "FasRecordLayout";
          ])
      declarations
  in
  let structured_types = Hashtbl.create 128 and colliding_records = Hashtbl.create 16 in
  List.iter
    (fun node ->
      match string "kind" node with
      | Some "FasTypeProbe" -> (
          match (string "target" node, get "tree" node) with
          | Some target, Some tree -> Hashtbl.replace structured_types target tree
          | _ -> ())
      | Some "FasTypeCollision" ->
          Option.iter
            (fun name -> Hashtbl.replace colliding_records name ())
            (string "name" node)
      | _ -> ())
    structured_nodes;
  let nodes =
    C_import_json.array (C_import_json.Arr declarations)
    |> List.filter (fun node ->
        not
          (List.mem (string "kind" node)
             [
               Some "FasLayoutDump";
               Some "FasTypedefLayout";
               Some "FasTypeProbe";
               Some "FasTypeCollision";
               Some "FasEnumType";
               Some "FasRecordLayout";
             ]))
  in
  let enum_probe_types = Hashtbl.create 32 in
  List.iter
    (fun node ->
      if string "kind" node = Some "FasEnumType" then
        match
          (string "enumId" node, string "underlyingCode" node, string "align" node)
        with
        | Some id, Some code, Some align ->
            Option.iter
              (fun result -> Hashtbl.replace enum_probe_types id result)
              (Option.bind (int_of_string_opt code) (fun code ->
                   Option.bind (int_of_string_opt align) (fun align ->
                       Option.bind (macro_type code) (fun (_, bits, unsigned) ->
                           let ty =
                             if code = 11 then Some Ast.Bool
                             else
                               match (bits, unsigned) with
                               | 8, false -> Some (Ast.Int Ast.I8)
                               | 8, true -> Some (Ast.Int Ast.U8)
                               | 16, false -> Some (Ast.Int Ast.I16)
                               | 16, true -> Some (Ast.Int Ast.U16)
                               | 32, false -> Some (Ast.Int Ast.I32)
                               | 32, true -> Some (Ast.Int Ast.U32)
                               | 64, false -> Some (Ast.Int Ast.I64)
                               | 64, true -> Some (Ast.Int Ast.U64)
                               | _ -> None
                           in
                           Option.map
                             (fun ty ->
                               if align > bits / 8 then
                                 Error "over-aligned C enums are not supported"
                               else Ok ty)
                             ty))))
        | _ -> ())
    structured_nodes;
  let record_layout_probes = Hashtbl.create 32 in
  List.iter
    (fun node ->
      if string "kind" node = Some "FasRecordLayout" then
        match
          ( string "recordName" node,
            string "size" node,
            string "align" node,
            string "offsets" node )
        with
        | Some name, Some size, Some align, Some offsets -> (
            let field_offsets =
              String.split_on_char ',' offsets
              |> List.filter_map (fun offset ->
                  match String.split_on_char ':' offset with
                  | [ name; value ] ->
                      Option.map (fun value -> (name, value)) (int_of_string_opt value)
                  | _ -> None)
            in
            match (int_of_string_opt size, int_of_string_opt align) with
            | Some size, Some align ->
                Hashtbl.replace record_layout_probes name (size, align, field_offsets)
            | _ -> ())
        | _ -> ())
    structured_nodes;
  let integer_width = function
    | Ast.Int (Ast.I8 | Ast.U8) -> Some 8
    | Ast.Int (Ast.I16 | Ast.U16) -> Some 16
    | Ast.Int (Ast.I32 | Ast.U32) -> Some 32
    | Ast.Int (Ast.I64 | Ast.U64 | Ast.Isize | Ast.Usize) -> Some 64
    | _ -> None
  in
  let enum_constant_type node =
    let rec builtin_type seen tree =
      match string "kind" tree with
      | Some "BuiltinType" ->
          Option.bind
            (Option.bind (get "type" tree) (fun ty ->
                 Option.bind (string "qualType" ty) builtin_info_of_name))
            builtin_integer_type
      | Some "TypedefType" -> (
          match type_node_id tree with
          | Some id when not (List.mem id seen) ->
              List.find_map
                (fun alias ->
                  if
                    string "kind" alias = Some "TypedefDecl"
                    && string "id" alias = Some id
                  then Option.bind (type_node alias) (builtin_type (id :: seen))
                  else None)
                nodes
          | _ -> None)
      | Some
          ( "QualType" | "ElaboratedType" | "ParenType" | "MacroQualifiedType"
          | "AttributedType" | "TypeOfType" | "TypeOfExprType" | "PredefinedSugarType"
            ) ->
          Option.bind (type_node tree) (builtin_type seen)
      | _ -> None
    in
    let constants =
      children node
      |> List.filter (fun child -> string "kind" child = Some "EnumConstantDecl")
    in
    let typed_constants =
      List.filter_map
        (fun child ->
          Option.bind (record_name child) (fun name ->
              Option.bind
                (Hashtbl.find_opt structured_types ("decl:" ^ name))
                (builtin_type [])))
        constants
    in
    match typed_constants with
    | first :: rest
      when List.length typed_constants = List.length constants
           && List.for_all (( = ) first) rest ->
        Some first
    | _ -> None
  in
  let enum_representation node underlying previous =
    match (enum_constant_type node, underlying) with
    | Some constant_type, Ok underlying_type -> (
        match (integer_width constant_type, integer_width underlying_type) with
        | Some constant_width, Some underlying_width
          when constant_width = underlying_width ->
            Ok constant_type
        | _ -> underlying)
    | _ -> Option.value ~default:underlying previous
  in
  let macro_shadow_names =
    List.filter_map
      (fun node ->
        match string "kind" node with
        | Some "FasIntegerMacro" -> string "name" node
        | _ -> None)
      nodes
  in
  let nodes =
    List.filter
      (fun node ->
        match string "kind" node with
        | Some ("FasIntegerMacro" | "FasInvisibleMacro") -> true
        | _ ->
            not
              (List.mem
                 (Option.value ~default:"" (record_name node))
                 macro_shadow_names))
      nodes
  in
  let records = Hashtbl.create 64
  and record_ids = Hashtbl.create 64
  and record_nodes_by_id = Hashtbl.create 64
  and anonymous_record_ids = Hashtbl.create 32
  and anonymous_record_aliases = Hashtbl.create 32
  and enums = Hashtbl.create 32
  and enum_id_types = Hashtbl.create 32
  and enum_id_names = Hashtbl.create 32
  and alias_nodes = Hashtbl.create 64
  and alias_nodes_by_id = Hashtbl.create 64
  and alias_names_by_id = Hashtbl.create 64 in
  List.iter
    (fun node ->
      match string "kind" node with
      | Some "RecordDecl" -> (
          match
            (Option.bind (get "id" node) C_import_json.string, record_name node)
          with
          | Some id, Some name ->
              Hashtbl.replace records name (Some name);
              Hashtbl.replace record_ids id name;
              Hashtbl.replace record_nodes_by_id id node
          | Some id, None ->
              Hashtbl.replace anonymous_record_ids id ();
              Hashtbl.replace record_nodes_by_id id node
          | None, _ -> ())
      | Some "TypedefDecl" -> (
          match (record_name node, get "id" node) with
          | Some name, Some (C_import_json.Str id) ->
              Hashtbl.replace alias_nodes name node;
              Hashtbl.replace alias_nodes_by_id id node;
              Hashtbl.replace alias_names_by_id id name
          | _ -> ())
      | _ -> ())
    nodes;
  List.iter
    (fun node ->
      if string "kind" node = Some "EnumDecl" then
        match string "id" node with
        | Some id ->
            Option.iter (Hashtbl.replace enum_id_names id) (record_name node);
            Option.iter
              (fun underlying ->
                let previous =
                  match Hashtbl.find_opt enum_id_types id with
                  | Some _ as previous -> previous
                  | None -> Option.bind (record_name node) (Hashtbl.find_opt enums)
                in
                let representation = enum_representation node underlying previous in
                Hashtbl.replace enum_id_types id representation;
                Option.iter
                  (fun name -> Hashtbl.replace enums name representation)
                  (record_name node))
              (Hashtbl.find_opt enum_probe_types id)
        | None -> ())
    nodes;
  List.iter
    (fun node ->
      match (string "kind" node, record_name node, record_decl_id node) with
      | Some "TypedefDecl", Some name, Some id when Hashtbl.mem anonymous_record_ids id
        ->
          let canonical = Option.value ~default:name (Hashtbl.find_opt record_ids id) in
          Hashtbl.replace records canonical (Some canonical);
          Hashtbl.replace anonymous_record_aliases canonical ();
          Hashtbl.replace record_ids id canonical;
          Hashtbl.replace record_nodes_by_id id (Hashtbl.find record_nodes_by_id id)
      | _ -> ())
    nodes;
  let same_record_typedef id _tag node = record_decl_id node = Some id in
  let collides_with_ordinary tag id =
    List.exists
      (fun node ->
        let name = record_name node in
        match string "kind" node with
        | Some ("FunctionDecl" | "VarDecl") -> name = Some tag
        | Some "TypedefDecl" -> name = Some tag && not (same_record_typedef id tag node)
        | Some "EnumDecl" ->
            List.exists
              (fun child ->
                string "kind" child = Some "EnumConstantDecl"
                && record_name child = Some tag)
              (children node)
        | _ -> false)
      nodes
  in
  Hashtbl.iter
    (fun id node ->
      match record_name node with
      | Some tag when collides_with_ordinary tag id ->
          let alias =
            List.find_map
              (fun candidate ->
                match record_name candidate with
                | Some name when name <> tag && same_record_typedef id tag candidate ->
                    Some name
                | _ -> None)
              nodes
          in
          (match alias with
          | Some name ->
              Hashtbl.replace records name (Some name);
              Hashtbl.replace record_ids id name
          | None -> Hashtbl.remove record_ids id);
          Hashtbl.replace records tag alias
      | _ -> ())
    record_nodes_by_id;
  List.iter
    (fun node ->
      match (string "kind" node, record_name node, enum_decl_id node) with
      | Some "TypedefDecl", Some name, Some id ->
          let representation =
            match Hashtbl.find_opt enum_id_names id with
            | Some enum_name -> (
                match Hashtbl.find_opt enums enum_name with
                | Some _ as representation -> representation
                | None -> Hashtbl.find_opt enum_id_types id)
            | None -> Hashtbl.find_opt enum_id_types id
          in
          Option.iter
            (fun representation ->
              Hashtbl.replace enum_id_types id representation;
              Hashtbl.replace enums name representation)
            representation
      | _ -> ())
    nodes;
  let aliases = Hashtbl.create 64 in
  let aliases_being_resolved = Hashtbl.create 16 in
  let alias_name id = Hashtbl.find_opt alias_names_by_id id in
  let alias_type_node id =
    Option.bind (Hashtbl.find_opt alias_nodes_by_id id) type_node
  in
  let machine_integer_type = function
    | "size_t" | "uintptr_t" -> Some (Ast.Int Ast.Usize)
    | "ssize_t" | "ptrdiff_t" | "intptr_t" -> Some (Ast.Int Ast.Isize)
    | _ -> None
  in
  let rec alias name =
    match Hashtbl.find_opt aliases name with
    | Some result -> result
    | None when Hashtbl.mem aliases_being_resolved name ->
        Error "recursive C typedef is not supported"
    | None -> (
        Hashtbl.replace aliases_being_resolved name ();
        match Hashtbl.find_opt typedef_layouts name with
        | Some (size, align) when align > size ->
            let result =
              Error
                (Printf.sprintf
                   "over-aligned typedef `%s` has alignment greater than its size" name)
            in
            Hashtbl.replace aliases name result;
            Hashtbl.remove aliases_being_resolved name;
            result
        | _ ->
            let result =
              match machine_integer_type name with
              | Some ty -> Ok ty
              | None -> (
                  match Hashtbl.find_opt alias_nodes name with
                  | None -> Error ("unknown C typedef " ^ name)
                  | Some node ->
                      Option.fold ~none:(Error "typedef has no canonical type")
                        ~some:
                          (type_result ~allow_arrays:true ~alias_name ~alias_type_node
                             ~resolve_alias:alias ~record_ids ~record_nodes_by_id
                             ~enum_types:enum_id_types ~allow_record:true)
                        (type_node node))
            in
            Hashtbl.replace aliases name result;
            Hashtbl.remove aliases_being_resolved name;
            result)
  in
  let source_location_key node =
    let loc =
      Option.map
        (fun loc -> Option.value ~default:loc (get "expansionLoc" loc))
        (get "loc" node)
    in
    let file =
      Option.bind loc (fun loc ->
          match string "presumedFile" loc with
          | Some _ as file -> file
          | None -> string "file" loc)
    in
    let offset =
      Option.bind loc (fun loc ->
          Option.bind (get "offset" loc) (function
            | C_import_json.Num value -> int_of_string_opt value
            | C_import_json.Str value -> int_of_string_opt value
            | _ -> None))
    in
    Option.bind file (fun file ->
        Option.map (fun offset -> "field:" ^ file ^ ":" ^ string_of_int offset) offset)
  in
  let type_tree node =
    match string "kind" node with
    | Some "TypedefDecl" -> type_node node
    | Some ("FunctionDecl" | "VarDecl" | "EnumConstantDecl") ->
        Option.bind (record_name node) (fun name ->
            Hashtbl.find_opt structured_types ("decl:" ^ name))
    | Some "FieldDecl" -> (
        let key = source_location_key node in
        match Option.bind key (Hashtbl.find_opt structured_types) with
        | Some _ as tree -> tree
        | None ->
            Option.bind (get "type" node) (fun ty ->
                Option.bind (string "typeAliasDeclId" ty) alias_type_node))
    | _ -> (
        let direct =
          Option.bind
            (Option.bind (get "id" node) C_import_json.string)
            (Hashtbl.find_opt structured_types)
        in
        match direct with
        | Some _ -> direct
        | None ->
            Option.bind (get "type" node) (fun ty ->
                Option.bind (string "typeAliasDeclId" ty) alias_type_node))
  in
  let rec qualifiers_in_type seen node =
    let own = type_qualifiers node |> List.filter (( <> ) "") in
    let nested =
      match string "kind" node with
      | Some "TypedefType" -> (
          match type_node_id node with
          | Some id when not (List.mem id seen) ->
              Option.fold ~none:[]
                ~some:(qualifiers_in_type (id :: seen))
                (alias_type_node id)
          | _ -> [])
      | _ -> List.concat_map (qualifiers_in_type seen) (children node)
    in
    own @ nested
  in
  let c_qualifiers node =
    Option.fold ~none:[] ~some:(qualifiers_in_type []) (type_tree node)
    |> List.sort_uniq compare
  in
  let parameter_qualifier_words node =
    let words value =
      String.map
        (function ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> c | _ -> ' ')
        value
      |> String.split_on_char ' '
    in
    Option.fold ~none:[]
      ~some:(fun ty ->
        [ "qualType" ]
        |> List.filter_map (fun key -> string key ty)
        |> List.concat_map words)
      (get "type" node)
  in
  let parameter_spelled_qualifiers node =
    parameter_qualifier_words node
    |> List.filter (fun qualifier -> List.mem qualifier [ "const"; "volatile" ])
  in
  let parameter_spells_restrict node =
    parameter_qualifier_words node
    |> List.exists (fun qualifier ->
        List.mem qualifier [ "restrict"; "__restrict"; "__restrict__" ])
  in
  let function_spells_restrict node =
    Option.fold ~none:false
      ~some:(fun function_name ->
        List.exists
          (fun declaration ->
            string "kind" declaration = Some "FunctionDecl"
            && record_name declaration = Some function_name
            && List.exists
                 (fun parameter ->
                   string "kind" parameter = Some "ParmVarDecl"
                   && parameter_spells_restrict parameter)
                 (children declaration))
          nodes)
      (record_name node)
  in
  let parameter_structural_qualifiers node =
    Option.fold ~none:[] ~some:(qualifiers_in_type []) (type_tree node)
  in
  let parameter_probe_qualifiers function_name index =
    Option.fold ~none:[] ~some:(qualifiers_in_type [])
      (Hashtbl.find_opt structured_types
         (Printf.sprintf "parameter:%s:%d" function_name index))
    |> List.filter (fun qualifier ->
        List.mem qualifier [ "restrict"; "__restrict"; "__restrict__" ])
  in
  let rec function_type_nodes node =
    match string "kind" node with
    | Some ("FunctionProtoType" | "FunctionNoProtoType") ->
        Some
          (children node
          |> List.filter (fun child ->
              Option.fold ~none:false
                ~some:(String.ends_with ~suffix:"Type")
                (string "kind" child)))
    | Some
        ( "QualType" | "ElaboratedType" | "ParenType" | "MacroQualifiedType"
        | "AttributedType" | "TypeOfType" | "TypeOfExprType" ) ->
        Option.bind (type_node node) function_type_nodes
    | _ -> None
  in
  let function_qualifiers node parameter_nodes =
    let function_name = Option.value ~default:"" (record_name node) in
    let source_parameters = List.map parameter_spelled_qualifiers parameter_nodes in
    let parameter_qualifiers =
      List.mapi
        (fun index parameter ->
          parameter_structural_qualifiers parameter
          @ parameter_probe_qualifiers function_name index)
        parameter_nodes
    in
    let from_declaration = List.concat parameter_qualifiers in
    match Option.bind (type_tree node) function_type_nodes with
    | Some (result :: parameter_types)
      when List.length parameter_types = List.length parameter_nodes ->
        let parameter_types =
          List.map2
            (fun tree (source, qualifiers) ->
              qualifiers_in_type [] tree @ qualifiers
              |> List.filter (fun qualifier ->
                  qualifier <> "volatile" || List.mem "volatile" source))
            parameter_types
            (List.combine source_parameters parameter_qualifiers)
          |> List.concat
        in
        qualifiers_in_type [] result @ parameter_types @ from_declaration
    | _ -> c_qualifiers node @ from_declaration |> List.sort_uniq compare
  in
  let rec top_level_const seen node =
    match string "kind" node with
    | Some "QualType" ->
        let own_const =
          Option.fold ~none:false
            ~some:(fun value ->
              String.map
                (function
                  | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> c | _ -> ' ')
                value
              |> String.split_on_char ' ' |> List.mem "const")
            (string "qualifiers" node)
        in
        own_const
        || Option.fold ~none:false
             ~some:(fun child ->
               if string "kind" child = Some "ConstantArrayType" then
                 top_level_const seen child
               else false)
             (type_node node)
    | Some "TypedefType" -> (
        match type_node_id node with
        | Some id when not (List.mem id seen) ->
            Option.fold ~none:false
              ~some:(top_level_const (id :: seen))
              (alias_type_node id)
        | _ -> false)
    | Some
        ( "ElaboratedType" | "ParenType" | "MacroQualifiedType" | "AdjustedType"
        | "DecayedType" | "AttributedType" | "TypeOfType" | "TypeOfExprType"
        | "PredefinedSugarType" ) ->
        Option.fold ~none:false ~some:(top_level_const seen) (type_node node)
    | Some "ConstantArrayType" ->
        Option.fold ~none:false ~some:(top_level_const seen) (type_node node)
    | _ -> false
  in
  let top_level_const_node node =
    Option.fold ~none:false ~some:(top_level_const []) (type_tree node)
  in
  let parse_type ?(allow_arrays = false) ~allow_record tree =
    type_result ~allow_arrays ~alias_name ~alias_type_node ~resolve_alias:alias
      ~record_ids ~record_nodes_by_id ~enum_types:enum_id_types ~allow_record tree
  in
  let rec contains_function_type seen node =
    match string "kind" node with
    | Some ("FunctionProtoType" | "FunctionNoProtoType") -> true
    | Some "TypedefType" -> (
        match type_node_id node with
        | Some id when not (List.mem id seen) ->
            Option.fold ~none:false
              ~some:(contains_function_type (id :: seen))
              (alias_type_node id)
        | _ -> false)
    | _ -> List.exists (contains_function_type seen) (children node)
  in
  let rec function_pointer_type seen node =
    match string "kind" node with
    | Some "PointerType" ->
        Option.fold ~none:false ~some:(contains_function_type []) (type_node node)
    | Some "TypedefType" -> (
        match type_node_id node with
        | Some id when not (List.mem id seen) ->
            Option.fold ~none:false
              ~some:(function_pointer_type (id :: seen))
              (alias_type_node id)
        | _ -> false)
    | Some
        ( "QualType" | "ElaboratedType" | "ParenType" | "MacroQualifiedType"
        | "AttributedType" | "TypeOfType" | "TypeOfExprType" | "PredefinedSugarType" )
      ->
        Option.fold ~none:false ~some:(function_pointer_type seen) (type_node node)
    | _ -> false
  in
  let rec record_id_in_type seen node =
    match string "kind" node with
    | Some "RecordType" -> type_node_id node
    | Some "TypedefType" -> (
        match type_node_id node with
        | Some id when not (List.mem id seen) ->
            Option.bind (alias_type_node id) (record_id_in_type (id :: seen))
        | _ -> None)
    | _ -> List.find_map (record_id_in_type seen) (children node)
  in
  let record_decl_id node = Option.bind (type_tree node) (record_id_in_type []) in
  let declaration_location node =
    let loc =
      Option.map
        (fun loc -> Option.value ~default:loc (get "expansionLoc" loc))
        (get "loc" node)
    in
    let loc_value key fallback =
      Option.bind loc (fun loc ->
          match get key loc with Some _ as value -> value | None -> get fallback loc)
    in
    let number = function
      | C_import_json.Num value -> int_of_string_opt value
      | C_import_json.Str value -> int_of_string_opt value
      | _ -> None
    in
    ( (match Option.bind loc (string "presumedFile") with
      | Some _ as file -> file
      | None -> Option.bind loc (string "file")),
      Option.bind (loc_value "presumedLine" "line") number )
  in
  Hashtbl.iter (fun name _ -> ignore (alias name)) alias_nodes;
  let typed_aliases =
    Hashtbl.fold
      (fun name result acc ->
        match result with
        | Ok (Ast.Named_type target) when target = name -> acc
        | Ok ty when not (Names.reserved_binding_name name) -> (name, ty) :: acc
        | Ok _ -> acc
        | Error _ -> acc)
      aliases []
    |> List.sort compare
  in
  let entities = Hashtbl.create 256
  and unsupported = Hashtbl.create 128
  and manifest = Hashtbl.create 256
  and incomplete_arrays = Hashtbl.create 16
  and static_functions = Hashtbl.create 32
  and items = ref [] in
  let add_unsupported name reason = Hashtbl.replace unsupported name reason in
  Hashtbl.iter
    (fun name result ->
      if Names.reserved_binding_name name then
        match result with
        | Ok _ ->
            add_unsupported name
              "name is reserved in Fas; call it through a C container function with \
               another name"
        | Error _ -> ())
    aliases;
  let add_item name spelling signature item origin obligations reason
      ?(normalize_restrict = Option.is_some (find_text spelling "restrict" 0))
      ?(entity_scope = "") ?(keep_unsupported_item = false) () =
    let reason =
      if Names.reserved_binding_name name then
        Some
          "name is reserved in Fas; call it through a C container function with \
           another name"
      else reason
    in
    let item =
      if Option.is_some reason && not keep_unsupported_item then None else item
    in
    let declaration_file, line = origin in
    let obligations =
      if normalize_restrict then
        List.map
          (function "__restrict" | "__restrict__" -> "restrict" | q -> q)
          obligations
      else obligations
    in
    let qualifier_text = List.sort_uniq compare obligations |> String.concat "," in
    let reason_text =
      Option.fold ~none:"" ~some:(fun r -> " unsupported=" ^ r) reason
    in
    let manifest_line =
      Printf.sprintf "%s\t%s\t%s\t%s\t%s:%d%s" name spelling signature qualifier_text
        (Option.value ~default:"<unknown>" declaration_file)
        (Option.value ~default:0 line)
        reason_text
    in
    Hashtbl.replace manifest name manifest_line;
    (match reason with Some reason -> add_unsupported name reason | None -> ());
    let identity_parts =
      [ signature; qualifier_text; Option.value ~default:"" reason ]
      @ if entity_scope = "" then [] else [ entity_scope ]
    in
    let identity = String.concat "\000" identity_parts in
    match Hashtbl.find_opt entities name with
    | Some previous when previous = identity -> ()
    | Some _ ->
        Hashtbl.replace entities name "\000conflict";
        add_unsupported name "conflicting C declarations"
    | None ->
        Hashtbl.add entities name identity;
        Option.iter (fun item -> items := item :: !items) item
  in
  let origin node =
    let file, line = declaration_location node in
    (file, line)
  in
  let declaration_spelling node name =
    match Option.bind (get "type" node) (string "qualType") with
    | Some raw -> raw ^ " " ^ name
    | None ->
        let tag = Option.value ~default:"record" (string "tagUsed" node) in
        tag ^ " " ^ name
  in
  let quals node = c_qualifiers node in
  let as_type ?(allow_arrays = false) ~allow_record node =
    match type_tree node with
    | None -> Error "declaration has no structural C type"
    | Some tree -> parse_type ~allow_arrays ~allow_record tree
  in
  let record_definitions = Hashtbl.create 64 in
  List.iter
    (fun node ->
      if
        string "kind" node = Some "RecordDecl"
        && get "completeDefinition" node = Some (C_import_json.Bool true)
      then
        match Option.bind (get "id" node) C_import_json.string with
        | Some id ->
            Option.iter
              (fun name -> Hashtbl.replace record_definitions name node)
              (Hashtbl.find_opt record_ids id)
        | None -> ())
    nodes;
  let anonymous_record_layout node =
    let loc =
      Option.map
        (fun loc -> Option.value ~default:loc (get "expansionLoc" loc))
        (get "loc" node)
    in
    Option.bind loc (fun loc ->
        let file =
          match string "presumedFile" loc with
          | Some _ as file -> file
          | None -> string "file" loc
        in
        let number = function
          | C_import_json.Num value -> int_of_string_opt value
          | C_import_json.Str value -> int_of_string_opt value
          | _ -> None
        in
        let line =
          Option.bind
            (match get "presumedLine" loc with
            | Some _ as line -> line
            | None -> get "line" loc)
            number
        and col = Option.bind (get "col" loc) number in
        match (file, line, col) with
        | Some file, Some line, Some col ->
            let tag = Option.value ~default:"struct" (string "tagUsed" node) in
            let name = Printf.sprintf "%s (unnamed at %s:%d:%d)" tag file line col in
            let suffix = Printf.sprintf "::(unnamed at %s:%d:%d)" file line col in
            List.find_opt
              (fun layout ->
                layout.layout_name = name || String.ends_with ~suffix layout.layout_name)
              layouts
        | _ -> None)
  in
  let record_layout ?anonymous_record node name =
    let tag = Option.value ~default:"struct" (string "tagUsed" node) in
    let direct = tag ^ " " ^ Option.value ~default:name (record_name node) in
    let base =
      match Option.bind anonymous_record anonymous_record_layout with
      | Some _ as layout -> layout
      | None -> (
          match List.find_opt (fun layout -> layout.layout_name = direct) layouts with
          | Some layout -> Some layout
          | None ->
              let loc =
                Option.map
                  (fun loc -> Option.value ~default:loc (get "expansionLoc" loc))
                  (get "loc" node)
              in
              Option.bind loc (fun loc ->
                  let file =
                    match string "presumedFile" loc with
                    | Some _ as file -> file
                    | None -> string "file" loc
                  in
                  let number = function
                    | C_import_json.Num value -> int_of_string_opt value
                    | _ -> None
                  in
                  let line =
                    Option.bind
                      (match get "presumedLine" loc with
                      | Some _ as line -> line
                      | None -> get "line" loc)
                      number
                  in
                  let col = Option.bind (get "col" loc) number in
                  match (file, line, col) with
                  | Some file, Some line, Some col ->
                      let spellings =
                        [
                          Printf.sprintf "%s %s::(unnamed at %s:%d:%d)" tag name file
                            line col;
                          Printf.sprintf "%s (unnamed at %s:%d:%d)" tag file line col;
                        ]
                      in
                      List.find_opt
                        (fun layout ->
                          List.mem layout.layout_name spellings
                          || String.ends_with
                               ~suffix:
                                 (Printf.sprintf "::(unnamed at %s:%d:%d)" file line col)
                               layout.layout_name)
                        layouts
                  | _ -> None))
    in
    match base with
    | Some layout -> (
        match
          ( Hashtbl.find_opt record_layout_probes name,
            Hashtbl.find_opt typedef_layouts name )
        with
        | Some (size, align, offsets), _ ->
            Some { layout with size; align; direct_offsets = offsets }
        | None, Some (size, align) -> Some { layout with size; align }
        | _ -> Some layout)
    | None -> None
  in
  let layout_member_offset layout field field_index =
    match record_name field with
    | Some name -> (
        match List.assoc_opt name layout.direct_offsets with
        | Some _ as found -> found
        | None -> Option.map snd (List.nth_opt layout.direct_members field_index))
    | None -> Option.map snd (List.nth_opt layout.direct_members field_index)
  in
  let field_storage_type field =
    let rec storage seen count node =
      match string "kind" node with
      | Some "TypedefType" -> (
          match type_node_id node with
          | Some id when not (List.mem id seen) ->
              Option.bind (alias_type_node id) (storage (id :: seen) count)
          | _ -> None)
      | Some "QualType" -> Option.bind (type_node node) (storage seen count)
      | Some "ConstantArrayType" ->
          let size =
            match get "size" node with
            | Some (C_import_json.Num value) -> int_of_string_opt value
            | Some (C_import_json.Str value) -> int_of_string_opt value
            | _ -> None
          in
          Option.bind size (fun size ->
              if size < 0 || (size <> 0 && count > max_int / size) then None
              else Option.bind (type_node node) (storage seen (count * size)))
      | Some "BuiltinType" -> (
          match
            Option.bind (get "type" node) (fun ty ->
                Option.bind (string "qualType" ty) builtin_info_of_name)
          with
          | Some (Floating bits) ->
              let bytes = (bits + 7) / 8 in
              if bytes <> 0 && count > max_int / bytes then None
              else Some (Ast.Array (string_of_int (count * bytes), Ast.Int Ast.U8))
          | _ -> None)
      | _ -> None
    in
    Option.bind (type_tree field) (storage [] 1)
  in
  let source_offset node =
    let loc =
      Option.map
        (fun loc -> Option.value ~default:loc (get "expansionLoc" loc))
        (get "loc" node)
    in
    Option.bind loc (fun loc ->
        Option.bind (get "offset" loc) (function
          | C_import_json.Num value -> int_of_string_opt value
          | C_import_json.Str value -> int_of_string_opt value
          | _ -> None))
  in
  let records_by_name =
    Hashtbl.fold (fun name node acc -> (name, node) :: acc) record_definitions []
    |> List.sort compare
  in
  let raw_records =
    List.map
      (fun (name, node) ->
        let unsupported = ref None and blocked = ref false in
        let reject reason =
          if Option.is_none !unsupported then unsupported := Some reason
        in
        let rec fields ?anonymous_record base record =
          let record_fields =
            children record
            |> List.filter (fun child -> string "kind" child = Some "FieldDecl")
            |> List.mapi (fun index field -> (index, field))
          in
          let layout = record_layout ?anonymous_record record name in
          List.concat_map
            (fun (field_index, field) ->
              let field_name = record_name field in
              if top_level_const_node field then (
                blocked := true;
                [])
              else if get "isBitfield" field = Some (C_import_json.Bool true) then (
                reject "bit-fields are not supported";
                [])
              else
                match field_name with
                | None -> (
                    let nested =
                      match
                        Option.bind (record_decl_id field)
                          (Hashtbl.find_opt record_nodes_by_id)
                      with
                      | Some nested -> Some nested
                      | None ->
                          let offset = source_offset field in
                          children record
                          |> List.find_opt (fun child ->
                              string "kind" child = Some "RecordDecl"
                              && record_name child = None
                              && source_offset child = offset)
                    in
                    match nested with
                    | Some nested when record_name nested = None -> (
                        let nested_layout =
                          Option.bind layout (fun layout ->
                              Option.map
                                (fun offset -> (layout, offset))
                                (layout_member_offset layout field field_index))
                        in
                        match nested_layout with
                        | None ->
                            reject "anonymous member layout is not available";
                            []
                        | Some (_, relative) ->
                            if relative < 0 || base > max_int - relative then (
                              reject "anonymous member layout is not representable";
                              [])
                            else
                              let transparent =
                                string "tagUsed" nested = Some "union"
                                && List.exists
                                     (fun child ->
                                       string "kind" child = Some "TransparentUnionAttr")
                                     (children nested)
                              in
                              if transparent then (
                                reject "transparent unions are not supported";
                                [])
                              else
                                fields ~anonymous_record:nested (base + relative) nested
                        )
                    | _ ->
                        reject "anonymous member type is not supported";
                        [])
                | Some field_name -> (
                    let reason, ty =
                      match type_tree field with
                      | None -> (Some "field has no structural C type", None)
                      | Some tree -> (
                          match
                            parse_type ~allow_arrays:true ~allow_record:true tree
                          with
                          | Ok ty -> (None, Some ty)
                          | Error "floating-point types are not supported" -> (
                              match field_storage_type field with
                              | Some ty ->
                                  ( Some
                                      "floating-point fields are not supported until \
                                       v0.5",
                                    Some ty )
                              | None ->
                                  (Some "floating-point fields are not supported", None)
                              )
                          | Error "arrays of unknown size are not supported" ->
                              (Some "flexible array members are not supported", None)
                          | Error "function pointers are not supported" ->
                              (Some "function-pointer fields are not supported", None)
                          | Error reason -> (Some reason, None))
                    in
                    (match reason with
                    | Some reason
                      when reason
                           <> "floating-point fields are not supported until v0.5" ->
                        reject reason
                    | _ -> ());
                    match
                      ( ty,
                        Option.bind layout (fun layout ->
                            layout_member_offset layout field field_index) )
                    with
                    | Some ty, Some relative
                      when relative >= 0 && base <= max_int - relative ->
                        [
                          {
                            Ast.name = field_name;
                            ty;
                            span;
                            offset = Some (base + relative);
                            unsupported_reason = reason;
                          };
                        ]
                    | Some _, _ ->
                        reject "record field layout is not available";
                        []
                    | None, _ -> []))
            record_fields
        in
        let fields = fields 0 node in
        let field_names = List.map (fun (field : Ast.field) -> field.name) fields in
        if List.length (List.sort_uniq compare field_names) <> List.length field_names
        then unsupported := Some "anonymous member field names collide";
        let reason =
          if
            string "tagUsed" node = Some "union"
            && List.exists
                 (fun child -> string "kind" child = Some "TransparentUnionAttr")
                 (children node)
          then Some "transparent unions are not supported"
          else !unsupported
        in
        (name, node, fields, record_layout node name, reason, !blocked))
      records_by_name
  in
  let rec contains_const_fields = function
    | Ast.Named_type name -> (
        match
          List.find_opt (fun (record, _, _, _, _, _) -> record = name) raw_records
        with
        | Some (_, _, fields, _, _, blocked) ->
            blocked
            || List.exists
                 (fun (field : Ast.field) -> contains_const_fields field.ty)
                 fields
        | None -> false)
    | Ast.Array (_, element) -> contains_const_fields element
    | _ -> false
  in
  let raw_records =
    List.map
      (fun (name, node, fields, layout, reason, blocked) ->
        ( name,
          node,
          fields,
          layout,
          reason,
          blocked
          || List.exists
               (fun (field : Ast.field) -> contains_const_fields field.ty)
               fields ))
      raw_records
  in
  let hir_ty =
    let rec convert = function
      | Ast.Bool -> Some Hir.Bool
      | Ast.Int Ast.U8 -> Some (Hir.Int Hir.U8)
      | Ast.Int U16 -> Some (Hir.Int Hir.U16)
      | Ast.Int U32 -> Some (Hir.Int Hir.U32)
      | Ast.Int U64 -> Some (Hir.Int Hir.U64)
      | Ast.Int I8 -> Some (Hir.Int Hir.I8)
      | Ast.Int I16 -> Some (Hir.Int Hir.I16)
      | Ast.Int I32 -> Some (Hir.Int Hir.I32)
      | Ast.Int I64 -> Some (Hir.Int Hir.I64)
      | Ast.Int Usize -> Some (Hir.Int Hir.Usize)
      | Ast.Int Isize -> Some (Hir.Int Hir.Isize)
      | Ast.Addr -> Some Hir.Addr
      | Ast.Handle (Ast.Named_type name) -> Some (Hir.Handle name)
      | Ast.Named_type name -> Some (Hir.Struct name)
      | Ast.Array (length, ty) ->
          Option.bind (int_of_string_opt length) (fun n ->
              Option.map (fun ty -> Hir.Array (n, ty)) (convert ty))
      | _ -> None
    in
    convert
  in
  let candidate_fields (name, _, fields, _, reason, blocked) =
    if Option.is_some reason || blocked then None
    else
      let fields =
        List.map
          (fun (field : Ast.field) ->
            Option.map (fun ty -> (field.name, ty)) (hir_ty field.ty))
          fields
      in
      if List.exists Option.is_none fields then None
      else Some (name, List.map Option.get fields)
  in
  let field_offsets =
    List.filter_map
      (fun (name, _, fields, _, _, _) ->
        let offsets =
          List.filter_map
            (fun (field : Ast.field) ->
              Option.map (fun offset -> (field.name, offset)) field.offset)
            fields
        in
        if offsets = [] then None else Some (name, offsets))
      raw_records
  and field_reasons =
    List.filter_map
      (fun (name, _, fields, _, _, _) ->
        let reasons =
          List.filter_map
            (fun (field : Ast.field) ->
              Option.map (fun reason -> (field.name, reason)) field.unsupported_reason)
            fields
        in
        if reasons = [] then None else Some (name, reasons))
      raw_records
  in
  let byte_storage = List.map fst field_offsets in
  let union_names =
    List.filter_map
      (fun (name, node, _, _, _, _) ->
        if string "tagUsed" node = Some "union" then Some name else None)
      raw_records
  in
  let alignments =
    List.filter_map
      (fun (name, _, _, layout, reason, blocked) ->
        if Option.is_some reason || blocked then None
        else Option.map (fun layout -> (name, Some layout.align)) layout)
      raw_records
  in
  let struct_sizes =
    List.filter_map
      (fun (name, _, _, layout, reason, blocked) ->
        if Option.is_some reason || blocked then None
        else Option.map (fun layout -> (name, layout.size)) layout)
      raw_records
  in
  let struct_declarations =
    List.map
      (fun (name, fields) ->
        let align = List.assoc_opt name alignments |> Option.join in
        (name, fields, align))
      (List.filter_map candidate_fields raw_records)
  in
  let layouts_cache =
    Hir.struct_layout_cache ~unions:union_names ~field_offsets ~field_reasons
      ~byte_storage ~struct_sizes struct_declarations
  in
  let record_results =
    List.map
      (fun (name, node, fields, layout, reason, blocked) ->
        let reason =
          if Option.is_some reason || blocked then reason
          else
            match (layout, Hir.compute_struct_cached layouts_cache name) with
            | Some clang, Ok fas
              when clang.align > 0
                   && clang.size mod clang.align = 0
                   && clang.size = fas.size && clang.align = fas.align
                   && List.for_all
                        (fun (field : Hir.field) ->
                          let offsets =
                            Option.value ~default:clang.direct_offsets
                              (List.assoc_opt name field_offsets)
                          in
                          List.assoc_opt field.name offsets = Some field.offset)
                        fas.fields ->
                None
            | _ -> Some "record layout differs from C"
        in
        (name, node, fields, reason, blocked, fst (declaration_location node), layout))
      raw_records
  in
  let record_value_reason name =
    match
      List.find_opt (fun (record, _, _, _, _, _, _) -> record = name) record_results
    with
    | Some (_, _, _, _, true, _, _) -> Some "const fields are not supported"
    | Some (_, _, _, Some reason, _, _, _) -> Some reason
    | Some _ -> None
    | None when Hashtbl.mem records name ->
        Some "struct and union values are not supported"
    | None -> None
  in
  let first_record_node name =
    List.filter
      (fun node ->
        string "kind" node = Some "RecordDecl"
        && Option.bind
             (Option.bind (get "id" node) C_import_json.string)
             (fun id -> Hashtbl.find_opt record_ids id)
           = Some name)
      nodes
    |> List.sort (fun left right ->
        compare (declaration_location left) (declaration_location right))
    |> function
    | [] -> None
    | node :: _ -> Some node
  in
  let rec type_value_reason = function
    | Ast.Named_type name -> record_value_reason name
    | Ast.Array (_, element) -> type_value_reason element
    | _ -> None
  in
  let record_types =
    List.concat_map
      (fun (name, _, _, reason, blocked, _, _) ->
        if blocked then [ (name, name, Some "const fields are not supported") ]
        else [ (name, name, reason) ])
      record_results
    @ Hashtbl.fold
        (fun name result acc ->
          match result with
          | Ok (Ast.Named_type target)
            when target <> name && Hashtbl.mem record_definitions target ->
              (name, target, record_value_reason target) :: acc
          | _ -> acc)
        aliases []
  in
  List.iter
    (fun (name, node, fields, reason, blocked, file, c_layout) ->
      let is_union = string "tagUsed" node = Some "union" in
      let kind = if is_union then "union" else "struct" in
      let spelling =
        if Hashtbl.mem anonymous_record_aliases name then name else kind ^ " " ^ name
      in
      let layout =
        if blocked || Option.is_some reason then None
        else
          let align = List.assoc_opt name alignments |> Option.join in
          Some align
      in
      let item, signature =
        match layout with
        | Some align ->
            ( Ast.Struct
                {
                  name;
                  generic_params = [];
                  fields;
                  align;
                  size = Option.map (fun layout -> layout.size) c_layout;
                  is_union;
                  span;
                },
              kind ^ " " ^ name
              ^ (if is_union then
                   Option.fold ~none:""
                     ~some:(fun layout ->
                       Printf.sprintf " size=%d align=%d" layout.size layout.align)
                     c_layout
                 else "")
              ^ " {"
              ^ String.concat ", "
                  (List.map
                     (fun (field : Ast.field) ->
                       let c_field =
                         children node
                         |> List.find_opt (fun child ->
                             string "kind" child = Some "FieldDecl"
                             && record_name child = Some field.name)
                       in
                       let c_type = Option.bind c_field c_type_name in
                       let is_function_pointer =
                         Option.fold ~none:false
                           ~some:(fun field ->
                             Option.fold ~none:false ~some:(function_pointer_type [])
                               (type_tree field))
                           c_field
                       in
                       field.name ^ " " ^ Ast.type_name field.ty
                       ^ (if is_union then " @0" else "")
                       ^ Option.fold ~none:""
                           ~some:(fun raw -> " (C " ^ raw ^ ")")
                           (match (c_type, is_function_pointer) with
                           | Some raw, true -> Some raw
                           | _ -> None))
                     fields)
              ^ "}" )
        | None ->
            ( Ast.Opaque { name; span },
              if blocked then "opaque " ^ name ^ " (const fields are not supported)"
              else "opaque " ^ name )
      in
      let reason = if blocked then Some "const fields are not supported" else reason in
      add_item name spelling signature (Some item)
        ( file,
          Option.bind (get "loc" node) (fun loc ->
              Option.bind (get "line" loc) (function
                | C_import_json.Num n -> int_of_string_opt n
                | _ -> None)) )
        [] reason ~keep_unsupported_item:true ())
    record_results;
  Hashtbl.iter
    (fun name visible ->
      if visible = Some name && not (Hashtbl.mem record_definitions name) then
        match first_record_node name with
        | None -> ()
        | Some node ->
            let item = Ast.Opaque { name; span } in
            add_item name
              (declaration_spelling node name)
              ("opaque " ^ name) (Some item) (origin node) [] None ())
    records;
  let function_type node =
    match type_tree node with
    | None -> Error "function declaration has no structural C type"
    | Some tree -> (
        let tree =
          if string "kind" tree = Some "AttributedType" then
            Option.value ~default:tree (type_node tree)
          else tree
        in
        match string "kind" tree with
        | Some ("FunctionProtoType" | "FunctionNoProtoType") -> (
            let calling_convention = string "cc" tree in
            if
              Option.fold ~none:false
                ~some:(fun cc -> cc <> "cdecl" && cc <> "CC_C")
                calling_convention
            then Error "non-default calling conventions are not supported"
            else if type_has_address_space tree then
              Error "C address spaces are not supported"
            else
              let type_children =
                children tree
                |> List.filter (fun child ->
                    Option.fold ~none:false
                      ~some:(String.ends_with ~suffix:"Type")
                      (string "kind" child))
              in
              match type_children with
              | [] -> Error "function result type is not available"
              | result_node :: parameter_nodes -> (
                  match parse_type ~allow_record:false result_node with
                  | Error reason -> Error reason
                  | Ok result ->
                      let rec parse_parameters acc = function
                        | [] -> Ok (List.rev acc)
                        | parameter :: rest -> (
                            match parse_type ~allow_record:false parameter with
                            | Error reason -> Error reason
                            | Ok ty -> parse_parameters ((ty, []) :: acc) rest)
                      in
                      Result.map
                        (fun parameters ->
                          let variadic =
                            get "variadic" tree = Some (C_import_json.Bool true)
                          in
                          (result, parameters, variadic))
                        (parse_parameters [] parameter_nodes)))
        | _ -> Error "function declaration has no structural function type")
  in
  Hashtbl.iter
    (fun name result ->
      match result with
      | Ok ty ->
          if not (Hashtbl.mem alias_nodes name) then
            let signature = "enum " ^ name ^ " as " ^ Ast.type_name ty in
            add_item name ("enum " ^ name) signature None (None, None) [] None ()
      | Error reason -> add_unsupported name reason)
    enums;
  let enum_aliases =
    Hashtbl.fold
      (fun name result acc ->
        match result with
        | Ok ty
          when (not (List.mem_assoc name typed_aliases))
               && not (Names.reserved_binding_name name) ->
            (name, ty) :: acc
        | _ -> acc)
      enums []
  in
  List.iter
    (fun node ->
      let name = Option.value ~default:"" (string "name" node) in
      let kind = string "kind" node in
      match (kind, name) with
      | Some "FasIntegerMacro", name when name <> "" -> (
          let macro_code =
            Option.bind (string "macroTypeCode" node) int_of_string_opt
          in
          let value = string "value" node in
          match (Option.bind macro_code macro_type, value) with
          | Some (_, bits, unsigned), Some value ->
              let ty =
                match (bits, unsigned, macro_code) with
                | _, _, Some 11 -> Some Ast.Bool
                | 8, false, _ -> Some (Ast.Int Ast.I8)
                | 8, true, _ -> Some (Ast.Int Ast.U8)
                | 16, false, _ -> Some (Ast.Int Ast.I16)
                | 16, true, _ -> Some (Ast.Int Ast.U16)
                | 32, false, _ -> Some (Ast.Int Ast.I32)
                | 32, true, _ -> Some (Ast.Int Ast.U32)
                | 64, false, _ -> Some (Ast.Int Ast.I64)
                | 64, true, _ -> Some (Ast.Int Ast.U64)
                | _ -> None
              in
              Option.iter
                (fun ty ->
                  let macro_span = span in
                  let expression =
                    if String.starts_with ~prefix:"-" value then
                      Ast.Unary
                        ( Ast.Neg,
                          Ast.Int_lit
                            (String.sub value 1 (String.length value - 1), macro_span),
                          macro_span )
                    else Ast.Int_lit (value, macro_span)
                  in
                  add_item name ("macro " ^ name)
                    ("macro " ^ Ast.type_name ty ^ " " ^ value)
                    (Some
                       (Ast.Const { name; ty; value = expression; span = macro_span }))
                    (None, None) [] None ())
                ty
          | _ -> ())
      | Some "RecordDecl", _ -> ()
      | Some "EnumDecl", _ ->
          let previous_value = ref None in
          List.iter
            (fun child ->
              if
                string "kind" child = Some "EnumConstantDecl"
                && not
                     (List.mem
                        (Option.value ~default:"" (record_name child))
                        macro_shadow_names)
              then
                match (string "name" child, type_tree child) with
                | Some constant, Some _ -> (
                    let constant_type = as_type ~allow_record:false child in
                    let explicit_value =
                      let rec find_value = function
                        | [] -> None
                        | node :: rest -> (
                            match string "value" node with
                            | Some value -> Some value
                            | None -> find_value (children node @ rest))
                      in
                      find_value (children child)
                    in
                    let value =
                      match explicit_value with
                      | Some value -> Some value
                      | None ->
                          Some
                            (Option.fold ~none:"0" ~some:add_one_decimal !previous_value)
                    in
                    previous_value := value;
                    match (constant_type, value) with
                    | Ok (Ast.Int _ as ty), Some value ->
                        let expression =
                          if String.starts_with ~prefix:"-" value then
                            Ast.Unary
                              ( Ast.Neg,
                                Ast.Int_lit
                                  (String.sub value 1 (String.length value - 1), span),
                                span )
                          else Ast.Int_lit (value, span)
                        in
                        let item =
                          Ast.Const { name = constant; ty; value = expression; span }
                        in
                        add_item constant
                          (declaration_spelling child constant)
                          (Ast.type_name ty ^ " " ^ value)
                          (Some item) (origin child) (quals child) None ()
                    | Ok _, _ ->
                        add_unsupported constant "enum constant type is not an integer"
                    | Error reason, _ -> add_unsupported constant reason)
                | _ -> ())
            (children node)
      | Some "TypedefDecl", _ -> (
          match Hashtbl.find_opt aliases name with
          | Some (Ok (Ast.Named_type target)) when target = name -> ()
          | Some (Ok ty) ->
              add_item name
                (declaration_spelling node name)
                ("typedef " ^ Ast.type_name ty)
                None (origin node) (quals node) None ()
          | Some (Error reason) ->
              add_item name
                (declaration_spelling node name)
                "typedef" None (origin node) (quals node) (Some reason) ()
          | None -> ())
      | Some "FunctionDecl", _
        when get "isImplicit" node = Some (C_import_json.Bool true) ->
          ()
      | Some "FunctionDecl", _ ->
          let origin = origin node in
          let is_static = string "storageClass" node = Some "static" in
          let parameter_nodes =
            children node
            |> List.filter (fun child -> string "kind" child = Some "ParmVarDecl")
          in
          let function_info = function_type node in
          let variadic =
            match function_info with
            | Ok (_, _, variadic) -> variadic
            | Error _ -> false
          in
          let parameters =
            match function_info with
            | Ok (_, parameter_types, _)
              when List.length parameter_nodes = List.length parameter_types ->
                List.mapi
                  (fun index (parameter, (ty, qualifiers)) ->
                    ( Option.value
                        ~default:("arg" ^ string_of_int index)
                        (string "name" parameter),
                      Ok ty,
                      qualifiers ))
                  (List.combine parameter_nodes parameter_types)
            | _ -> []
          in
          let signature =
            match function_info with
            | Error reason -> Error reason
            | Ok (_, _, _) when List.length parameter_nodes <> List.length parameters ->
                Error "function parameter types are not available"
            | Ok (ret, _, _) ->
                let rec values acc = function
                  | [] -> Ok (List.rev acc)
                  | (name, Ok ty, _) :: rest -> values ((name, ty) :: acc) rest
                  | (_, Error reason, _) :: _ -> Error reason
                in
                Result.map (fun params -> (params, ret)) (values [] parameters)
          in
          let signature_name =
            match signature with
            | Ok (params, ret) ->
                "fn("
                ^ String.concat "," (List.map (fun (_, ty) -> Ast.type_name ty) params)
                ^ (if variadic then ",..." else "")
                ^ ")->" ^ Ast.type_name ret
            | Error reason -> "unsupported: " ^ reason
          in
          let reason =
            match signature with Ok _ -> None | Error reason -> Some reason
          in
          (match (is_static, signature, c_type_name node, type_tree node) with
          | true, Ok (_, ret), Some c_signature, Some function_tree -> (
              match String.index_opt c_signature '(' with
              | Some open_paren ->
                  let parameter_types =
                    parameter_nodes
                    |> List.filter_map (fun child ->
                        Option.bind (get "type" child) (string "qualType"))
                  in
                  let result_type =
                    match
                      children function_tree
                      |> List.filter (fun child ->
                          Option.fold ~none:false
                            ~some:(String.ends_with ~suffix:"Type")
                            (string "kind" child))
                    with
                    | result :: _ -> Some result
                    | [] -> None
                  in
                  Hashtbl.replace static_functions name
                    {
                      name;
                      return_type =
                        c_function_result c_signature
                          (String.sub c_signature 0 open_paren);
                      parameter_types;
                      variadic;
                      void_result = ret = Ast.Void;
                      return_function_pointer =
                        Option.fold ~none:false ~some:(function_pointer_type [])
                          result_type;
                      signature = signature_name;
                      span;
                    }
              | None -> ())
          | _ -> ());
          let item =
            match (signature, reason) with
            | Ok (params, ret), None ->
                Some
                  (Ast.Func
                     {
                       name;
                       params =
                         List.mapi
                           (fun index (_, ty) ->
                             ({ Ast.name = "arg" ^ string_of_int index; ty; span }
                               : Ast.param))
                           params;
                       ret;
                       body = Ast.Declaration;
                       linkage = Ast.External_c;
                       variadic;
                       generic_params = [];
                       span;
                     })
            | _ -> None
          in
          add_item name
            (declaration_spelling node name)
            signature_name item origin
            (function_qualifiers node parameter_nodes
            @ List.concat_map (fun (_, _, q) -> q) parameters)
            reason
            ~normalize_restrict:(function_spells_restrict node)
            ~entity_scope:(if is_static then span.Span.file else "")
            ()
      | Some "VarDecl", _ ->
          let ty =
            match as_type ~allow_arrays:true ~allow_record:true node with
            | Error "anonymous records are not supported" ->
                Error "struct and union values are not supported"
            | result -> result
          in
          let rec incomplete_element seen tree =
            match string "kind" tree with
            | Some "IncompleteArrayType" ->
                Option.bind (type_node tree) (fun element ->
                    match parse_type ~allow_arrays:true ~allow_record:true element with
                    | Ok ty -> Some ty
                    | Error _ -> None)
            | Some "TypedefType" -> (
                match type_node_id tree with
                | Some id when not (List.mem id seen) ->
                    Option.bind (alias_type_node id) (incomplete_element (id :: seen))
                | _ -> None)
            | Some
                ( "QualType" | "ElaboratedType" | "ParenType" | "MacroQualifiedType"
                | "AttributedType" | "TypeOfType" | "TypeOfExprType"
                | "PredefinedSugarType" ) ->
                Option.bind (type_node tree) (incomplete_element seen)
            | _ -> None
          in
          let incomplete_element =
            Option.bind (type_tree node) (incomplete_element [])
          in
          Option.iter (Hashtbl.replace incomplete_arrays name) incomplete_element;
          let storage = string "storageClass" node in
          let reason =
            if storage = Some "static" then
              Some "static C globals are not externally visible"
            else
              match ty with
              | Error reason -> Some reason
              | Ok ty -> type_value_reason ty
          in
          let item =
            match (ty, reason) with
            | Ok ty, None ->
                Some
                  (Ast.Global
                     {
                       name;
                       ty;
                       init = None;
                       linkage =
                         (if top_level_const_node node then Ast.Import_const_c
                          else Ast.Import_c);
                       span;
                     })
            | _ -> None
          in
          let signature =
            match ty with
            | Ok ty -> Ast.type_name ty
            | Error reason -> "unsupported: " ^ reason
          in
          add_item name
            (declaration_spelling node name)
            signature item (origin node) (quals node) reason ()
      | _ -> ())
    nodes;
  let items =
    List.sort
      (fun left right -> String.compare (item_name left) (item_name right))
      (List.filter
         (fun item -> Hashtbl.find_opt entities (item_name item) <> Some "\000conflict")
         !items)
  in
  let identities =
    Hashtbl.fold (fun name identity acc -> (name, identity) :: acc) entities []
  in
  {
    records =
      Hashtbl.fold
        (fun name visible acc ->
          if visible <> Some name then acc
          else
            let node =
              match Hashtbl.find_opt record_definitions name with
              | Some node -> Some node
              | None -> first_record_node name
            in
            match node with
            | None -> acc
            | Some node ->
                let spelling =
                  if Hashtbl.mem anonymous_record_aliases name then name
                  else
                    Option.value ~default:"struct" (string "tagUsed" node) ^ " " ^ name
                in
                let origin =
                  if Hashtbl.mem anonymous_record_aliases name then
                    match
                      List.find_opt
                        (fun node ->
                          string "kind" node = Some "TypedefDecl"
                          && string "name" node = Some name)
                        nodes
                    with
                    | Some alias -> (
                        match string "fasHeader" alias with
                        | Some header -> Some header
                        | None -> fst (declaration_location node))
                    | None -> fst (declaration_location node)
                  else
                    match string "fasHeader" node with
                    | Some header -> Some header
                    | None -> fst (declaration_location node)
                in
                (name, spelling, origin) :: acc)
        records []
      |> List.sort compare;
    record_types;
    incomplete_arrays =
      Hashtbl.fold (fun name ty acc -> (name, ty) :: acc) incomplete_arrays [];
    container_names =
      (if container then
         List.map item_name items
         @ Hashtbl.fold (fun name _ acc -> name :: acc) unsupported []
         |> List.sort_uniq String.compare
       else []);
    items;
    aliases =
      List.filter
        (fun (name, _) -> Hashtbl.find_opt entities name <> Some "\000conflict")
        (List.sort compare (typed_aliases @ enum_aliases));
    unsupported =
      Hashtbl.fold (fun name reason acc -> (name, reason) :: acc) unsupported []
      |> List.sort compare;
    identities;
    manifest =
      Hashtbl.fold (fun _ line acc -> line :: acc) manifest [] |> List.sort compare;
    static_functions =
      Hashtbl.fold (fun _ static acc -> static :: acc) static_functions []
      |> List.sort (fun a b -> String.compare a.name b.name);
  }

let merge_imports mappings =
  let identities = Hashtbl.create 256 in
  List.iter
    (fun (name, identity) ->
      match Hashtbl.find_opt identities name with
      | Some previous when previous <> identity ->
          Hashtbl.replace identities name "\000conflict"
      | Some _ -> ()
      | None -> Hashtbl.add identities name identity)
    (List.concat_map (fun mapping -> mapping.identities) mappings);
  let bad name = Hashtbl.find_opt identities name = Some "\000conflict" in
  let items =
    List.concat_map (fun mapping -> mapping.items) mappings
    |> List.filter (fun item -> not (bad (item_name item)))
    |> List.sort_uniq (fun left right ->
        String.compare (item_name left) (item_name right))
  in
  let aliases =
    List.concat_map (fun mapping -> mapping.aliases) mappings
    |> List.sort_uniq compare
    |> List.filter (fun (name, _) -> not (bad name))
  in
  let unsupported =
    List.concat_map (fun mapping -> mapping.unsupported) mappings
    |> List.filter (fun (name, _) -> not (bad name))
    |> fun entries ->
    entries
    @ Hashtbl.fold
        (fun name identity acc ->
          if identity = "\000conflict" then (name, "conflicting C declarations") :: acc
          else acc)
        identities []
    |> List.sort_uniq compare
  in
  {
    items;
    aliases;
    records =
      List.concat_map (fun mapping -> mapping.records) mappings
      |> List.sort_uniq compare;
    record_types =
      List.concat_map (fun mapping -> mapping.record_types) mappings
      |> List.sort_uniq compare
      |> List.filter (fun (name, _, _) -> not (bad name));
    incomplete_arrays =
      List.concat_map (fun mapping -> mapping.incomplete_arrays) mappings
      |> List.sort_uniq compare
      |> List.filter (fun (name, _) -> not (bad name));
    container_names =
      List.concat_map (fun mapping -> mapping.container_names) mappings
      |> List.sort_uniq String.compare;
    unsupported;
    identities = [];
    manifest = List.concat_map (fun mapping -> mapping.manifest) mappings;
    static_functions =
      List.concat_map (fun mapping -> mapping.static_functions) mappings;
  }

let canonical_type aliases ty =
  let rec canonical = function
    | Ast.Named_type name as ty ->
        Option.value ~default:ty (List.assoc_opt name aliases)
    | Ast.Handle inner -> Ast.Handle (canonical inner)
    | Ast.Array (length, inner) -> Ast.Array (length, canonical inner)
    | Ast.Vec (length, inner) -> Ast.Vec (length, canonical inner)
    | ty -> ty
  in
  canonical ty

let c_signature aliases = function
  | Ast.Func { params; ret; variadic; _ } ->
      "fn("
      ^ String.concat ","
          (List.map
             (fun (p : Ast.param) -> Ast.type_name (canonical_type aliases p.ty))
             params)
      ^ (if variadic then ",..." else "")
      ^ ")->"
      ^ Ast.type_name (canonical_type aliases ret)
  | Ast.Global { ty; linkage; _ } ->
      (if linkage = Ast.Import_const_c then "const " else "")
      ^ Ast.type_name (canonical_type aliases ty)
  | _ -> "unsupported C declaration"

let source_signature aliases = function
  | Ast.Func { linkage = Ast.External_c; generic_params = []; _ } as item ->
      Some (c_signature aliases item)
  | Ast.Global { linkage = Ast.Import_c; init = None; _ } as item ->
      Some (c_signature aliases item)
  | Ast.Global { linkage = Ast.Export_c; init = Some _; _ } as item ->
      Some (c_signature aliases item)
  | _ -> None

let reconcile_source ?(container_mismatch_to_clang = false) source_items imported =
  let confirmed = ref [] in
  let bindings = List.map (fun item -> (item_name item, item)) imported.items in
  let check item =
    let name = item_name item in
    let binding = List.assoc_opt name bindings in
    if
      name = ""
      || binding = None
         && not
              (List.mem_assoc name imported.aliases
              || List.mem_assoc name imported.unsupported)
    then None
    else
      let duplicate () =
        Diag.error (Ast.item_span item)
          (Printf.sprintf "duplicate declaration `%s`" name)
      in
      match (item, binding, source_signature imported.aliases item) with
      | _, Some foreign, Some native ->
          let actual = c_signature imported.aliases foreign in
          if actual = native then (
            confirmed := name :: !confirmed;
            None)
          else if container_mismatch_to_clang && List.mem name imported.container_names
          then (
            confirmed := name :: !confirmed;
            None)
          else
            Some
              (Diag.error (Ast.item_span item)
                 (Printf.sprintf
                    "C declaration `%s` has type `%s`, but Fas declares `%s`" name
                    actual native))
      | _, None, Some _ when List.mem_assoc name imported.incomplete_arrays -> (
          let element = List.assoc name imported.incomplete_arrays in
          match item with
          | Ast.Global
              { linkage = Ast.Export_c; init = Some _; ty = Ast.Array (_, actual); _ }
            when canonical_type imported.aliases actual
                 = canonical_type imported.aliases element ->
              confirmed := name :: !confirmed;
              None
          | Ast.Global
              { linkage = Ast.Export_c; init = Some _; ty = Ast.Array (_, actual); _ }
            ->
              Some
                (Diag.error (Ast.item_span item)
                   (Printf.sprintf
                      "C declaration `%s` has type `arr[?, %s]`, but Fas declares `%s`"
                      name (Ast.type_name element)
                      (Ast.type_name (Ast.Array ("?", actual)))))
          | _ -> Some (duplicate ()))
      | _ -> Some (duplicate ())
  in
  match List.find_map check source_items with
  | Some diagnostic -> Error [ diagnostic ]
  | None ->
      Ok
        {
          imported with
          items =
            List.filter
              (fun item -> not (List.mem (item_name item) !confirmed))
              imported.items;
          unsupported =
            List.filter
              (fun (name, _) -> not (List.mem name !confirmed))
              imported.unsupported;
          manifest =
            List.filter
              (fun line ->
                match String.index_opt line '\t' with
                | None -> true
                | Some stop ->
                    not
                      (List.mem (String.sub line 0 stop) !confirmed
                      && List.mem_assoc (String.sub line 0 stop)
                           imported.incomplete_arrays))
              imported.manifest;
          incomplete_arrays =
            List.filter
              (fun (name, _) -> not (List.mem name !confirmed))
              imported.incomplete_arrays;
        }

let manifest_text imported =
  let rows = Hashtbl.create 256 in
  List.sort compare imported.manifest
  |> List.iter (fun line ->
      match String.index_opt line '\t' with
      | None -> ()
      | Some stop ->
          let name = String.sub line 0 stop in
          if not (Hashtbl.mem rows name) then Hashtbl.add rows name line);
  let lines =
    Hashtbl.fold
      (fun name line acc ->
        match String.split_on_char '\t' line with
        | _ :: spelling :: signature :: qualifiers :: location :: _ ->
            let location =
              match find_text location " unsupported=" 0 with
              | Some stop -> String.sub location 0 stop
              | None -> location
            in
            let reason =
              Option.fold ~none:""
                ~some:(fun value -> " unsupported=" ^ value)
                (List.assoc_opt name imported.unsupported)
            in
            Printf.sprintf "%s\t%s\t%s\t%s\t%s%s" name spelling signature qualifiers
              location reason
            :: acc
        | _ -> acc)
      rows []
  in
  let lines =
    lines
    @ List.filter_map
        (fun (name, reason) ->
          if Hashtbl.mem rows name then None
          else
            Some
              (Printf.sprintf "%s\t\tunsupported\t\t<unknown>:0 unsupported=%s" name
                 reason))
        imported.unsupported
    |> List.sort compare
  in
  if lines = [] then "" else String.concat "\n" lines ^ "\n"
