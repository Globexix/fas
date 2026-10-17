type document = { uri : string; text : string }
type snapshot = { index : Sema_index.t; texts : (string, string) Hashtbl.t }

exception Invalid of string

type state = {
  options : Cli.lsp_options;
  cache : C_import.cache;
  documents : (string, document) Hashtbl.t;
  contributions : (string, (string * Json.t) list) Hashtbl.t;
  loads : (string, string list) Hashtbl.t;
  snapshots : (string, snapshot) Hashtbl.t;
  c_roots : (string, Sema_index.t) Hashtbl.t;
  mutable initialized : bool;
  mutable shut_down : bool;
  mutable exit_code : int option;
}

let create options =
  {
    options;
    cache = C_import.create_cache ();
    documents = Hashtbl.create 8;
    contributions = Hashtbl.create 8;
    loads = Hashtbl.create 8;
    snapshots = Hashtbl.create 8;
    c_roots = Hashtbl.create 8;
    initialized = false;
    shut_down = false;
    exit_code = None;
  }

let send json = Lsp_frame.write_message stdout (Json.to_string json)
let envelope fields = Json.Obj (("jsonrpc", Json.Str "2.0") :: fields)
let respond id result = send (envelope [ ("id", id); ("result", result) ])

let fail id code message =
  send
    (envelope
       [
         ("id", id);
         ( "error",
           Json.Obj
             [ ("code", Json.Num (string_of_int code)); ("message", Json.Str message) ]
         );
       ])

let notify meth params =
  send (envelope [ ("method", Json.Str meth); ("params", params) ])

let log message =
  notify "window/logMessage"
    (Json.Obj [ ("type", Json.Num "1"); ("message", Json.Str message) ])

let member key = function Json.Obj fields -> List.assoc_opt key fields | _ -> None
let child key value = match member key value with Some v -> v | None -> Json.Null

let string_member key value =
  match member key value with Some (Json.Str s) -> Some s | _ -> None

let hex_value c =
  match c with
  | '0' .. '9' -> Some (Char.code c - 48)
  | 'a' .. 'f' -> Some (Char.code c - 87)
  | 'A' .. 'F' -> Some (Char.code c - 55)
  | _ -> None

let percent_decode text =
  let n = String.length text in
  let buffer = Buffer.create n in
  let rec go i =
    if i >= n then Some (Buffer.contents buffer)
    else if text.[i] <> '%' then (
      Buffer.add_char buffer text.[i];
      go (i + 1))
    else if i + 2 >= n then None
    else
      match (hex_value text.[i + 1], hex_value text.[i + 2]) with
      | Some high, Some low ->
          Buffer.add_char buffer (Char.chr ((high * 16) + low));
          go (i + 3)
      | _ -> None
  in
  go 0

let uri_to_path uri =
  let prefix = "file://" in
  let plen = String.length prefix in
  if String.length uri > plen && String.sub uri 0 plen = prefix then
    let rest = String.sub uri plen (String.length uri - plen) in
    if rest.[0] = '/' then percent_decode rest else None
  else None

let path_to_uri path =
  let buffer = Buffer.create (String.length path + 8) in
  Buffer.add_string buffer "file://";
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' | '/' ->
          Buffer.add_char buffer c
      | _ -> Buffer.add_string buffer (Printf.sprintf "%%%02X" (Char.code c)))
    path;
  Buffer.contents buffer

let key_of_uri uri =
  match uri_to_path uri with
  | Some path when Filename.check_suffix path ".fas" ->
      Some (Driver.canonical_path path)
  | _ -> None

let read_text path =
  try In_channel.with_open_bin path In_channel.input_all with Sys_error _ -> ""

let overlay state path =
  match Hashtbl.find_opt state.documents path with
  | Some document -> Some document.text
  | None -> None

let disk_text path =
  try
    if Sys.is_directory path then None
    else Some (In_channel.with_open_bin path In_channel.input_all)
  with Sys_error _ -> None

let current_text state key =
  match Hashtbl.find_opt state.documents key with
  | Some document -> Some document.text
  | None -> disk_text key

let config_for state key : Cli.t =
  {
    Cli.input = key;
    output = key;
    output_explicit = false;
    emit = Cli.Ir;
    keep = false;
    optimization = 2;
    debug = false;
    no_inline_function = None;
    sanitizers = [];
    link_inputs = [];
    c_flags = state.options.lsp_c_flags;
    diagnostics = Cli.Text;
    target_cpu = None;
    target = state.options.lsp_target;
    target_features = None;
  }

let lsp_message (diag : Diag.t) =
  let help = match diag.help with None -> [] | Some text -> [ "help: " ^ text ] in
  let notes = List.map (fun note -> "note: " ^ note) diag.notes in
  String.concat "\n" ((diag.message :: help) @ notes)

let position_json (line, character) =
  Json.Obj
    [
      ("line", Json.Num (string_of_int line));
      ("character", Json.Num (string_of_int character));
    ]

let diagnostic_json text (span : Span.t) message =
  let zero = (0, 0) in
  let start, finish =
    if Span.is_synthetic span then (zero, zero)
    else
      ( Lsp_position.position_of_offset text span.start_offset,
        Lsp_position.position_of_offset text span.end_offset )
  in
  Json.Obj
    [
      ( "range",
        Json.Obj [ ("start", position_json start); ("end", position_json finish) ] );
      ("severity", Json.Num "1");
      ("source", Json.Str "fas");
      ("message", Json.Str message);
    ]

let target state (root : document) (span : Span.t) =
  if Span.is_synthetic span then (root.uri, "")
  else
    let canonical = Driver.canonical_path span.file in
    match Hashtbl.find_opt state.documents canonical with
    | Some document -> (document.uri, document.text)
    | None ->
        if Sys.file_exists canonical && not (Sys.is_directory canonical) then
          (path_to_uri canonical, read_text canonical)
        else (root.uri, "")

let contributions_of state root diags =
  List.map
    (fun (diag : Diag.t) ->
      let uri, text = target state root diag.primary in
      (uri, diagnostic_json text diag.primary (lsp_message diag)))
    diags

let publish state uris =
  let roots =
    List.sort
      (fun (a, _) (b, _) -> String.compare a b)
      (Hashtbl.fold (fun key value acc -> (key, value) :: acc) state.contributions [])
  in
  List.iter
    (fun uri ->
      let diagnostics =
        List.fold_left
          (fun acc (_, fresh) ->
            List.fold_left
              (fun acc (target_uri, diagnostic) ->
                if target_uri = uri && not (List.mem diagnostic acc) then
                  diagnostic :: acc
                else acc)
              acc fresh)
          [] roots
      in
      notify "textDocument/publishDiagnostics"
        (Json.Obj
           [ ("uri", Json.Str uri); ("diagnostics", Json.Arr (List.rev diagnostics)) ]))
    (List.sort_uniq String.compare uris)

let previous_uris state key =
  match Hashtbl.find_opt state.contributions key with
  | Some fresh -> List.map fst fresh
  | None -> []

let analyze_root state key =
  match Hashtbl.find_opt state.documents key with
  | None -> []
  | Some root ->
      let previous = previous_uris state key in
      let loaded = ref [ key ] in
      let texts = Hashtbl.create 8 in
      let observe path =
        match Hashtbl.find_opt texts path with
        | Some text -> Some text
        | None ->
            loaded := path :: !loaded;
            let text =
              match overlay state path with
              | Some text -> Some text
              | None -> disk_text path
            in
            Option.iter (Hashtbl.replace texts path) text;
            text
      in
      let index = Sema_index.create () in
      let diags =
        match
          Driver.check ~overlay:observe ~cache:state.cache ~index (config_for state key)
        with
        | Ok checked ->
            Driver.release checked;
            Hashtbl.replace texts key root.text;
            Hashtbl.replace state.snapshots key { index; texts };
            []
        | Error diags -> diags
      in
      if Sema_index.has_c_import index then Hashtbl.replace state.c_roots key index
      else Hashtbl.remove state.c_roots key;
      let fresh = contributions_of state root diags in
      Hashtbl.replace state.contributions key fresh;
      Hashtbl.replace state.loads key (List.sort_uniq String.compare !loaded);
      (root.uri :: previous) @ List.map fst fresh

let dependants state path =
  Hashtbl.fold
    (fun key _ acc ->
      let loaded = Option.value ~default:[] (Hashtbl.find_opt state.loads key) in
      if key = path || List.mem path loaded then key :: acc else acc)
    state.documents []

let refresh state extra keys =
  let uris = List.concat_map (analyze_root state) (List.sort String.compare keys) in
  publish state (extra @ uris)

let open_document state params =
  let document = child "textDocument" params in
  match (string_member "uri" document, string_member "text" document) with
  | Some uri, Some text -> (
      match key_of_uri uri with
      | Some key ->
          Hashtbl.replace state.documents key { uri; text };
          refresh state [] (dependants state key)
      | None -> ())
  | _ -> ()

let change_document state params =
  let text =
    match member "contentChanges" params with
    | Some (Json.Arr changes) ->
        List.fold_left
          (fun acc change ->
            match string_member "text" change with
            | Some text -> Some text
            | None -> acc)
          None changes
    | _ -> None
  in
  match (string_member "uri" (child "textDocument" params), text) with
  | Some uri, Some text -> (
      match key_of_uri uri with
      | Some key when Hashtbl.mem state.documents key ->
          let document = Hashtbl.find state.documents key in
          Hashtbl.replace state.documents key { document with text };
          refresh state [] (dependants state key)
      | _ -> ())
  | _ -> ()

let save_document state params =
  match string_member "uri" (child "textDocument" params) with
  | Some uri -> (
      match key_of_uri uri with
      | Some key -> refresh state [] (dependants state key)
      | None -> ())
  | None -> ()

let close_document state params =
  match string_member "uri" (child "textDocument" params) with
  | Some uri -> (
      match key_of_uri uri with
      | Some key -> (
          match Hashtbl.find_opt state.documents key with
          | None -> ()
          | Some document ->
              let previous = previous_uris state key in
              Hashtbl.remove state.documents key;
              Hashtbl.remove state.contributions key;
              Hashtbl.remove state.loads key;
              Hashtbl.remove state.snapshots key;
              Hashtbl.remove state.c_roots key;
              refresh state (document.uri :: previous) (dependants state key))
      | None -> ())
  | None -> ()

let invalid message = raise (Invalid message)

let document_uri params =
  match string_member "uri" (child "textDocument" params) with
  | Some uri -> uri
  | None -> invalid "params.textDocument.uri must be a string"

let index_member name value =
  match member name value with
  | Some (Json.Num text) -> (
      match int_of_string_opt text with Some n when n >= 0 -> Some n | _ -> None)
  | _ -> None

let request_point params =
  let uri = document_uri params in
  let position = child "position" params in
  match (index_member "line" position, index_member "character" position) with
  | Some line, Some character -> (uri, line, character)
  | _ -> invalid "params.position must hold non-negative integer line and character"

let include_declaration params =
  match member "includeDeclaration" (child "context" params) with
  | Some (Json.Bool flag) -> flag
  | _ -> invalid "params.context.includeDeclaration must be a boolean"

let snapshot_for state key =
  match Hashtbl.find_opt state.snapshots key with
  | Some snapshot -> Some snapshot
  | None ->
      let roots = Hashtbl.fold (fun root _ acc -> root :: acc) state.snapshots [] in
      List.find_map
        (fun root ->
          let snapshot = Hashtbl.find state.snapshots root in
          if Hashtbl.mem snapshot.texts key then Some snapshot else None)
        (List.sort String.compare roots)

let mapped state snapshot (span : Span.t) =
  let key = Driver.canonical_path span.file in
  match (Hashtbl.find_opt snapshot.texts key, current_text state key) with
  | Some analyzed, Some current -> (
      let map = Lsp_map.create ~analyzed ~current in
      let first = Lsp_position.position_of_offset analyzed span.start_offset in
      let last = Lsp_position.position_of_offset analyzed span.end_offset in
      match (Lsp_map.to_current map (fst first), Lsp_map.to_current map (fst last)) with
      | Some first_line, Some last_line ->
          Some (key, (first_line, snd first), (last_line, snd last))
      | _ -> None)
  | _ -> None

let uri_of_key state key =
  match Hashtbl.find_opt state.documents key with
  | Some document -> document.uri
  | None -> path_to_uri key

let range_json first last =
  Json.Obj [ ("start", position_json first); ("end", position_json last) ]

let resolve state uri line character =
  match key_of_uri uri with
  | None -> None
  | Some key -> (
      match (snapshot_for state key, current_text state key) with
      | Some snapshot, Some current -> (
          match Hashtbl.find_opt snapshot.texts key with
          | None -> None
          | Some analyzed -> (
              let map = Lsp_map.create ~analyzed ~current in
              match Lsp_map.to_analyzed map line with
              | None -> None
              | Some analyzed_line ->
                  Some
                    ( snapshot,
                      key,
                      Lsp_position.offset_of_position analyzed (analyzed_line, character)
                    )))
      | _ -> None)

let target (snapshot : snapshot) key offset =
  let spans =
    List.map
      (fun (r : Sema_index.reference) ->
        (r.Sema_index.use_span, r.Sema_index.declaration))
      (Sema_index.references snapshot.index)
    @ List.map
        (fun (d : Sema_index.declaration) -> (d.Sema_index.name_span, d))
        (Sema_index.declarations snapshot.index)
  in
  let spans =
    List.filter
      (fun ((span : Span.t), _) ->
        (not (Span.is_synthetic span)) && Driver.canonical_path span.file = key)
      spans
  in
  let contains ((span : Span.t), _) =
    span.start_offset <= offset && offset < span.end_offset
  in
  let ends_at ((span : Span.t), _) = span.end_offset = offset in
  match List.find_opt contains spans with
  | Some (_, declaration) -> Some declaration
  | None -> Option.map snd (List.find_opt ends_at spans)

let is_word_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'

let line_start text line =
  let length = String.length text in
  let rec go i current =
    if current >= line || i >= length then i
    else
      match String.index_from_opt text i '\n' with
      | None -> length
      | Some j -> go (j + 1) (current + 1)
  in
  go 0 1

let line_of_offset text offset =
  let count = ref 1 in
  for i = 0 to min offset (String.length text) - 1 do
    if text.[i] = '\n' then incr count
  done;
  !count

let c_declaration_end text start =
  let length = String.length text in
  let rec block i =
    if i + 1 >= length then length
    else if text.[i] = '*' && text.[i + 1] = '/' then i + 2
    else block (i + 1)
  in
  let rec line i = if i >= length || text.[i] = '\n' then i else line (i + 1) in
  let rec quoted quote i =
    if i >= length then length
    else if text.[i] = '\\' then quoted quote (i + 2)
    else if text.[i] = quote then i + 1
    else quoted quote (i + 1)
  in
  let rec scan i braces parens =
    if i >= length then length
    else
      match text.[i] with
      | '/' when i + 1 < length && text.[i + 1] = '*' ->
          scan (block (i + 2)) braces parens
      | '/' when i + 1 < length && text.[i + 1] = '/' -> scan (line i) braces parens
      | ('"' | '\'') as quote -> scan (quoted quote (i + 1)) braces parens
      | '(' -> scan (i + 1) braces (parens + 1)
      | ')' -> scan (i + 1) braces (parens - 1)
      | '{' -> scan (i + 1) (braces + 1) parens
      | '}' when braces <= 1 -> i + 1
      | '}' -> scan (i + 1) (braces - 1) parens
      | ';' when braces = 0 && parens = 0 -> i + 1
      | _ -> scan (i + 1) braces parens
  in
  scan start 0 0

let normalise_c_text raw =
  let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let keep_newlines = String.contains raw '{' in
  let length = String.length raw in
  let buffer = Buffer.create length in
  let rec go i =
    if i < length then
      if is_space raw.[i] then begin
        let j = ref i and newline = ref false in
        while !j < length && is_space raw.[!j] do
          if raw.[!j] = '\n' then newline := true;
          incr j
        done;
        if Buffer.length buffer > 0 && !j < length then
          Buffer.add_string buffer (if keep_newlines && !newline then "\n" else " ");
        go !j
      end
      else begin
        Buffer.add_char buffer raw.[i];
        go (i + 1)
      end
  in
  go 0;
  Buffer.contents buffer

let c_declaration_text text start =
  normalise_c_text (String.sub text start (c_declaration_end text start - start))

let c_field_line text origin name =
  let start = line_start text origin in
  let stop = c_declaration_end text start in
  let length = String.length name in
  let boundary i = i < 0 || i >= String.length text || not (is_word_char text.[i]) in
  let rec find i =
    if i + length > stop then origin
    else if String.sub text i length = name && boundary (i - 1) && boundary (i + length)
    then line_of_offset text i
    else find (i + 1)
  in
  find start

let c_line_of (declaration : Sema_index.declaration) text origin =
  match (declaration.Sema_index.kind, text) with
  | Sema_index.Field, Some text -> c_field_line text origin declaration.Sema_index.name
  | _ -> origin

let c_parts (declaration : Sema_index.declaration) =
  match declaration.Sema_index.c_origin with
  | None | Some ("<unknown>", _) -> []
  | Some (header, origin) ->
      let text = disk_text header in
      let line = c_line_of declaration text origin in
      let fence =
        match text with
        | None -> []
        | Some text ->
            let code = c_declaration_text text (line_start text line) in
            if code = "" then [] else [ "```c\n" ^ code ^ "\n```" ]
      in
      fence @ [ Printf.sprintf "%s:%d" header line ]

let c_location header line =
  if header = "<unknown>" then Json.Null
  else
    let row = max 0 (line - 1) in
    Json.Obj
      [
        ("uri", Json.Str (path_to_uri (Driver.canonical_path header)));
        ("range", range_json (row, 0) (row, 0));
      ]

let definition state params =
  let uri, line, character = request_point params in
  match resolve state uri line character with
  | None -> Json.Null
  | Some (snapshot, key, offset) -> (
      match target snapshot key offset with
      | None -> Json.Null
      | Some ({ Sema_index.c_origin = Some (header, origin); _ } as declaration) ->
          c_location header (c_line_of declaration (disk_text header) origin)
      | Some declaration -> (
          match mapped state snapshot declaration.Sema_index.name_span with
          | Some (file, first, last) ->
              Json.Obj
                [
                  ("uri", Json.Str (uri_of_key state file));
                  ("range", range_json first last);
                ]
          | None -> Json.Null))

let same_span (a : Span.t) (b : Span.t) =
  a.start_offset = b.start_offset
  && a.end_offset = b.end_offset
  && Driver.canonical_path a.file = Driver.canonical_path b.file

let references state params =
  let uri, line, character = request_point params in
  let with_declaration = include_declaration params in
  match resolve state uri line character with
  | None -> Json.Null
  | Some (snapshot, key, offset) -> (
      match target snapshot key offset with
      | None -> Json.Null
      | Some declaration ->
          let same (other : Sema_index.declaration) =
            other.Sema_index.name = declaration.Sema_index.name
            && other.Sema_index.kind = declaration.Sema_index.kind
            && same_span other.Sema_index.name_span declaration.Sema_index.name_span
          in
          let uses =
            List.filter_map
              (fun (r : Sema_index.reference) ->
                if same r.Sema_index.declaration then Some r.Sema_index.use_span
                else None)
              (Sema_index.references snapshot.index)
          in
          let spans =
            if with_declaration then declaration.Sema_index.name_span :: uses else uses
          in
          let keyed =
            List.filter_map
              (fun span ->
                Option.map
                  (fun (file, (l1, c1), (l2, c2)) ->
                    (uri_of_key state file, l1, c1, l2, c2))
                  (mapped state snapshot span))
              spans
          in
          Json.Arr
            (List.map
               (fun (location_uri, l1, c1, l2, c2) ->
                 Json.Obj
                   [
                     ("uri", Json.Str location_uri);
                     ("range", range_json (l1, c1) (l2, c2));
                   ])
               (List.sort_uniq compare keyed)))

let symbol_kind kind =
  Sema_index.(
    match kind with
    | Function -> 12
    | Parameter | Local | View | Global -> 13
    | Const | Const_array -> 14
    | Struct | Union | Opaque | Type_alias -> 23
    | Field -> 8
    | Type_parameter | Const_parameter -> 26)

let span_range text (span : Span.t) =
  range_json
    (Lsp_position.position_of_offset text span.start_offset)
    (Lsp_position.position_of_offset text span.end_offset)

let outline_range (d : Sema_index.declaration) =
  match d.Sema_index.kind with
  | Parameter | Local | View | Field | Type_parameter | Const_parameter ->
      d.Sema_index.name_span
  | _ ->
      if Span.is_synthetic d.Sema_index.item_span then d.Sema_index.name_span
      else d.Sema_index.item_span

let symbol_json place (d : Sema_index.declaration) children =
  match (place (outline_range d), place d.Sema_index.name_span) with
  | Some range, Some selection ->
      Some
        (Json.Obj
           [
             ("name", Json.Str d.Sema_index.name);
             ("detail", Json.Str "");
             ("kind", Json.Num (string_of_int (symbol_kind d.Sema_index.kind)));
             ("range", range);
             ("selectionRange", selection);
             ("children", Json.Arr children);
           ])
  | _ -> None

let outline_symbols place index key =
  let declarations =
    List.filter
      (fun (d : Sema_index.declaration) ->
        d.Sema_index.c_origin = None
        && (not (Span.is_synthetic d.Sema_index.name_span))
        && Driver.canonical_path d.Sema_index.name_span.Span.file = key)
      (Sema_index.declarations index)
  in
  let by_position (a : Sema_index.declaration) (b : Sema_index.declaration) =
    compare a.Sema_index.name_span.Span.start_offset
      b.Sema_index.name_span.Span.start_offset
  in
  let children parent =
    List.filter
      (fun (d : Sema_index.declaration) -> d.Sema_index.container = Some parent)
      declarations
    |> List.sort by_position
  in
  let symbol (d : Sema_index.declaration) =
    let nested =
      match d.Sema_index.kind with
      | Sema_index.Function ->
          List.filter_map (fun c -> symbol_json place c []) (children d.Sema_index.name)
      | Sema_index.Struct | Sema_index.Union ->
          List.filter_map
            (fun (c : Sema_index.declaration) ->
              if c.Sema_index.kind = Sema_index.Field then symbol_json place c []
              else None)
            (children d.Sema_index.name)
      | _ -> []
    in
    symbol_json place d nested
  in
  let roots =
    List.filter
      (fun (d : Sema_index.declaration) -> d.Sema_index.container = None)
      declarations
    |> List.sort by_position
  in
  Json.Arr (List.filter_map symbol roots)

let document_symbols state params =
  let uri = document_uri params in
  match key_of_uri uri with
  | None -> Json.Null
  | Some key -> (
      match current_text state key with
      | None -> Json.Null
      | Some text -> (
          match Parser.parse (Source.create ~file:key ~text) with
          | Ok program ->
              let index = Sema_index.create () in
              Sema_index.declare_program index ~c_aliases:[] program;
              outline_symbols (fun span -> Some (span_range text span)) index key
          | Error _ -> (
              match snapshot_for state key with
              | None -> Json.Arr []
              | Some snapshot ->
                  let place (span : Span.t) =
                    match mapped state snapshot span with
                    | Some (_, first, last) -> Some (range_json first last)
                    | None -> None
                  in
                  outline_symbols place snapshot.index key)))

let identifier_span text (declaration : Sema_index.declaration) =
  let span = declaration.Sema_index.name_span in
  let name = declaration.Sema_index.name in
  let length = String.length name in
  let limit = min span.end_offset (String.length text) in
  let boundary i = i < 0 || i >= String.length text || not (is_word_char text.[i]) in
  let rec scan i =
    if i + length > limit then None
    else if String.sub text i length = name && boundary (i - 1) && boundary (i + length)
    then Some (i, i + length)
    else scan (i + 1)
  in
  scan span.start_offset

let hover_declaration (snapshot : snapshot) key offset =
  match Hashtbl.find_opt snapshot.texts key with
  | None -> None
  | Some text ->
      let inside (span : Span.t) =
        (not (Span.is_synthetic span)) && Driver.canonical_path span.file = key
      in
      let declared =
        List.filter_map
          (fun (d : Sema_index.declaration) ->
            if not (inside d.name_span) then None
            else
              match identifier_span text d with
              | Some (first, last) when first <= offset && offset < last ->
                  Some (last - first, d, None)
              | _ -> None)
          (Sema_index.declarations snapshot.index)
      in
      let used =
        List.filter_map
          (fun (r : Sema_index.reference) ->
            let span = r.use_span in
            if inside span && span.start_offset <= offset && offset < span.end_offset
            then Some (span.end_offset - span.start_offset, r.declaration, Some span)
            else None)
          (Sema_index.references snapshot.index)
      in
      let best =
        List.fold_left
          (fun best ((length, _, _) as candidate) ->
            match best with
            | Some (best_length, _, _) when best_length <= length -> best
            | _ -> Some candidate)
          None (used @ declared)
      in
      Option.map (fun (_, declaration, use) -> (declaration, use)) best

let expression_type (snapshot : snapshot) key offset =
  let containing =
    List.filter
      (fun (e : Sema_index.expression_type) ->
        let span = e.span in
        (not (Span.is_synthetic span))
        && Driver.canonical_path span.file = key
        && span.start_offset <= offset && offset < span.end_offset)
      (Sema_index.expression_types snapshot.index)
  in
  let width (e : Sema_index.expression_type) =
    e.span.end_offset - e.span.start_offset
  in
  match containing with
  | [] -> None
  | first :: _ ->
      let smallest =
        List.fold_left (fun acc e -> min acc (width e)) (width first) containing
      in
      let at_smallest = List.find (fun e -> width e = smallest) containing in
      let same (e : Sema_index.expression_type) =
        e.span.start_offset = at_smallest.span.start_offset
        && e.span.end_offset = at_smallest.span.end_offset
      in
      let at_best =
        List.stable_sort
          (fun (a : Sema_index.expression_type) (b : Sema_index.expression_type) ->
            Int.compare a.rank b.rank)
          (List.filter same containing)
      in
      let texts =
        List.fold_left
          (fun acc (e : Sema_index.expression_type) ->
            if List.mem e.ty acc then acc else acc @ [ e.ty ])
          [] at_best
      in
      Some (String.concat " | " texts, at_smallest.span)

let hover_markdown (header, facts) extra =
  String.concat "\n\n" ((("```fas\n" ^ header ^ "\n```") :: facts) @ extra)

let word_bounds text offset =
  let length = String.length text in
  let offset = min offset length in
  let rec back i = if i > 0 && is_word_char text.[i - 1] then back (i - 1) else i in
  let rec forward i =
    if i < length && is_word_char text.[i] then forward (i + 1) else i
  in
  (back offset, forward offset)

let unsupported_hover state params =
  let uri, line, character = request_point params in
  match key_of_uri uri with
  | None -> None
  | Some key -> (
      match current_text state key with
      | None -> None
      | Some current -> (
          let first, last =
            word_bounds current
              (Lsp_position.offset_of_position current (line, character))
          in
          if first >= last then None
          else
            let name = String.sub current first (last - first) in
            let closure root =
              List.mem key
                (Option.value ~default:[] (Hashtbl.find_opt state.loads root))
            in
            let roots =
              Hashtbl.fold
                (fun root index acc ->
                  if closure root then (root, index) :: acc else acc)
                state.c_roots []
              |> List.sort (fun (a, _) (b, _) -> String.compare a b)
            in
            match
              List.find_map
                (fun (_, index) ->
                  Option.map
                    (fun reason -> (index, reason))
                    (List.assoc_opt name (Sema_index.c_unsupported index)))
                roots
            with
            | None -> None
            | Some (index, reason) ->
                let origin =
                  match Sema_index.c_origin_named index name with
                  | Some (header, row) -> [ Printf.sprintf "%s:%d" header row ]
                  | None -> []
                in
                let value =
                  String.concat "\n\n"
                    (Printf.sprintf "C declaration `%s` is not supported: %s" name
                       reason
                    :: origin)
                in
                Some
                  (Json.Obj
                     [
                       ( "contents",
                         Json.Obj
                           [ ("kind", Json.Str "markdown"); ("value", Json.Str value) ]
                       );
                       ( "range",
                         range_json
                           (Lsp_position.position_of_offset current first)
                           (Lsp_position.position_of_offset current last) );
                     ])))

let hover_result state params =
  let uri, line, character = request_point params in
  match resolve state uri line character with
  | None -> None
  | Some (snapshot, key, offset) -> (
      let located =
        match hover_declaration snapshot key offset with
        | Some (declaration, use) ->
            let span =
              match use with
              | Some span -> Some span
              | None ->
                  Option.map
                    (fun (first, last) ->
                      let name_span = declaration.Sema_index.name_span in
                      { name_span with Span.start_offset = first; end_offset = last })
                    (Option.bind (Hashtbl.find_opt snapshot.texts key) (fun text ->
                         identifier_span text declaration))
            in
            Option.map
              (fun content -> (content, c_parts declaration, span))
              (Sema_index.describe snapshot.index ?use declaration)
        | None ->
            Option.map
              (fun (ty, span) -> ((ty, []), [], Some span))
              (expression_type snapshot key offset)
      in
      match located with
      | None -> None
      | Some (content, extra, span) -> (
          match Option.bind span (mapped state snapshot) with
          | None -> None
          | Some (_, first, last) ->
              Some
                (Json.Obj
                   [
                     ( "contents",
                       Json.Obj
                         [
                           ("kind", Json.Str "markdown");
                           ("value", Json.Str (hover_markdown content extra));
                         ] );
                     ("range", range_json first last);
                   ])))

let hover state params =
  match hover_result state params with
  | Some result -> result
  | None -> Option.value ~default:Json.Null (unsupported_hover state params)

let inlay_range params =
  let range = child "range" params in
  let position name =
    let value = child name range in
    match (index_member "line" value, index_member "character" value) with
    | Some line, Some character -> (line, character)
    | _ -> invalid "params.range must hold non-negative integer start and end positions"
  in
  let first = position "start" in
  let last = position "end" in
  if compare first last > 0 then invalid "params.range start must not follow end"
  else (first, last)

let inlay_label instances =
  let render arguments = "[" ^ String.concat ", " arguments ^ "]" in
  let labels = List.map render instances in
  let distinct =
    List.fold_left
      (fun acc label -> if List.mem label acc then acc else acc @ [ label ])
      [] labels
  in
  String.concat " | " distinct

let inlay_hints state params =
  let uri = document_uri params in
  let first, last = inlay_range params in
  match key_of_uri uri with
  | None -> Json.Arr []
  | Some key -> (
      match snapshot_for state key with
      | None -> Json.Arr []
      | Some snapshot ->
          let groups = Hashtbl.create 16 in
          List.iter
            (fun (call : Sema_index.generic_call) ->
              let span = call.Sema_index.callee_span in
              if
                (not call.Sema_index.explicit)
                && (not (Span.is_synthetic span))
                && Driver.canonical_path span.file = key
              then
                let previous =
                  Option.value ~default:[] (Hashtbl.find_opt groups span)
                in
                Hashtbl.replace groups span (call :: previous))
            (Sema_index.generic_calls snapshot.index);
          Hashtbl.fold
            (fun span calls acc ->
              let ordered =
                List.stable_sort
                  (fun (a : Sema_index.generic_call) (b : Sema_index.generic_call) ->
                    Int.compare a.Sema_index.order b.Sema_index.order)
                  (List.rev calls)
              in
              let label =
                inlay_label
                  (List.map
                     (fun (c : Sema_index.generic_call) -> c.Sema_index.arguments)
                     ordered)
              in
              match mapped state snapshot span with
              | Some (_, _, position) when first <= position && position <= last ->
                  (position, label) :: acc
              | _ -> acc)
            groups []
          |> List.sort (fun (a, _) (b, _) -> compare a b)
          |> List.map (fun (position, label) ->
              Json.Obj
                [
                  ("position", position_json position);
                  ("label", Json.Str label);
                  ("kind", Json.Num "1");
                  ("paddingLeft", Json.Bool false);
                ])
          |> fun hints -> Json.Arr hints)

let initialize_result =
  Json.Obj
    [
      ( "capabilities",
        Json.Obj
          [
            ("textDocumentSync", Json.Num "1");
            ("definitionProvider", Json.Bool true);
            ("referencesProvider", Json.Bool true);
            ("documentSymbolProvider", Json.Bool true);
            ("hoverProvider", Json.Bool true);
            ("inlayHintProvider", Json.Bool true);
          ] );
      ( "serverInfo",
        Json.Obj [ ("name", Json.Str "fas"); ("version", Json.Str Driver.version) ] );
    ]

let answer id compute =
  match compute () with
  | result -> respond id result
  | exception Invalid message -> fail id (-32602) message

let handle_request state id meth params =
  if state.shut_down then fail id (-32600) "server is shutting down"
  else if (not state.initialized) && meth <> "initialize" then
    fail id (-32002) "server not initialized"
  else
    match meth with
    | "initialize" ->
        if state.initialized then fail id (-32600) "server already initialized"
        else (
          state.initialized <- true;
          respond id initialize_result)
    | "shutdown" ->
        state.shut_down <- true;
        respond id Json.Null
    | "textDocument/definition" -> answer id (fun () -> definition state params)
    | "textDocument/references" -> answer id (fun () -> references state params)
    | "textDocument/hover" -> answer id (fun () -> hover state params)
    | "textDocument/inlayHint" -> answer id (fun () -> inlay_hints state params)
    | "textDocument/documentSymbol" ->
        answer id (fun () -> document_symbols state params)
    | _ -> fail id (-32601) ("method not found: " ^ meth)

let handle_notification state meth params =
  match meth with
  | "exit" -> state.exit_code <- Some (if state.shut_down then 0 else 1)
  | _ when (not state.initialized) || state.shut_down -> ()
  | "textDocument/didOpen" -> open_document state params
  | "textDocument/didChange" -> change_document state params
  | "textDocument/didSave" -> save_document state params
  | "textDocument/didClose" -> close_document state params
  | _ -> ()

let handle_body state body =
  match Json.parse body with
  | Error _ -> fail Json.Null (-32700) "parse error"
  | Ok (Json.Obj fields as message) -> (
      let params = child "params" message in
      match (string_member "method" message, List.assoc_opt "id" fields) with
      | Some meth, Some id -> (
          try handle_request state id meth params
          with exn -> fail id (-32603) (Printexc.to_string exn))
      | Some meth, None -> (
          try handle_notification state meth params
          with exn -> log (Printexc.to_string exn))
      | None, Some id -> fail id (-32600) "invalid request"
      | None, None -> fail Json.Null (-32600) "invalid request")
  | Ok _ -> fail Json.Null (-32600) "invalid request"

let run options =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  let state = create options in
  let chunk = Bytes.create 65536 in
  let finish () = if state.shut_down then 0 else 1 in
  let rec loop pending =
    match state.exit_code with
    | Some code -> code
    | None -> (
        match Lsp_frame.take_message pending with
        | Lsp_frame.Message (body, rest) ->
            (try handle_body state body with exn -> log (Printexc.to_string exn));
            loop rest
        | Malformed (reason, rest) ->
            fail Json.Null (-32700) reason;
            loop rest
        | Incomplete -> (
            match input stdin chunk 0 (Bytes.length chunk) with
            | 0 -> finish ()
            | n -> loop (pending ^ Bytes.sub_string chunk 0 n)
            | exception Sys_error _ -> finish ()))
  in
  let code = loop "" in
  C_import.clear_cache state.cache;
  code
