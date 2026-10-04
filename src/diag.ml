type severity = Error | Warning
type issue = General | Not_constant

type t = {
  severity : severity;
  issue : issue;
  primary : Span.t;
  message : string;
  notes : string list;
  help : string option;
}

let error ?(issue = General) ?(notes = []) ?help primary message =
  { severity = Error; issue; primary; message; notes; help }

let warning ?(notes = []) ?help primary message =
  { severity = Warning; issue = General; primary; message; notes; help }

let local_declaration_message name =
  if List.mem name [ "let"; "auto"; "mut" ] then
    Some
      (Printf.sprintf
         "locals are declared as `name Type = value`; `%s` is not a Fas keyword" name)
  else if
    List.mem name
      [ "int"; "char"; "short"; "long"; "unsigned"; "signed"; "float"; "double" ]
  then
    Some
      (Printf.sprintf
         "`%s` is not a Fas type; locals are declared as `name Type = value`, e.g. `x \
          i32`"
         name)
  else None

let local_type_error ?span name result =
  Result.map_error
    (List.map (fun diagnostic ->
         let prefix = "unknown type `" in
         if not (String.starts_with ~prefix diagnostic.message) then diagnostic
         else
           let message =
             match local_declaration_message name with
             | Some message -> message
             | None when Names.parser_type_name name ->
                 let other =
                   String.sub diagnostic.message (String.length prefix)
                     (String.length diagnostic.message - String.length prefix - 1)
                 in
                 Printf.sprintf
                   "locals are declared as `name Type = value`; write `%s %s`" other
                   name
             | None -> diagnostic.message
           in
           let primary =
             match span with
             | Some (span : Span.t) when message <> diagnostic.message ->
                 { span with Span.end_offset = span.start_offset + String.length name }
             | _ -> diagnostic.primary
           in
           { diagnostic with primary; message }))
    result

let render_one ~source diagnostic =
  let level = if diagnostic.severity = Warning then "warning" else "error" in
  let location = Span.to_string diagnostic.primary in
  let excerpt =
    match source with
    | None -> ""
    | Some src when Source.file src = diagnostic.primary.Span.file -> (
        match Source.line_text src diagnostic.primary.Span.line with
        | None -> ""
        | Some line ->
            let text = Source.text src in
            let offset_for_line line =
              let rec find current offset =
                if current >= line then offset
                else
                  match String.index_from_opt text offset '\n' with
                  | Some newline -> find (current + 1) (newline + 1)
                  | None -> String.length text
              in
              find 1 0
            in
            let offset_only =
              diagnostic.primary.Span.start_offset = 0
              && diagnostic.primary.Span.end_offset = 0
            in
            let line_start =
              if offset_only then offset_for_line diagnostic.primary.Span.line
              else
                max 0
                  (diagnostic.primary.Span.start_offset
                  - (diagnostic.primary.Span.column - 1))
            in
            let line_stop =
              match String.index_from_opt text line_start '\n' with
              | Some stop -> stop
              | None -> String.length text
            in
            let start =
              if offset_only then
                min line_stop (line_start + diagnostic.primary.Span.column - 1)
              else max line_start (min line_stop diagnostic.primary.Span.start_offset)
            in
            let stop =
              if offset_only then min line_stop (start + 1)
              else max start (min line_stop diagnostic.primary.Span.end_offset)
            in
            let width = max 1 (stop - start) in
            let prefix_length = max 0 (min (String.length line) (start - line_start)) in
            let prefix = String.make prefix_length ' ' in
            Printf.sprintf "\n  %s\n  %s^%s\n" line prefix (String.make (width - 1) '~')
        )
    | Some _ -> ""
  in
  let notes = List.map (fun n -> Printf.sprintf "note: %s\n" n) diagnostic.notes in
  let help =
    match diagnostic.help with
    | None -> ""
    | Some text ->
        Printf.sprintf "%shelp: %s\n" (if excerpt = "" then "\n" else "") text
  in
  let notes =
    if excerpt = "" && help = "" && notes <> [] then "\n" :: notes else notes
  in
  Printf.sprintf "%s: %s: %s%s%s%s" location level diagnostic.message excerpt help
    (String.concat "" notes)

let render ~source diagnostic = render_one ~source diagnostic

let render_all ~source diagnostics =
  String.concat "\n" (List.map (render_one ~source) diagnostics)
