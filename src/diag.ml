type severity = Error | Warning
type issue = General | Not_constant

type t = {
  severity : severity;
  issue : issue;
  primary : Span.t;
  message : string;
  notes : string list;
  hints : string list;
}

let error ?(issue = General) ?(notes = []) ?(hints = []) primary message =
  { severity = Error; issue; primary; message; notes; hints }

let warning ?(notes = []) ?(hints = []) primary message =
  { severity = Warning; issue = General; primary; message; notes; hints }

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

let local_type_error name result =
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
           { diagnostic with message }))
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
        | Some line -> Printf.sprintf "\n  %s\n" line)
    | Some _ -> ""
  in
  let notes = List.map (fun n -> Printf.sprintf "note: %s\n" n) diagnostic.notes in
  let hints = List.map (fun h -> Printf.sprintf "help: %s\n" h) diagnostic.hints in
  Printf.sprintf "%s: %s: %s%s%s%s" location level diagnostic.message excerpt
    (String.concat "" notes) (String.concat "" hints)

let render ~source diagnostic = render_one ~source diagnostic

let render_all ~source diagnostics =
  String.concat "\n" (List.map (render_one ~source) diagnostics)
