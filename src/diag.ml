type severity = Error | Warning

type t = {
  severity : severity;
  primary : Span.t;
  message : string;
  notes : string list;
  hints : string list;
}

let error ?(notes = []) ?(hints = []) primary message =
  { severity = Error; primary; message; notes; hints }

let warning ?(notes = []) ?(hints = []) primary message =
  { severity = Warning; primary; message; notes; hints }

let render_one ~source diagnostic =
  let level =
    match diagnostic.severity with Error -> "error" | Warning -> "warning"
  in
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
