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
