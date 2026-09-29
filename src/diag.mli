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

val error :
  ?issue:issue -> ?notes:string list -> ?hints:string list -> Span.t -> string -> t

val warning : ?notes:string list -> ?hints:string list -> Span.t -> string -> t
val render : source:Source.t option -> t -> string
val render_all : source:Source.t option -> t list -> string
