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

val error : ?issue:issue -> ?notes:string list -> ?help:string -> Span.t -> string -> t
val warning : ?notes:string list -> ?help:string -> Span.t -> string -> t
val render : source:Source.t option -> t -> string
val render_all : source:Source.t option -> t list -> string
val local_declaration_message : string -> string option

val local_type_error :
  ?span:Span.t -> string -> ('a, t list) result -> ('a, t list) result
