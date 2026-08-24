type severity = Error | Warning

type t = {
  severity : severity;
  primary : Span.t;
  message : string;
  notes : string list;
  hints : string list;
}

val error : ?notes:string list -> ?hints:string list -> Span.t -> string -> t
val warning : ?notes:string list -> ?hints:string list -> Span.t -> string -> t
val render : source:Source.t option -> t -> string
val render_all : source:Source.t option -> t list -> string
