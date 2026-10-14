val use_path_error : string -> string option

val load_program :
  limits:Limits.t ->
  string ->
  ( Ast.program
    * (string * string list) list
    * (string * C_import.header list) list
    * (string * C_import.header list) list,
    Diag.t list )
  result

val run :
  ?remarks:string ->
  ?llc_remarks:string ->
  ?llvm_text:(string -> unit) ->
  ?c_debug:bool ->
  Cli.t ->
  (string, Diag.t list) result

val tool : string -> string -> string
val executable_path : string -> string
val version_lines : unit -> string list
