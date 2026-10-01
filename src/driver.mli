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

val run : Cli.t -> (string, Diag.t list) result
