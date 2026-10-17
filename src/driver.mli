val use_path_error : string -> string option
val canonical_path : string -> string
val use_target : string -> string -> string -> string * string

val load_program :
  ?overlay:(string -> string option) ->
  limits:Limits.t ->
  string ->
  ( Ast.program
    * (string * string list) list
    * (string * C_import.header list) list
    * (string * C_import.header list) list,
    Diag.t list )
  result

type checked = {
  program : Ast.program;
  ir : Ir.module_;
  redirect : string -> string;
  adapters : C_import.adapter list;
  c_objects : string list;
  declaration_headers : string list;
  declarations : string;
  artifacts : string list;
  keep : bool;
}

val analyze :
  ?overlay:(string -> string option) ->
  ?cache:C_import.cache ->
  ?index:Sema_index.t ->
  Cli.t ->
  (checked, Diag.t list) result

val release : checked -> unit

type gap = { file : string; start_offset : int; end_offset : int }
type recovered = { recovered_index : Sema_index.t; gaps : gap list }

val recover :
  ?overlay:(string -> string option) ->
  ?cache:C_import.cache ->
  Cli.t ->
  (recovered, Diag.t list) result

val check :
  ?overlay:(string -> string option) ->
  ?cache:C_import.cache ->
  ?index:Sema_index.t ->
  Cli.t ->
  (checked, Diag.t list) result

val run :
  ?remarks:string ->
  ?llc_remarks:string ->
  ?llvm_text:(string -> unit) ->
  ?c_debug:bool ->
  Cli.t ->
  (string, Diag.t list) result

val tool : string -> string -> string
val executable_path : string -> string
val version : string
val version_lines : unit -> string list
