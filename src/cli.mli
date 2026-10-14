type emit = Ir | Ir_json | Llvm | Optimized_llvm | Asm | Obj | Header | Executable
type diagnostics = Text | Json

type t = {
  input : string;
  output : string;
  output_explicit : bool;
  emit : emit;
  keep : bool;
  optimization : int;
  debug : bool;
  no_inline_function : string option;
  sanitizers : string list;
  link_inputs : string list;
  c_flags : string list;
  diagnostics : diagnostics;
}

type command = Run of t | Run_program of t * string list | Check of t | Help | Version

val parse : string array -> (command, string) result
val usage : string
