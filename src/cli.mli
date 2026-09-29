type emit = Ir | Llvm | Asm | Obj | Executable

type t = {
  input : string;
  output : string;
  output_explicit : bool;
  emit : emit;
  keep : bool;
  optimization : int;
  debug : bool;
  no_inline_function : string option;
  link_inputs : string list;
}

type command = Run of t | Help

val parse : string array -> (command, string) result
val usage : string
