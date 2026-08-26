type emit = Ast | Ir | Llvm | Asm | Obj | Executable

type t = {
  inputs : string list;
  output : string;
  output_explicit : bool;
  emit : emit;
  keep : bool;
  optimization : int;
  debug : bool;
  release : bool;
  kernel : bool;
}

type command = Run of t | Help

val parse : string array -> (command, string) result
val usage : string
