type emit = Ast | Ir | Llvm | Asm | Obj | Executable

type t = {
  inputs : string list;
  output : string;
  emit : emit;
  keep : bool;
  optimization : int;
  debug : bool;
  release : bool;
  kernel : bool;
}

val parse : string array -> (t, string) result
val usage : string
