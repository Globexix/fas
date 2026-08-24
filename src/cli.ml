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

let usage =
  "usage: fas [options] file.fas ...\n\
  \  -o PATH       output path (default a.out)\n\
  \  --emit-ast    print parsed AST\n\
  \  --emit-ir     print custom IR\n\
  \  --emit-llvm   print LLVM IR\n\
  \  --emit-asm, -S emit assembly\n\
  \  --emit-obj, -c emit object\n\
  \  --keep        keep intermediate files\n\
  \  -O0..-O3      optimization level\n\
  \  -debug        debug mode\n\
  \  -release      release mode\n\
  \  -kernel       kernel mode\n\
  \  --help        show this help"

let parse argv =
  let n = Array.length argv in
  let rec loop i inputs output emit keep optimization debug release kernel =
    if i >= n then
      if inputs = [] then Error "no input files"
      else
        Ok
          {
            inputs = List.rev inputs;
            output;
            emit;
            keep;
            optimization;
            debug;
            release;
            kernel;
          }
    else
      match argv.(i) with
      | "--help" | "-h" -> Error usage
      | "-o" ->
          if i + 1 >= n then Error "-o requires an output path"
          else
            loop (i + 2) inputs argv.(i + 1) emit keep optimization debug release kernel
      | "--emit-ast" ->
          loop (i + 1) inputs output Ast keep optimization debug release kernel
      | "--emit-ir" ->
          loop (i + 1) inputs output Ir keep optimization debug release kernel
      | "--emit-llvm" ->
          loop (i + 1) inputs output Llvm keep optimization debug release kernel
      | "--emit-asm" | "-S" ->
          loop (i + 1) inputs output Asm keep optimization debug release kernel
      | "--emit-obj" | "-c" ->
          loop (i + 1) inputs output Obj keep optimization debug release kernel
      | "--keep" ->
          loop (i + 1) inputs output emit true optimization debug release kernel
      | "-debug" -> loop (i + 1) inputs output emit keep 0 true release kernel
      | "-release" -> loop (i + 1) inputs output emit keep 2 debug true kernel
      | "-kernel" -> loop (i + 1) inputs output emit keep 2 debug release true
      | flag
        when String.length flag = 3
             && flag.[0] = '-'
             && flag.[1] = 'O'
             && flag.[2] >= '0'
             && flag.[2] <= '3' ->
          loop (i + 1) inputs output emit keep
            (Char.code flag.[2] - Char.code '0')
            debug release kernel
      | flag when String.length flag > 0 && flag.[0] = '-' ->
          Error ("unknown option: " ^ flag)
      | file ->
          loop (i + 1) (file :: inputs) output emit keep optimization debug release
            kernel
  in
  loop 1 [] "a.out" Executable false 2 false false false
