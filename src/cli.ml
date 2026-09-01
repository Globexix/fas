type emit = Ast | Ir | Llvm | Asm | Obj | Executable

type t = {
  inputs : string list;
  output : string;
  output_explicit : bool;
  emit : emit;
  keep : bool;
  optimization : int;
  debug : bool;
  no_inline_function : string option;
}

type command = Run of t | Help

let usage =
  "usage: fas [options] file.fas ...\n\
  \  -o PATH       output path (default a.out, INPUT.o with -c, INPUT.s with -S)\n\
  \  --emit-ast    print parsed AST\n\
  \  --emit-ir     print custom IR\n\
  \  --emit-llvm   print LLVM IR\n\
  \  --emit-asm, -S emit assembly\n\
  \  --emit-obj, -c emit object\n\
  \  --keep        keep intermediate files\n\
  \  -O0..-O3      optimization level\n\
  \  -debug        debug mode\n\
  \  -no-inline NAME  keep NAME out of inline optimization (debug only)\n\
  \  --help        show this help"

let default_output emit inputs =
  let source_output extension =
    match inputs with
    | input :: _ -> Filename.remove_extension (Filename.basename input) ^ extension
    | [] -> "a.out" ^ extension
  in
  match emit with Asm -> source_output ".s" | Obj -> source_output ".o" | _ -> "a.out"

let parse argv =
  let n = Array.length argv in
  let rec loop i inputs output emit keep optimization optimization_explicit debug
      no_inline_function =
    if i >= n then
      if inputs = [] then Error "no input files"
      else if Option.is_some no_inline_function && not debug then
        Error "-no-inline requires -debug"
      else
        let inputs = List.rev inputs in
        let output_explicit = Option.is_some output in
        let output =
          match output with Some path -> path | None -> default_output emit inputs
        in
        Ok
          (Run
             {
               inputs;
               output;
               output_explicit;
               emit;
               keep;
               optimization;
               debug;
               no_inline_function;
             })
    else
      match argv.(i) with
      | "--help" | "-h" -> Ok Help
      | "-o" ->
          if i + 1 >= n then Error "-o requires an output path"
          else
            loop (i + 2) inputs
              (Some argv.(i + 1))
              emit keep optimization optimization_explicit debug no_inline_function
      | "--emit-ast" ->
          loop (i + 1) inputs output Ast keep optimization optimization_explicit debug
            no_inline_function
      | "--emit-ir" ->
          loop (i + 1) inputs output Ir keep optimization optimization_explicit debug
            no_inline_function
      | "--emit-llvm" ->
          loop (i + 1) inputs output Llvm keep optimization optimization_explicit debug
            no_inline_function
      | "--emit-asm" | "-S" ->
          loop (i + 1) inputs output Asm keep optimization optimization_explicit debug
            no_inline_function
      | "--emit-obj" | "-c" ->
          loop (i + 1) inputs output Obj keep optimization optimization_explicit debug
            no_inline_function
      | "--keep" ->
          loop (i + 1) inputs output emit true optimization optimization_explicit debug
            no_inline_function
      | "-debug" ->
          loop (i + 1) inputs output emit keep
            (if optimization_explicit then optimization else 0)
            optimization_explicit true no_inline_function
      | "-no-inline" ->
          if i + 1 >= n then Error "-no-inline requires a function name"
          else
            let name = argv.(i + 1) in
            if name = "" || name.[0] = '-' then
              Error "-no-inline requires a function name"
            else if Option.is_some no_inline_function then
              Error "duplicate -no-inline option"
            else
              loop (i + 2) inputs output emit keep optimization optimization_explicit
                debug (Some name)
      | flag
        when String.length flag = 3
             && flag.[0] = '-'
             && flag.[1] = 'O'
             && flag.[2] >= '0'
             && flag.[2] <= '3' ->
          loop (i + 1) inputs output emit keep
            (Char.code flag.[2] - Char.code '0')
            true debug no_inline_function
      | flag when String.length flag > 0 && flag.[0] = '-' ->
          Error ("unknown option: " ^ flag)
      | file ->
          loop (i + 1) (file :: inputs) output emit keep optimization
            optimization_explicit debug no_inline_function
  in
  loop 1 [] None Executable false 2 false false None
