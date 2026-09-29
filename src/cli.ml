type emit = Ir | Llvm | Asm | Obj | Executable

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
  \  --emit-ir     print the compiler's custom IR dump\n\
  \  --emit-llvm   print unoptimized LLVM IR after verification\n\
  \  --emit-asm, -S emit assembly\n\
  \  --emit-obj, -c emit object\n\
  \  --keep        retain and report intermediate paths and active tool pipeline\n\
  \  -O0..-O3      set opt and llc optimization levels (default -O2)\n\
  \  -debug        default to -O0; enables -no-inline; emits no DWARF info\n\
  \  -no-inline NAME  add LLVM noinline to NAME (debug only)\n\
  \  LLVM_OPT, LLVM_LLC, CC select tools (defaults: opt-22, llc-22, clang-22)\n\
  \  FAS_OPT, FAS_LLC, FAS_CC are fallback tool aliases\n\
  \  FAS_OPT_PASSES overrides opt's pipeline (default: default<Olevel>)\n\
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
          else if argv.(i + 1) = "-" then Error "-o - is not supported"
          else
            loop (i + 2) inputs
              (Some argv.(i + 1))
              emit keep optimization optimization_explicit debug no_inline_function
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
