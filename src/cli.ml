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

let usage =
  "usage: fas [options] file.fas [C inputs and link flags]\n\
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

let default_output emit input =
  let source_output extension =
    Filename.remove_extension (Filename.basename input) ^ extension
  in
  match emit with Asm -> source_output ".s" | Obj -> source_output ".o" | _ -> "a.out"

let parse argv =
  let n = Array.length argv in
  let link_inputs_rev = ref [] in
  let add_link_inputs values =
    link_inputs_rev := List.rev_append values !link_inputs_rev
  in
  let passthrough_file file =
    List.exists (Filename.check_suffix file) [ ".c"; ".o"; ".a"; ".so" ]
  in
  let rec loop i input output emit keep optimization optimization_explicit debug
      no_inline_function =
    if i >= n then
      if Option.is_none input then Error "no input files"
      else if !link_inputs_rev <> [] && emit <> Executable then
        Error "C inputs and link flags require an executable output"
      else if Option.is_some no_inline_function && not debug then
        Error "-no-inline requires -debug"
      else
        let input = Option.get input in
        let output_explicit = Option.is_some output in
        let output =
          match output with Some path -> path | None -> default_output emit input
        in
        Ok
          (Run
             {
               input;
               output;
               output_explicit;
               emit;
               keep;
               optimization;
               debug;
               no_inline_function;
               link_inputs = List.rev !link_inputs_rev;
             })
    else
      match argv.(i) with
      | "--help" | "-h" -> Ok Help
      | "-o" ->
          if i + 1 >= n then Error "-o requires an output path"
          else if argv.(i + 1) = "-" then Error "-o - is not supported"
          else
            loop (i + 2) input
              (Some argv.(i + 1))
              emit keep optimization optimization_explicit debug no_inline_function
      | "--emit-ir" ->
          loop (i + 1) input output Ir keep optimization optimization_explicit debug
            no_inline_function
      | "--emit-llvm" ->
          loop (i + 1) input output Llvm keep optimization optimization_explicit debug
            no_inline_function
      | "--emit-asm" | "-S" ->
          loop (i + 1) input output Asm keep optimization optimization_explicit debug
            no_inline_function
      | "--emit-obj" | "-c" ->
          loop (i + 1) input output Obj keep optimization optimization_explicit debug
            no_inline_function
      | "--keep" ->
          loop (i + 1) input output emit true optimization optimization_explicit debug
            no_inline_function
      | "-debug" ->
          loop (i + 1) input output emit keep
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
              loop (i + 2) input output emit keep optimization optimization_explicit
                debug (Some name)
      | ("-l" | "-L") as flag ->
          if i + 1 >= n || argv.(i + 1) = "" then Error (flag ^ " requires an argument")
          else (
            add_link_inputs [ flag; argv.(i + 1) ];
            loop (i + 2) input output emit keep optimization optimization_explicit debug
              no_inline_function)
      | flag
        when (String.length flag > 2 && String.sub flag 0 2 = "-l")
             || (String.length flag > 2 && String.sub flag 0 2 = "-L") ->
          add_link_inputs [ flag ];
          loop (i + 1) input output emit keep optimization optimization_explicit debug
            no_inline_function
      | flag
        when String.length flag = 3
             && flag.[0] = '-'
             && flag.[1] = 'O'
             && flag.[2] >= '0'
             && flag.[2] <= '3' ->
          loop (i + 1) input output emit keep
            (Char.code flag.[2] - Char.code '0')
            true debug no_inline_function
      | flag when String.length flag > 0 && flag.[0] = '-' ->
          Error ("unknown option: " ^ flag)
      | file when passthrough_file file ->
          add_link_inputs [ file ];
          loop (i + 1) input output emit keep optimization optimization_explicit debug
            no_inline_function
      | file -> (
          match input with
          | None ->
              loop (i + 1) (Some file) output emit keep optimization
                optimization_explicit debug no_inline_function
          | Some _ ->
              Error
                "multiple input files are not supported; use \"path.fas\" for \
                 dependencies")
  in
  loop 1 None None Executable false 2 false false None
