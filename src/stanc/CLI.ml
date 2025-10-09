open Core
open Frontend
open Cmdliner

module Arguments = struct
  (** Positional arguments *)

  let model_file =
    let doc =
      "The Stan model file to compile. This should be a file ending in \
       $(b,.stan) or $(b,.stanfunctions) (which automatically sets \
       $(b,--standalone-functions)), or '-' to read from standard input." in
    Arg.(
      let dash_or_file =
        let else_parse = conv_parser non_dir_file in
        let pp = conv_printer non_dir_file in
        let parse s = if String.equal s "-" then Ok s else else_parse s in
        conv (parse, pp) in
      value & pos 0 (some dash_or_file) None & info [] ~docv:"MODEL_FILE" ~doc)
end

module Options = struct
  (** Normal, user-facing options and flags *)

  let qmark : Manpage.format option Term.t =
    Arg.(
      value
      & opt ~vopt:(Some `Auto)
          (some
             (enum
                [ ("auto", `Auto); ("pager", `Pager); ("plain", `Plain)
                ; ("groff", `Groff) ]))
          None
      & info ["?"] ~doc:"Synonym for $(b,--help)." ~docv:"=FMT")

  (** Wrapper around flag_all to support passing the same flag multiple times. Emits a warning *)
  let multi_flag name arg_info =
    let flags = Arg.(value & flag_all & arg_info [name]) in
    Term.map
      (fun fs ->
        match List.length fs with
        | 0 -> false
        | 1 -> true
        | _ ->
            Printf.eprintf
              "Warning: Duplicated flag '--%s' ignored, consider updating your \
               call to stanc!"
              name;
            true)
      flags

  let allow_undefined =
    let doc =
      "Do not fail if a function is declared but not defined. This usually \
       means the definition will be provided later as a C++ function." in
    Arg.(multi_flag "allow-undefined" & info ~doc)

  let auto_format =
    let doc =
      "Output a formatted version of the Stan program. The output can be \
       tweaked using $(b,--max-line-length) and $(b,--canonicalize)." in
    Arg.(multi_flag "auto-format" & info ~doc)

  let canonicalizer_settings =
    let fold_canonicalize_options
        (settings : Canonicalize.canonicalizer_settings) = function
      | `Deprecations -> {settings with deprecations= true}
      | `Parentheses -> {settings with parentheses= true}
      | `Braces -> {settings with braces= true}
      | `Includes -> {settings with inline_includes= true}
      | `Strip_comments -> {settings with strip_comments= true} in
    let doc =
      "Enable specific canonicalizations in a comma separated list. Options \
       are 'deprecations', 'parentheses', 'braces', 'includes', \
       'strip-comments'." in
    let base_arg =
      Arg.enum
        [ ("deprecations", `Deprecations); ("parentheses", `Parentheses)
        ; ("braces", `Braces); ("includes", `Includes)
        ; ("strip-comments", `Strip_comments) ] in
    let make_case_insensitive converter =
      Arg.(
        conv
          ( Fn.compose (conv_parser converter) String.lowercase
          , conv_printer converter )) in
    Term.(
      const (List.fold ~f:fold_canonicalize_options ~init:Canonicalize.none)
      $ Arg.(
          value
          & opt (list (make_case_insensitive base_arg)) []
          & info ["canonicalize"] ~doc ~docv:"OPTIONS"))

  let filename_in_msg =
    let doc = "Sets the filename used in compiler and runtime errors. " in
    Arg.(
      value
      & opt (some string) None
      & info ["filename-in-msg"] ~absent:"$(b,MODEL_FILE)" ~doc ~docv:"FILENAME")

  let include_paths =
    let doc =
      "A comma-separated list of directories which are searched whenever an \
       #include directive is parsed." in
    Term.(
      const (fun p -> Frontend.Include_files.FileSystemPaths p)
      $ Arg.(
          value
          & opt (list string) []
          & info ["include-paths"] ~doc ~absent:"\"\"" ~docv:"DIRS"))

  let info =
    let doc = "If set, print information about the model." in
    Arg.(multi_flag "info" & info ~doc)

  let max_line_length =
    let doc = "Set the maximum line length for the formatter." in
    Arg.(value & opt int 78 & info ["max-line-length"] ~doc ~docv:"LENGTH")

  let name =
    let doc =
      "Take a string to set as the model name. This controls the namespace in \
       the generated C++." in
    Arg.(
      value
      & opt (some string) None
      & info ["name"] ~doc ~absent:"$(b,MODEL_FILE_NAME)_model" ~docv:"NAME")

  let output_file =
    let doc =
      "Output file for generated C++ code (default = \
       \"$(b,MODEL_FILE_NAME).hpp\") or auto-formatting output (default: no \
       file/print to stdout)." in
    Arg.(value & opt string "" & info ["o"; "output"] ~doc ~docv:"FILENAME")

  let o0 =
    let doc = "$(i,(Default)) Do not apply optimizations to the Stan code." in
    Arg.(multi_flag "O0" & info ~doc)

  let o1 =
    let doc =
      "Only basic optimizations are applied. Recommended. The deprecated \
       option $(b,--O) is aliased to this starting in Stan 2.37." in
    Arg.(multi_flag "O1" & info ~doc)

  let oexperimental =
    let doc =
      "$(i,(Experimental)) Apply all compiler optimizations. Some of these are \
       not thorougly tested and may not always improve a programs performance."
    in
    Arg.(multi_flag "Oexperimental" & info ~doc)

  let print_canonical =
    let doc =
      "Prints the canonicalized program. Equivalent to $(b,--auto-format \
       --canonicalize=deprecations,includes,parentheses,braces)." in
    Arg.(multi_flag "print-canonical" & info ~doc)

  let print_cpp =
    let doc = "If set, output the generated C++ Stan model class to stdout." in
    Arg.(multi_flag "print-cpp" & info ~doc)

  let standalone_functions =
    let doc =
      "If set, the generated C++ will only contain the code for the functions \
       in the functions block, not the full Stan model class." in
    Arg.(multi_flag "standalone-functions" & info ~doc)

  let use_opencl =
    let doc =
      "If set, try to use matrix_cl signatures for supported Stan Math \
       functions." in
    Arg.(multi_flag "use-opencl" & info ~doc)

  let warn_pedantic =
    let doc =
      "Emit warnings about common mistakes in Stan programs. $(i,Note:) This \
       may produce false positive warnings." in
    Arg.(multi_flag "warn-pedantic" & info ~doc)

  let warn_uninitialized =
    let doc =
      "$(i,(Experimental)) Emit warnings about uninitialized variables." in
    Arg.(multi_flag "warn-uninitialized" & info ~doc)
end

module Commands = struct
  (** These would be subcommands if we hadn't originally made
    them options. They change 'modes' and do not need a model file *)

  let dump_stan_math_signatures =
    let doc =
      "Dump out the list of supported function signatures the for Stan Math \
       backend." in
    Arg.(
      value & flag
      & info ["dump-stan-math-signatures"] ~doc ~docs:Manpage.s_commands)

  let dump_stan_math_distributions =
    let doc =
      "Dump out the list of supported probability distributions and their \
       supported suffix types for the Stan Math backend." in
    Arg.(
      value & flag
      & info ["dump-stan-math-distributions"] ~doc ~docs:Manpage.s_commands)
end

module Debug_Options = struct
  (** Options that are developer-facing or for debugging *)

  let section = "EXTRA OPTIONS"
  let docs = section

  let debug_ast =
    let doc =
      "For debugging purposes: print the undecorated AST, before semantic \
       checking." in
    Arg.(value & flag & info ["debug-ast"] ~doc ~docs)

  let debug_data_json : string option Term.ret Term.t =
    let doc =
      "Provide (possibly partially specified) data block values for use with \
       $(b,--debug-generate-data) or $(b,--debug-generate-inits)." in
    Term.(
      const (function
        | None -> `Ok None
        | Some file -> (
            try `Ok (Some (In_channel.read_all file))
            with _ ->
              `Error (true, "File '" ^ file ^ "' not found or cannot be opened.")
            ))
      $ Arg.(
          value
          & opt (some non_dir_file) None
          & info ["debug-data-file"] ~doc ~docv:"JSON_FILE" ~docs))

  let debug_decorated_ast =
    let doc =
      "For debugging purposes: print the decorated AST, after semantic \
       checking." in
    Arg.(value & flag & info ["debug-decorated-ast"] ~doc ~docs)

  let debug_generate_data =
    let doc =
      "For debugging purposes: generate a mock dataset to run the model on."
    in
    Arg.(value & flag & info ["debug-generate-data"] ~doc ~docs)

  let debug_generate_inits =
    let doc =
      "For debugging purposes: generate a mock initial value for each \
       parameter." in
    Arg.(value & flag & info ["debug-generate-inits"] ~doc ~docs)

  let debug_lex =
    let doc = "For debugging purposes: print the lexer actions." in
    Arg.(value & flag & info ["debug-lex"] ~doc ~docs)

  let debug_lir =
    let doc =
      "For debugging purposes: print the C++ LIR as a s-expression. Mainly for \
       comparison with $(b,--print-cpp)." in
    Arg.(value & flag & info ["debug-lir"] ~doc ~docs)

  let debug_mem_patterns =
    let doc =
      "For debugging purposes: print a list of matrix variables and their \
       memory type, either AoS (array of structs) or the more efficient SoA \
       (struct of arrays). Only has an effect when optimizations are turned \
       on." in
    Arg.(value & flag & info ["debug-mem-patterns"] ~doc ~docs)

  (** helper for paired args like --debug-mir and --debug-mir-pretty *)
  let debug_basic_or_pretty ~doc flag_name : Driver.Flags.debug_options Term.t =
    let doc_pretty =
      String.substr_replace_all ~pattern:"print" ~with_:"pretty-print" doc in
    Arg.(
      value
      & vflag Driver.Flags.Off
          [ (Basic, info [flag_name] ~doc ~docs)
          ; (Pretty, info [flag_name ^ "-pretty"] ~doc:doc_pretty ~docs) ])

  let debug_mir =
    let doc = "For debugging purposes: print the MIR after lowering." in
    debug_basic_or_pretty ~doc "debug-mir"

  let debug_optimized_mir =
    let doc =
      "For debugging purposes: print the MIR after it's been optimized. Only \
       has an effect when optimizations are turned on." in
    debug_basic_or_pretty ~doc "debug-optimized-mir"

  let debug_parse =
    let doc = "For debugging purposes: print the parser actions." in
    Arg.(value & flag & info ["debug-parse"] ~doc ~docs)

  let debug_transformed_mir =
    let doc =
      "For debugging purposes: print the MIR after the backend has transformed \
       it." in
    debug_basic_or_pretty ~doc "debug-transformed-mir"

  let force_soa =
    let doc =
      "Debugging features. Valid values: $(b,-fsoa) to force on the Struct of \
       Arrays optimization. $(b,-fno-soa) to force it off." in
    let soa_conv =
      Arg.(conv_parser (enum [("soa", Some true); ("no-soa", Some false)]))
    in
    let soa_printer pf = function
      | Some true -> Fmt.string pf "soa"
      | Some false -> Fmt.string pf "no-soa"
      | None -> Fmt.string pf "" in
    Arg.(
      value
      & opt (conv (soa_conv, soa_printer)) None
      & info ["f"] ~doc ~docv:"SETTING" ~docs)
end

(** Flags common to all compiler drivers and those specific to the command line *)
type compiler_flags =
  { debug_lex: bool
  ; debug_parse: bool
  ; print_cpp: bool
  ; name: string option
  ; output_file: string
  ; tty_colors: Fmt.style_renderer option
  ; flags: Driver.Flags.t
  ; model_file: string }

open Cmdliner.Term.Syntax

module Conversion = struct
  (** Helper terms to combine the above arguments into useful ocaml values *)

  let optimization_level :
      Analysis_and_optimization.Optimize.optimization_level Term.t =
    let+ o0 = Options.o0
    and+ o1 = Options.o1
    and+ oexperimental = Options.oexperimental in
    if o0 then Analysis_and_optimization.Optimize.O0
    else if o1 then O1
    else if oexperimental then Oexperimental
    else O0

  let debug_settings : Driver.Flags.debug_settings Term.t =
    let open Debug_Options in
    let+ print_ast = debug_ast
    and+ print_typed_ast = debug_decorated_ast
    and+ print_mir = debug_mir
    and+ print_transformed_mir = debug_transformed_mir
    and+ print_optimized_mir = debug_optimized_mir
    and+ print_mem_patterns = debug_mem_patterns
    and+ force_soa = force_soa
    and+ print_lir = debug_lir
    and+ debug_generate_data = debug_generate_data
    and+ debug_generate_inits = debug_generate_inits
    and+ debug_data_json = Term.ret debug_data_json in
    Driver.Flags.
      { print_ast
      ; print_typed_ast
      ; print_mir
      ; print_transformed_mir
      ; print_optimized_mir
      ; print_mem_patterns
      ; force_soa
      ; print_lir
      ; debug_generate_data
      ; debug_generate_inits
      ; debug_data_json }

  let flags : Driver.Flags.t Term.t =
    let open Options in
    let+ optimization_level = optimization_level
    and+ allow_undefined = allow_undefined
    and+ standalone_functions = standalone_functions
    and+ use_opencl = use_opencl
    and+ include_source = include_paths
    and+ info = info
    and+ auto_format = auto_format
    and+ line_length = max_line_length
    and+ print_canonical = print_canonical
    and+ canonicalizer_settings = canonicalizer_settings
    and+ warn_pedantic = warn_pedantic
    and+ warn_uninitialized = warn_uninitialized
    and+ filename_in_msg = filename_in_msg
    and+ debug_settings = debug_settings in
    Driver.Flags.
      { optimization_level
      ; allow_undefined
      ; functions_only= false
      ; standalone_functions
      ; use_opencl
      ; include_source
      ; info
      ; version= false
      ; auto_format= auto_format || print_canonical
      ; line_length
      ; canonicalizer_settings=
          (if print_canonical then Canonicalize.legacy
           else canonicalizer_settings)
      ; warn_pedantic
      ; warn_uninitialized
      ; filename_in_msg
      ; debug_settings }
end

(* The overarching command line.
   Either we're in one of the dump sub-commands,
   or we have all the flags we need to proceed *)
let commands =
  Term.ret
  @@
  let+ qmark = Options.qmark
  and+ dump_stan_math_distributions = Commands.dump_stan_math_distributions
  and+ dump_stan_math_sigs = Commands.dump_stan_math_signatures
  and+ debug_lex = Debug_Options.debug_lex
  and+ debug_parse = Debug_Options.debug_parse
  and+ print_cpp = Options.print_cpp
  and+ name = Options.name
  and+ output_file = Options.output_file
  and+ model_file = Arguments.model_file
  and+ flags = Conversion.flags
  and+ tty_colors =
    Fmt_cli.style_renderer ~env:(Cmd.Env.info "STANC_COLORS") () in
  match qmark with
  | Some fmt -> `Help (fmt, None)
  | None -> (
      if dump_stan_math_distributions then `Ok `DumpMathDists
      else if dump_stan_math_sigs then `Ok `DumpMathSigs
      else
        match model_file with
        | None -> `Error (true, "No model file provided")
        | Some model_file ->
            `Ok
              (`Default
                { debug_lex
                ; debug_parse
                ; print_cpp
                ; name
                ; output_file
                ; model_file
                ; tty_colors
                ; flags }))

let exit_ok = 0
let exit_err = 1
let exit_ice = 125

let info =
  let doc = "compile Stan programs to C++" in
  let man =
    [ `S Manpage.s_description
    ; `P
        "The Stan compiler (also known as $(i,stanc) or $(i,stanc3)) reads a \
         Stan file and compiles it to C++. It also allows for other Stan \
         program manipulation like formatting ($(b,--auto-format)) and \
         introspection ($(b,--info))."
    ; `P "For more information on Stan, see https://mc-stan.org."
    ; `P
        "For more documentation on the compiler for users, see \
         https://mc-stan.org/docs/stan-users-guide/using-stanc.html."
    ; `P
        "For more information on the compiler for developers, see \
         https://mc-stan.org/stanc3/stanc/."; `S Manpage.s_arguments
    ; `S Manpage.s_options; `S Manpage.s_commands
    ; `P
        "The following flags will cause the compiler to exit after printing \
         information. No $(b,MODEL_FILE) is required."; `S Debug_Options.section
    ; `P
        "These flags are provided primarily for development and debugging; \
         their exact behavior should not be relied on."
    ; `S Manpage.s_exit_status; `S Manpage.s_bugs
    ; `P "Please report at https://github.com/stan-dev/stanc3/issues/new." ]
  in
  let exits =
    Cmd.Exit.
      [ info ~doc:"on success." exit_ok
      ; info ~doc:"on compilation failure." exit_err
      ; info ~doc:"on command line parsing errors." 124
      ; info ~doc:"on internal compiler errors. Please file a bug!" exit_ice ]
  in
  Cmd.info "%%NAME%%"
    ~version:("%%NAME%%3 %%VERSION%%" ^ " (" ^ Sys.os_type ^ ")")
    ~sdocs:Manpage.s_options ~doc ~man ~exits
