(** stanc console application *)

open Core_kernel
open Frontend
open Stan_math_backend
open Analysis_and_optimization
open Middle

(** The main program. *)
let version = "%%NAME%%3 %%VERSION%%"

let name = "%%NAME%%"

(** The usage message. *)
let usage = "Usage: " ^ name ^ " [option] ... <model_file.stan>"

let model_file = ref ""
let pretty_print_program = ref false
let print_info_json = ref false
let filename_for_msg = ref ""
let canonicalize_program = ref false
let print_model_cpp = ref false
let dump_mir = ref false
let dump_mir_pretty = ref false
let dump_tx_mir = ref false
let dump_tx_mir_pretty = ref false
let dump_opt_mir = ref false
let dump_opt_mir_pretty = ref false
let dump_stan_math_sigs = ref false
let optimize = ref false
let output_file = ref ""
let generate_data = ref false
let warn_uninitialized = ref false
let warn_pedantic = ref false

(** Some example command-line options here *)
let options =
  Arg.align
    [ ( "--debug-lex"
      , Arg.Set Debugging.lexer_logging
      , " For debugging purposes: print the lexer actions" )
    ; ( "--debug-parse"
      , Arg.Set Debugging.grammar_logging
      , " For debugging purposes: print the parser actions" )
    ; ( "--debug-ast"
      , Arg.Set Debugging.ast_printing
      , " For debugging purposes: print the undecorated AST, before semantic \
         checking" )
    ; ( "--debug-decorated-ast"
      , Arg.Set Debugging.typed_ast_printing
      , " For debugging purposes: print the decorated AST, after semantic \
         checking" )
    ; ( "--debug-generate-data"
      , Arg.Set generate_data
      , " For debugging purposes: generate a mock dataset to run the model on"
      )
    ; ( "--debug-mir"
      , Arg.Set dump_mir
      , " For debugging purposes: print the MIR as an S-expression." )
    ; ( "--debug-mir-pretty"
      , Arg.Set dump_mir_pretty
      , " For debugging purposes: pretty-print the MIR." )
    ; ( "--debug-optimized-mir"
      , Arg.Set dump_opt_mir
      , " For debugging purposes: print the MIR after it's been optimized. \
         Only has an effect when optimizations are turned on." )
    ; ( "--debug-optimized-mir-pretty"
      , Arg.Set dump_opt_mir_pretty
      , " For debugging purposes: pretty print the MIR after it's been \
         optimized. Only has an effect when optimizations are turned on." )
    ; ( "--debug-transformed-mir"
      , Arg.Set dump_tx_mir
      , " For debugging purposes: print the MIR after the backend has \
         transformed it." )
    ; ( "--debug-transformed-mir-pretty"
      , Arg.Set dump_tx_mir_pretty
      , " For debugging purposes: pretty print the MIR after the backend has \
         transformed it." )
    ; ( "--dump-stan-math-signatures"
      , Arg.Set dump_stan_math_sigs
      , "Dump out the list of supported type signatures for Stan Math backend."
      )
    ; ( "--warn-uninitialized"
      , Arg.Set warn_uninitialized
      , " Emit warnings about uninitialized variables to stderr. Currently an \
         experimental feature." )
    ; ( "--warn-pedantic"
      , Arg.Set warn_pedantic
      , " Emit warnings about common mistakes in Stan programs." )
    ; ( "--auto-format"
      , Arg.Set pretty_print_program
      , " Pretty prints the program to the console" )
    ; ( "--print-canonical"
      , Arg.Set canonicalize_program
      , " Prints the canonicalized program to the console" )
    ; ( "--version"
      , Arg.Unit
          (fun _ ->
            print_endline (version ^ " " ^ "(" ^ Sys.os_type ^ ")") ;
            exit 0 )
      , " Display stanc version number" )
    ; ( "--name"
      , Arg.Set_string Semantic_check.model_name
      , " Take a string to set the model name (default = \
         \"$model_filename_model\")" )
    ; ( "--O"
      , Arg.Set optimize
      , "Allow the compiler to apply all optimizations to the Stan code." )
    ; ( "--o"
      , Arg.Set_string output_file
      , " Take the path to an output file for generated C++ code (default = \
         \"$name.hpp\")" )
    ; ( "--print-cpp"
      , Arg.Set print_model_cpp
      , " If set, output the generated C++ Stan model class to stdout." )
    ; ( "--allow-undefined"
      , Arg.Clear Semantic_check.check_that_all_functions_have_definition
      , " Do not fail if a function is declared but not defined" )
    ; ( "--allow_undefined"
      , Arg.Clear Semantic_check.check_that_all_functions_have_definition
      , " Deprecated. Same as --allow-undefined." )
    ; ( "--include-paths"
      , Arg.String
          (fun str ->
            Preprocessor.include_paths := String.split_on_chars ~on:[','] str
            )
      , " Takes a comma-separated list of directories that may contain a file \
         in an #include directive (default = \"\")" )
    ; ( "--include_paths"
      , Arg.String
          (fun str ->
            Preprocessor.include_paths :=
              !Preprocessor.include_paths @ String.split_on_chars ~on:[','] str
            )
      , " Deprecated. Same as --include-paths." )
    ; ( "--use-opencl"
      , Arg.Set Transform_Mir.use_opencl
      , " If set, try to use matrix_cl signatures." )
    ; ( "--standalone-functions"
      , Arg.Set Stan_math_code_gen.standalone_functions
      , " If set, the generated C++ will be the standalone functions C++ code."
      )
    ; ( "--filename-in-msg"
      , Arg.Set_string filename_for_msg
      , " Sets the filename used in compiler errors. Uses actual filename by \
         default." )
    ; ( "--info"
      , Arg.Set print_info_json
      , " If set, print information about the model." ) ]

let print_deprecated_arg_warning =
  (* is_prefix is used to also cover the --include-paths=... *)
  let arg_is_used arg =
    Array.mem ~equal:(fun x y -> String.is_prefix ~prefix:x y) Sys.argv arg
  in
  if arg_is_used "--allow_undefined" then
    eprintf "--allow_undefined is deprecated. Please use --allow-undefined.\n" ;
  if arg_is_used "--include_paths" then
    eprintf "--include_paths is deprecated. Please use --include-paths.\n"

let model_file_err () =
  Arg.usage options ("Please specify one model_file.\n\n" ^ usage) ;
  exit 127

let add_file filename =
  if !model_file = "" then model_file := filename else model_file_err ()

(*
      I am not using Fmt to print to stderr here because there was a pretty awful
      bug where it would unpredictably fail to flush. It would flush when using
      stdout or when trying to print some strings and not others. I tried using
      Fmt.flush and various other hacks to no avail. So now I use Fmt to build a
      string, and Out_channel to write it.
 *)

let pp_stderr formatter formatee =
  Fmt.strf "%a" formatter formatee |> Out_channel.(output_string stderr)

(** ad directives from the given file. *)
let use_file filename =
  let ast =
    Frontend_utils.get_ast_or_exit filename
      ~print_warnings:(not !canonicalize_program)
  in
  let ast =
    if !canonicalize_program then Canonicalize.repair_syntax ast else ast
  in
  Debugging.ast_logger ast ;
  if !pretty_print_program then
    print_endline (Pretty_printing.pretty_print_program ast) ;
  let typed_ast = Frontend_utils.type_ast_or_exit ast in
  if !print_info_json then (
    print_endline (Info.info typed_ast) ;
    exit 0 ) ;
  let printed_filename =
    match !filename_for_msg with "" -> None | s -> Some s
  in
  Warnings.pp_warnings Fmt.stderr ?printed_filename
    (Deprecation_analysis.collect_warnings typed_ast) ;
  if !canonicalize_program then
    print_endline
      (Pretty_printing.pretty_print_typed_program
         (Canonicalize.canonicalize_program typed_ast)) ;
  if !generate_data then
    print_endline (Debug_data_generation.print_data_prog typed_ast) ;
  Debugging.typed_ast_logger typed_ast ;
  if not (!pretty_print_program || !canonicalize_program) then (
    let mir = Ast_to_Mir.trans_prog filename typed_ast in
    if !dump_mir then
      Sexp.pp_hum Format.std_formatter [%sexp (mir : Middle.Program.Typed.t)] ;
    if !dump_mir_pretty then Program.Typed.pp Format.std_formatter mir ;
    if !warn_pedantic then
      Pedantic_analysis.warn_pedantic mir
      |> pp_stderr (Warnings.pp_warnings ?printed_filename)
    else if !warn_uninitialized then
      Pedantic_analysis.warn_uninitialized mir
      |> pp_stderr (Warnings.pp_warnings ?printed_filename) ;
    let tx_mir = Transform_Mir.trans_prog mir in
    if !dump_tx_mir then
      Sexp.pp_hum Format.std_formatter
        [%sexp (tx_mir : Middle.Program.Typed.t)] ;
    if !dump_tx_mir_pretty then Program.Typed.pp Format.std_formatter tx_mir ;
    let opt_mir =
      if !optimize then (
        let opt = Optimize.optimization_suite tx_mir in
        if !dump_opt_mir then
          Sexp.pp_hum Format.std_formatter
            [%sexp (opt : Middle.Program.Typed.t)] ;
        if !dump_opt_mir_pretty then Program.Typed.pp Format.std_formatter opt ;
        opt )
      else tx_mir
    in
    let cpp = Fmt.strf "%a" Stan_math_code_gen.pp_prog opt_mir in
    Out_channel.write_all !output_file ~data:cpp ;
    if !print_model_cpp then print_endline cpp )

let remove_dotstan s = String.drop_suffix s 5

let mangle =
  String.concat_map ~f:(fun c ->
      Char.(
        if is_alphanum c || c = '_' then to_string c
        else match c with '-' -> "_" | _ -> "x" ^ Int.to_string (to_int c)) )

let main () =
  (* Parse the arguments. *)
  Arg.parse options add_file usage ;
  print_deprecated_arg_warning ;
  (* print_deprecated_arg_warning options; *)
  (* Deal with multiple modalities *)
  if !dump_stan_math_sigs then (
    Stan_math_signatures.pretty_print_all_math_sigs Format.std_formatter () ;
    exit 0 ) ;
  (* Just translate a stan program *)
  if !model_file = "" then model_file_err () ;
  if !Semantic_check.model_name = "" then
    Semantic_check.model_name :=
      mangle
        (remove_dotstan List.(hd_exn (rev (String.split !model_file ~on:'/'))))
      ^ "_model"
  else Semantic_check.model_name := mangle !Semantic_check.model_name ;
  if !output_file = "" then output_file := remove_dotstan !model_file ^ ".hpp" ;
  use_file !model_file

let () = main ()
