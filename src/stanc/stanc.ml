(** stanc console application *)
open Core

open Frontend
module Optimize = Analysis_and_optimization.Optimize
module Stan_math_signatures = Middle.Stan_math_signatures

(** The name of the executable. *)
let name = "%%NAME%%"

(** The usage message. *)
let usage = "Usage: " ^ name ^ " [option] ... <model_file.stan[functions]>"

(* some flags aren't available in other drivers / don't make sense there *)
let dump_stan_math_sigs = ref false
let dump_stan_math_distributions = ref false
let model_file = ref ""
let output_file = ref ""
let print_model_cpp = ref false

(* but most flags are stored here to be handled by the driver *)
let driver_flags = ref Driver.Flags.default

let parse_canonical_options (settings : Canonicalize.canonicalizer_settings)
    string =
  match String.lowercase string with
  | "deprecations" -> {settings with deprecations= true}
  | "parentheses" -> {settings with parentheses= true}
  | "braces" -> {settings with braces= true}
  | "includes" -> {settings with inline_includes= true}
  | "strip-comments" -> {settings with strip_comments= true}
  | s ->
      raise
      @@ Arg.Bad
           ("Unrecognized canonicalizer option '" ^ s
          ^ "'. \n\
             Should be one of 'deprecations', 'parentheses', 'braces', \
             'includes', 'strip-comments'")

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
      , Arg.Unit
          (fun () ->
            driver_flags :=
              { !driver_flags with
                debug_settings=
                  {!driver_flags.debug_settings with debug_ast= true} })
      , " For debugging purposes: print the undecorated AST, before semantic \
         checking" )
    ; ( "--debug-decorated-ast"
      , Arg.Unit
          (fun () ->
            driver_flags :=
              { !driver_flags with
                debug_settings=
                  {!driver_flags.debug_settings with debug_typed_ast= true} })
      , " For debugging purposes: print the decorated AST, after semantic \
         checking" )
    ; ( "--debug-generate-data"
      , Arg.Unit
          (fun () ->
            driver_flags :=
              { !driver_flags with
                debug_settings=
                  {!driver_flags.debug_settings with debug_generate_data= true}
              })
      , " For debugging purposes: generate a mock dataset to run the model on"
      )
    ; ( "--debug-generate-inits"
      , Arg.Unit
          (fun () ->
            driver_flags :=
              { !driver_flags with
                debug_settings=
                  {!driver_flags.debug_settings with debug_generate_inits= true}
              })
      , " For debugging purposes: generate a mock initial value for each \
         parameter" )
    ; ( "--debug-data-file"
      , Arg.String
          (fun s ->
            driver_flags :=
              { !driver_flags with
                debug_settings=
                  { !driver_flags.debug_settings with
                    debug_data_json= Some (In_channel.read_all s) } })
      , " For --debug-generate-data or --debug-generate-inits" )
    ; ( "--debug-mir"
      , Arg.Unit
          (fun () ->
            driver_flags :=
              { !driver_flags with
                debug_settings=
                  {!driver_flags.debug_settings with debug_mir= Basic} })
      , " For debugging purposes: print the MIR as an S-expression." )
    ; ( "--debug-mir-pretty"
      , Arg.Unit
          (fun () ->
            driver_flags :=
              { !driver_flags with
                debug_settings=
                  {!driver_flags.debug_settings with debug_mir= Pretty} })
      , " For debugging purposes: pretty-print the MIR." )
    ; ( "--debug-optimized-mir"
      , Arg.Unit
          (fun () ->
            driver_flags :=
              { !driver_flags with
                debug_settings=
                  {!driver_flags.debug_settings with debug_optimized_mir= Basic}
              })
      , " For debugging purposes: print the MIR after it's been optimized. \
         Only has an effect when optimizations are turned on." )
    ; ( "--debug-optimized-mir-pretty"
      , Arg.Unit
          (fun () ->
            driver_flags :=
              { !driver_flags with
                debug_settings=
                  {!driver_flags.debug_settings with debug_optimized_mir= Pretty}
              })
      , " For debugging purposes: pretty print the MIR after it's been \
         optimized. Only has an effect when optimizations are turned on." )
    ; ( "--debug-lir"
      , Arg.Unit
          (fun () ->
            driver_flags :=
              { !driver_flags with
                debug_settings=
                  {!driver_flags.debug_settings with debug_lir= true} })
      , " For debugging purposes: print the C++ LIR as a s-expression. Mainly \
         for comparison with --print-cpp" )
    ; ( "--debug-mem-patterns"
      , Arg.Unit
          (fun () ->
            driver_flags :=
              { !driver_flags with
                debug_settings=
                  {!driver_flags.debug_settings with debug_mem_patterns= true}
              })
      , " For debugging purposes: print a list of matrix variables and their \
         memory type, either AoS (array of structs) or the more efficient SoA \
         (struct of arrays). Only has an effect when optimizations are turned \
         on." )
    ; ( "--debug-transformed-mir"
      , Arg.Unit
          (fun () ->
            driver_flags :=
              { !driver_flags with
                debug_settings=
                  { !driver_flags.debug_settings with
                    debug_transformed_mir= Basic } })
      , " For debugging purposes: print the MIR after the backend has \
         transformed it." )
    ; ( "--debug-transformed-mir-pretty"
      , Arg.Unit
          (fun () ->
            driver_flags :=
              { !driver_flags with
                debug_settings=
                  { !driver_flags.debug_settings with
                    debug_transformed_mir= Pretty } })
      , " For debugging purposes: pretty print the MIR after the backend has \
         transformed it." )
    ; ( "--dump-stan-math-signatures"
      , Arg.Set dump_stan_math_sigs
      , " Dump out the list of supported type signatures for Stan Math backend."
      )
    ; ( "--dump-stan-math-distributions"
      , Arg.Set dump_stan_math_distributions
      , " Dump out the list of supported probability distributions and their \
         supported suffix types for the Stan Math backend." )
    ; ( "--warn-uninitialized"
      , Arg.Unit
          (fun () ->
            driver_flags := {!driver_flags with warn_uninitialized= true})
      , " Emit warnings about uninitialized variables to stderr. Currently an \
         experimental feature." )
    ; ( "--warn-pedantic"
      , Arg.Unit
          (fun () -> driver_flags := {!driver_flags with warn_pedantic= true})
      , " Emit warnings about common mistakes in Stan programs." )
    ; ( "--auto-format"
      , Arg.Unit
          (fun () -> driver_flags := {!driver_flags with auto_format= true})
      , " Pretty prints a formatted version of the Stan program." )
    ; ( "--canonicalize"
      , Arg.String
          (fun s ->
            let canonicalizer_settings =
              List.fold ~f:parse_canonical_options
                ~init:!driver_flags.canonicalizer_settings
                (String.split s ~on:',') in
            driver_flags := {!driver_flags with canonicalizer_settings})
      , " Enable specific canonicalizations in a comma separated list. Options \
         are 'deprecations', 'parentheses', 'braces', 'includes', \
         'strip-comments'." )
    ; ( "--max-line-length"
      , Arg.Int
          (fun line_length -> driver_flags := {!driver_flags with line_length})
      , " Set the maximum line length for the formatter. Defaults to 78 \
         characters." )
    ; ( "--print-canonical"
      , Arg.Unit
          (fun () ->
            driver_flags :=
              { !driver_flags with
                auto_format= true
              ; canonicalizer_settings= Canonicalize.legacy })
      , " Prints the canonicalized program. Equivalent to --auto-format \
         --canonicalize deprecations,includes,parentheses,braces" )
    ; ( "--version"
      , Arg.Unit (fun () -> driver_flags := {!driver_flags with version= true})
      , " Display stanc version number" )
    ; ( "--name"
      , Arg.Set_string Typechecker.model_name
      , " Take a string to set the model name (default = \
         \"$model_filename_model\")" )
    ; ( "--O0"
      , Arg.Unit
          (fun () ->
            driver_flags := {!driver_flags with optimization_level= Optimize.O0})
      , "\t(Default) Do not apply optimizations to the Stan code." )
    ; ( "--O1"
      , Arg.Unit
          (fun () ->
            driver_flags := {!driver_flags with optimization_level= Optimize.O1})
      , "\tApply level 1 compiler optimizations (only basic optimizations)." )
    ; ( "--Oexperimental"
      , Arg.Unit
          (fun () ->
            driver_flags :=
              {!driver_flags with optimization_level= Optimize.Oexperimental})
      , "\t(Experimental) Apply all compiler optimizations. Some of these are \
         not thorougly tested and may not always improve a programs \
         performance." )
    ; ( "--O"
      , Arg.Unit
          (fun () ->
            driver_flags := {!driver_flags with optimization_level= Optimize.O1})
      , "\tSame as --O1." )
    ; ( "-fno-soa"
      , Arg.Unit
          (fun () ->
            driver_flags :=
              { !driver_flags with
                debug_settings=
                  { !driver_flags.debug_settings with
                    debug_manual_soa= Some false } })
      , "\tTurn off the Struct of Arrays optimization" )
    ; ( "-fsoa"
      , Arg.Unit
          (fun () ->
            driver_flags :=
              { !driver_flags with
                debug_settings=
                  {!driver_flags.debug_settings with debug_manual_soa= Some true}
              })
      , "\tTurn on the Struct of Arrays optimization" )
    ; ( "--o"
      , Arg.Set_string output_file
      , " Take the path to an output file for generated C++ code (default = \
         \"$name.hpp\") or auto-formatting output (default: no file/print to \
         stdout)" )
    ; ( "--print-cpp"
      , Arg.Set print_model_cpp
      , " If set, output the generated C++ Stan model class to stdout." )
    ; ( "--allow-undefined"
      , Arg.Unit
          (fun () -> driver_flags := {!driver_flags with allow_undefined= true})
      , " Do not fail if a function is declared but not defined" )
    ; ( "--include-paths"
      , Arg.String
          (fun str ->
            driver_flags :=
              { !driver_flags with
                include_source=
                  Include_files.FileSystemPaths
                    (String.split_on_chars ~on:[','] str) })
      , " Takes a comma-separated list of directories that may contain a file \
         in an #include directive (default = \"\")" )
    ; ( "--use-opencl"
      , Arg.Unit
          (fun () -> driver_flags := {!driver_flags with use_opencl= true})
      , " If set, try to use matrix_cl signatures." )
    ; ( "--standalone-functions"
      , Arg.Unit
          (fun () ->
            driver_flags := {!driver_flags with standalone_functions= true})
      , " If set, the generated C++ will be the standalone functions C++ code."
      )
    ; ( "--filename-in-msg"
      , Arg.String
          (fun filename_in_msg ->
            driver_flags :=
              {!driver_flags with filename_in_msg= Some filename_in_msg})
      , " Sets the filename used in compiler errors. Uses actual filename by \
         default." )
    ; ( "--info"
      , Arg.Unit (fun () -> driver_flags := {!driver_flags with info= true})
      , " If set, print information about the model." ) ]

let model_file_err () =
  Arg.usage options ("Please specify a model_file.\n" ^ usage);
  exit 127

let add_file filename =
  if String.equal !model_file "" then model_file := filename
  else raise (Arg.Bad "Please specify only one model_file")

let print_or_write_and_exit data =
  if not (String.equal !output_file "") then
    Out_channel.write_all !output_file ~data
  else print_endline data;
  exit 0

let print_and_exit data =
  print_endline data;
  exit 0

let output_callback printed_filename : Driver.Entry.other_output -> unit =
  function
  | Info s -> print_and_exit s
  | Version s -> print_and_exit (s ^ "(" ^ Sys.os_type ^ ")")
  | Generated s | Formatted s ->
      (* these options will use the --o flag if it was passed *)
      print_or_write_and_exit s
  | DebugOutput s | Memory_patterns s ->
      (* historically, these flags didn't prevent you from continuing *)
      print_string s
  | Warnings ws -> Warnings.pp_warnings Fmt.stderr ?printed_filename ws

let main () =
  (* Parse the arguments. *)
  Arg.parse options add_file usage;
  (* Deal with multiple modalities *)
  if !dump_stan_math_sigs then (
    Stan_math_signatures.pretty_print_all_math_sigs Format.std_formatter ();
    exit 0);
  if !dump_stan_math_distributions then (
    Stan_math_signatures.pretty_print_all_math_distributions
      Format.std_formatter ();
    exit 0);
  if String.equal !model_file "" && not !driver_flags.version then
    model_file_err ();
  Driver.Flags.set_backend_args_list
    (* remove executable itself from list before passing *)
    (Sys.get_argv () |> Array.to_list |> List.tl_exn);
  (* if we only have functions, always compile as standalone *)
  if String.is_suffix !model_file ~suffix:".stanfunctions" then
    driver_flags :=
      {!driver_flags with standalone_functions= true; functions_only= true};
  match
    Driver.Entry.stan2cpp !model_file (`File !model_file) !driver_flags
      (output_callback !driver_flags.filename_in_msg)
  with
  | Ok cpp_str ->
      if String.equal !output_file "" then
        output_file := Driver.Flags.remove_dotstan !model_file ^ ".hpp";
      Out_channel.write_all !output_file ~data:cpp_str;
      if !print_model_cpp then print_endline cpp_str
  | Error (DebugDataError _ as e) ->
      (* separated out to suggest the possibly-fixing flag *)
      Errors.pp Fmt.stderr ?printed_filename:!driver_flags.filename_in_msg e;
      if Option.is_none !driver_flags.debug_settings.debug_data_json then
        Fmt.pf Fmt.stderr "Supplying a --debug-data-file may help@;";
      exit 1
  | Error e ->
      Errors.pp Fmt.stderr ?printed_filename:!driver_flags.filename_in_msg e;
      exit 1

let () =
  match Common.ICE.with_exn_message main with
  | Ok () -> ()
  | Error internal_error ->
      Out_channel.output_string stderr internal_error;
      Out_channel.flush stderr;
      exit 2
