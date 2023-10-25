(** stanc console application *)

open Core
open Core.Poly
open Frontend
open Stan_math_backend
open Analysis_and_optimization
open Middle

(** The main program. *)
let version = "%%NAME%%3 %%VERSION%%"

let name = "%%NAME%%"

(** The usage message. *)
let usage = "Usage: " ^ name ^ " [option] ... <model_file.stan[functions]>"

let model_file = ref ""
let pretty_print_program = ref false
let pretty_print_line_length = ref 78
let print_info_json = ref false
let filename_for_msg = ref ""
let canonicalize_settings = ref Canonicalize.none
let print_model_cpp = ref false
let dump_mir = ref false
let dump_mir_pretty = ref false
let dump_tx_mir = ref false
let dump_tx_mir_pretty = ref false
let dump_opt_mir = ref false
let dump_opt_mir_pretty = ref false
let dump_lir = ref false
let dump_mem_pattern = ref false
let dump_stan_math_sigs = ref false
let dump_stan_math_distributions = ref false
let opt_lvl = ref Optimize.O0
let no_soa_opt = ref false
let soa_opt = ref false
let output_file = ref ""
let generate_data = ref false
let generate_inits = ref false
let data_file = ref None
let warn_uninitialized = ref false
let warn_pedantic = ref false
let bare_functions = ref false

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
    ; ( "--debug-generate-inits"
      , Arg.Set generate_inits
      , " For debugging purposes: generate a mock initial value for each \
         parameter" )
    ; ( "--debug-data-file"
      , Arg.String (fun s -> data_file := Some s)
      , " For --debug-generate-data or --debug-generate-inits" )
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
    ; ( "--debug-lir"
      , Arg.Set dump_lir
      , " For debugging purposes: print the C++ LIR as a s-expression. Mainly \
         for comparison with --print-cpp" )
    ; ( "--debug-mem-patterns"
      , Arg.Set dump_mem_pattern
      , " For debugging purposes: print a list of matrix variables and their \
         memory type, either AoS (array of structs) or the more efficient SoA \
         (struct of arrays). Only has an effect when optimizations are turned \
         on." )
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
      , " Dump out the list of supported type signatures for Stan Math backend."
      )
    ; ( "--dump-stan-math-distributions"
      , Arg.Set dump_stan_math_distributions
      , " Dump out the list of supported probability distributions and their \
         supported suffix types for the Stan Math backend." )
    ; ( "--warn-uninitialized"
      , Arg.Set warn_uninitialized
      , " Emit warnings about uninitialized variables to stderr. Currently an \
         experimental feature." )
    ; ( "--warn-pedantic"
      , Arg.Set warn_pedantic
      , " Emit warnings about common mistakes in Stan programs." )
    ; ( "--auto-format"
      , Arg.Set pretty_print_program
      , " Pretty prints a formatted version of the Stan program." )
    ; ( "--canonicalize"
      , Arg.String
          (fun s ->
            let settings =
              List.fold ~f:parse_canonical_options ~init:!canonicalize_settings
                (String.split s ~on:',') in
            canonicalize_settings := settings)
      , " Enable specific canonicalizations in a comma separated list. Options \
         are 'deprecations', 'parentheses', 'braces', 'includes', \
         'strip-comments'." )
    ; ( "--max-line-length"
      , Arg.Set_int pretty_print_line_length
      , " Set the maximum line length for the formatter. Defaults to 78 \
         characters." )
    ; ( "--print-canonical"
      , Arg.Unit
          (fun () ->
            pretty_print_program := true;
            canonicalize_settings := Canonicalize.legacy)
      , " Prints the canonicalized program. Equivalent to --auto-format \
         --canonicalize deprecations,includes,parentheses,braces" )
    ; ( "--version"
      , Arg.Unit
          (fun _ ->
            print_endline (version ^ " " ^ "(" ^ Sys.os_type ^ ")");
            exit 0)
      , " Display stanc version number" )
    ; ( "--name"
      , Arg.Set_string Typechecker.model_name
      , " Take a string to set the model name (default = \
         \"$model_filename_model\")" )
    ; ( "--O0"
      , Arg.Unit (fun () -> opt_lvl := Optimize.O0)
      , "\t(Default) Do not apply optimizations to the Stan code." )
    ; ( "--O1"
      , Arg.Unit (fun () -> opt_lvl := Optimize.O1)
      , "\tApply level 1 compiler optimizations (only basic optimizations)." )
    ; ( "--Oexperimental"
      , Arg.Unit (fun () -> opt_lvl := Optimize.Oexperimental)
      , "\t(Experimental) Apply all compiler optimizations. Some of these are \
         not thorougly tested and may not always improve a programs \
         performance." )
    ; ( "--O"
      , Arg.Unit (fun () -> opt_lvl := Optimize.Oexperimental)
      , "\t(Experimental) Same as --Oexperimental. Apply all compiler \
         optimizations. Some of these are not thorougly tested and may not \
         always improve a programs performance." )
    ; ( "-fno-soa"
      , Arg.Unit (fun () -> no_soa_opt := true)
      , "\tTurn off the Struct of Arrays optimization" )
    ; ( "-fsoa"
      , Arg.Unit (fun () -> soa_opt := true)
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
      , Arg.Clear Typechecker.check_that_all_functions_have_definition
      , " Do not fail if a function is declared but not defined" )
    ; ( "--include-paths"
      , Arg.String
          (fun str ->
            Preprocessor.include_paths := String.split_on_chars ~on:[','] str)
      , " Takes a comma-separated list of directories that may contain a file \
         in an #include directive (default = \"\")" )
    ; ( "--use-opencl"
      , Arg.Set Transform_Mir.use_opencl
      , " If set, try to use matrix_cl signatures." )
    ; ( "--standalone-functions"
      , Arg.Set Lower_program.standalone_functions
      , " If set, the generated C++ will be the standalone functions C++ code."
      )
    ; ( "--filename-in-msg"
      , Arg.Set_string filename_for_msg
      , " Sets the filename used in compiler errors. Uses actual filename by \
         default." )
    ; ( "--info"
      , Arg.Set print_info_json
      , " If set, print information about the model." ) ]

let model_file_err () =
  Arg.usage options ("Please specify a model_file.\n" ^ usage);
  exit 127

let add_file filename =
  if !model_file = "" then model_file := filename
  else raise (Arg.Bad "Please specify only one model_file")

let remove_dotstan s =
  if String.is_suffix ~suffix:".stanfunctions" s then String.drop_suffix s 14
  else String.drop_suffix s 5

let get_ast_or_exit ?printed_filename ?(print_warnings = true)
    ?(bare_functions = false) filename =
  let res, warnings =
    if bare_functions then
      Parse.parse_file Parser.Incremental.functions_only filename
    else Parse.parse_file Parser.Incremental.program filename in
  if print_warnings then
    (Warnings.pp_warnings ?printed_filename) Fmt.stderr warnings;
  match res with
  | Result.Ok ast -> ast
  | Result.Error err ->
      Errors.pp ?printed_filename Fmt.stderr err;
      exit 1

let print_sexp sexp =
  let ppf = Format.std_formatter in
  Format.pp_set_margin ppf 90;
  Sexp.pp_hum ppf sexp

let type_ast_or_exit ?printed_filename ast =
  match Typechecker.check_program ast with
  | Result.Ok (p, warns) ->
      Warnings.pp_warnings ?printed_filename Fmt.stderr warns;
      p
  | Result.Error error ->
      Errors.pp_semantic_error ?printed_filename Fmt.stderr error;
      exit 1

(*
      I am not using Fmt to print to stderr here because there was a pretty awful
      bug where it would unpredictably fail to flush. It would flush when using
      stdout or when trying to print some strings and not others. I tried using
      Fmt.flush and various other hacks to no avail. So now I use Fmt to build a
      string, and Out_channel to write it.
 *)
let pp_stderr formatter formatee =
  Fmt.str "%a" formatter formatee |> Out_channel.(output_string stderr)

let print_or_write data =
  if !output_file <> "" then Out_channel.write_all !output_file ~data
  else print_endline data

let use_file filename =
  let printed_filename =
    match !filename_for_msg with "" -> None | s -> Some s in
  let ast =
    get_ast_or_exit ?printed_filename filename
      ~print_warnings:(not !canonicalize_settings.deprecations)
      ~bare_functions:!bare_functions in
  (* must be before typecheck to fix up deprecated syntax which gets rejected *)
  let ast = Canonicalize.repair_syntax ast !canonicalize_settings in
  Debugging.ast_logger ast;
  let typed_ast = type_ast_or_exit ?printed_filename ast in
  let canonical_ast =
    Canonicalize.canonicalize_program typed_ast !canonicalize_settings in
  if !pretty_print_program then
    print_or_write
      (Pretty_printing.pretty_print_typed_program
         ~bare_functions:!bare_functions ~line_length:!pretty_print_line_length
         ~inline_includes:!canonicalize_settings.inline_includes canonical_ast
         ~strip_comments:!canonicalize_settings.strip_comments);
  if !print_info_json then (
    print_endline (Info.info canonical_ast);
    exit 0);
  if not !canonicalize_settings.deprecations then
    Warnings.pp_warnings Fmt.stderr ?printed_filename
      (Deprecation_analysis.collect_warnings typed_ast);
  (if not !canonicalize_settings.deprecations then
     let removals = Deprecation_removals.collect_removals typed_ast in
     if not (List.is_empty removals) then (
       Deprecation_removals.pp_removals Fmt.stderr ?printed_filename removals;
       exit 65 (* EX_DATAERR in sysexits.h*)));
  if !generate_data then (
    let decls = Ast_to_Mir.gather_declarations typed_ast.datablock in
    let context =
      match !data_file with
      | None -> Map.Poly.empty
      | Some file ->
          Debug_data_generation.json_to_mir decls (Yojson.Basic.from_file file)
    in
    match Debug_data_generation.gen_values_json ~context decls with
    | Ok s ->
        print_or_write s;
        exit 0
    | Error e ->
        Errors.pp Fmt.stderr ?printed_filename (Errors.DebugDataError e);
        exit 1)
  else if !generate_inits then (
    let context =
      match !data_file with
      | None -> Map.Poly.empty
      | Some file ->
          Debug_data_generation.json_to_mir
            (Ast_to_Mir.gather_declarations typed_ast.datablock)
            (Yojson.Basic.from_file file) in
    match
      Debug_data_generation.gen_values_json ~new_only:true ~context
        (Ast_to_Mir.gather_declarations typed_ast.parametersblock)
    with
    | Ok s ->
        print_or_write s;
        exit 0
    | Error e ->
        Errors.pp Fmt.stderr ?printed_filename (Errors.DebugDataError e);
        if Option.is_none !data_file then
          Fmt.pf Fmt.stderr "Supplying a --debug-data-file may help@;";
        exit 1)
  else if Option.is_some !data_file then
    Fmt.pf Fmt.stderr "Warning: ignoring --debug-data-file";
  Debugging.typed_ast_logger typed_ast;
  if not !pretty_print_program then (
    let mir = Ast_to_Mir.trans_prog filename typed_ast in
    if !dump_mir then print_sexp [%sexp (mir : Middle.Program.Typed.t)];
    if !dump_mir_pretty then Program.Typed.pp Format.std_formatter mir;
    if !warn_pedantic then
      Pedantic_analysis.warn_pedantic mir
      |> pp_stderr (Warnings.pp_warnings ?printed_filename)
    else if !warn_uninitialized then
      Pedantic_analysis.warn_uninitialized mir
      |> pp_stderr (Warnings.pp_warnings ?printed_filename);
    let tx_mir = Transform_Mir.trans_prog mir in
    if !dump_tx_mir then print_sexp [%sexp (tx_mir : Middle.Program.Typed.t)];
    if !dump_tx_mir_pretty then Program.Typed.pp Format.std_formatter tx_mir;
    let opt_mir =
      let set_optims =
        let base_optims = Optimize.level_optimizations !opt_lvl in
        if !no_soa_opt then {base_optims with optimize_soa= false}
        else if !soa_opt then {base_optims with optimize_soa= true}
        else base_optims in
      Optimize.optimization_suite ~settings:set_optims tx_mir in
    if !dump_mem_pattern then
      Memory_patterns.pp_mem_patterns Format.std_formatter opt_mir;
    if !dump_opt_mir then print_sexp [%sexp (opt_mir : Middle.Program.Typed.t)];
    if !dump_opt_mir_pretty then Program.Typed.pp Format.std_formatter opt_mir;
    if !output_file = "" then output_file := remove_dotstan !model_file ^ ".hpp";
    let cpp = Lower_program.lower_program ?printed_filename opt_mir in
    if !dump_lir then print_sexp [%sexp (cpp : Cpp.program)];
    let cpp_str = Fmt.(to_to_string Cpp.Printing.pp_program) cpp in
    Out_channel.write_all !output_file ~data:cpp_str;
    if !print_model_cpp then print_endline cpp_str)

let mangle =
  String.concat_map ~f:(fun c ->
      Char.(
        if is_alphanum c || c = '_' then to_string c
        else match c with '-' -> "_" | _ -> "x" ^ Int.to_string (to_int c)))

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
  if !model_file = "" then model_file_err ();
  let stanc_args_to_print =
    let sans_model_and_hpp_paths x =
      not
        String.(
          is_suffix ~suffix:".stan" x
          && not (is_prefix ~prefix:"--filename-in-msg" x)
          || is_prefix ~prefix:"--o" x) in
    (* Ignore the "--o" arg, the stan file and the binary name (bin/stanc). *)
    Array.to_list (Sys.get_argv ())
    |> List.tl_exn
    |> List.filter ~f:sans_model_and_hpp_paths
    |> String.concat ~sep:" " in
  Lower_program.stanc_args_to_print := stanc_args_to_print;
  (* if we only have functions, always compile as standalone *)
  if String.is_suffix !model_file ~suffix:".stanfunctions" then (
    Lower_program.standalone_functions := true;
    bare_functions := true);
  (* Just translate a stan program *)
  if !Typechecker.model_name = "" then
    Typechecker.model_name :=
      mangle
        (remove_dotstan List.(hd_exn (rev (String.split !model_file ~on:'/'))))
      ^ "_model"
  else Typechecker.model_name := mangle !Typechecker.model_name;
  use_file !model_file

let () = main ()
