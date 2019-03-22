(** stanc console application *)

open Core_kernel
open Stanc_mir
open Stanc_frontend
open Compiler
open Stanc_optimization
open Stanc_backend_cplusplus

module Stanc = Make(Backend.CPlusPlus)(Optimization.Identity)



(** The main program. *)
let version = "stanc version 3.0 alpha"

let name = "stanc"

(** The usage message. *)
let usage = "Usage: " ^ name ^ " [option] ... <model_file.stan>"

let model_file = ref ""
let pretty_print_program = ref false
let print_model_cpp = ref false
let dump_mir = ref false
let output_file = ref ""

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
    ; ( "--debug-mir"
      , Arg.Set dump_mir
      , " For debugging purposes: print the MIR." )
    ; ( "--auto-format"
      , Arg.Set pretty_print_program
      , " Pretty prints the program to the console" )
    ; ( "--version"
      , Arg.Unit
          (fun _ ->
            print_endline (version ^ " " ^ "(" ^ Sys.os_type ^ ")") ;
            exit 1 )
      , " Display stanc version number" )
    ; ( "--name"
      , Arg.Set_string Semantic_check.model_name
      , " Take a string to set the model name (default = \
         \"$model_filename_model\")" )
    ; ( "--o"
      , Arg.Set_string output_file
      , " Take the path to an output file for generated C++ code (default = \
         \"$name.cpp\")" )
    ; ( "--print-cpp"
      , Arg.Set print_model_cpp
      , " If set, output the generated C++ Stan model class to stdout." )
    ; ( "--allow_undefined"
      , Arg.Clear Semantic_check.check_that_all_functions_have_definition
      , " Do not fail if a function is declared but not defined" )
    ; ( "--include_paths"
      , Arg.String
          (fun str ->
            Preprocessor.include_paths := String.split_on_chars ~on:[','] str
            )
      , " Takes a comma-separated list of directories that may contain a file \
         in an #include directive (default = \"\")" ) ]

let model_file_err () =
  Arg.usage options ("Please specify one model_file.\n\n" ^ usage) ;
  exit 127

let add_file filename =
  if !model_file = "" then model_file := filename else model_file_err ()



(** ad directives from the given file. *)
let use_file filename = Stanc.(
  match compile_verbose filename with 
  | Error (state , err) -> 
      (match err with 
      | Compiler.Lexing(err,loc) -> 
          Printf.eprintf "\nSyntax error in %s, lexing error:\n"
            (Errors.string_of_location {loc with col_num= loc.col_num - 1});
          Printf.eprintf "%s" err;
          (* XXX restore context mesages *)
          exit 1

      | Compiler.Include(err,loc) ->
          Printf.eprintf "\nSyntax error in %s, include error:\n"
            (Errors.string_of_location loc);
          Printf.eprintf "%s" err;
          (* XXX restore context mesages *)
          exit 1

      | Compiler.Parsing(err,loc_sp) ->
          Printf.eprintf "\nSyntax error in %s, parsing error:\n"
            (Errors.string_of_location_span loc_sp);
          Printf.eprintf "%s" err;
          (* XXX restore context mesages *)
          exit 1

      | Compiler.SemanticError(err,loc_sp) ->
        match state.ast with 
        | Some ast -> 
            let _ = Debugging.ast_logger ast in

            Printf.eprintf "\n%s in %s:\n" "Semantic error"
              (Errors.string_of_location_span loc_sp);
            Printf.eprintf "%s" err;
            exit 1;

        | _ -> 
          exit 1

      )

  | Ok verbose ->
      match (verbose.ast, verbose.typed_ast, verbose.mir, verbose.out) with
      | Some ast, Some typed_ast, Some mir, Some out ->
          let _ = Debugging.ast_logger ast in
          if !pretty_print_program then
            print_endline (Pretty_printing.pretty_print_program ast);

          let _ = Debugging.typed_ast_logger typed_ast in

          if !dump_mir then
            Mir.pp_typed_prog Fmt.stdout mir;

          Out_channel.write_all !output_file ~data:out;

          if !print_model_cpp then 
            print_endline out
      | _ -> 
        exit 1
)


let remove_dotstan s = String.drop_suffix s 5

let main () =
  (* Parse the arguments. *)
  Arg.parse options add_file usage ;
  if !model_file = "" then model_file_err () ;
  if !Semantic_check.model_name = "" then
    Semantic_check.model_name :=
      remove_dotstan List.(hd_exn (rev (String.split !model_file ~on:'/')))
      ^ "_model" ;
  if !output_file = "" then output_file := remove_dotstan !model_file ^ ".cpp" ;
  use_file !model_file

let () = main ()
