(** stanc console application *)

open Core_kernel
open Stanclib

(** The main program. *)
let version = "stanc version 3.0 alpha"

let name = "stanc"

(** The usage message. *)
let usage = "Usage: " ^ name ^ " [option] ... [file] ..."

(** A list of files to be loaded and run. *)
let files = ref []

let pretty_print_program = ref false
let dump_mir = ref false

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
    ; ( "--dump-mir"
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
      , Arg.String (fun str -> Semantic_check.model_name := str)
      , " Take a string to set the model name (default = \
         \"$model_filename_model\")" )
    ; ( "--o"
      , Arg.String
          (fun _ ->
            print_endline "TODO: not yet implemented" ;
            assert false )
      , " Take the path to an output file for generated C++ code (default = \
         \"$name.cpp\")" )
    ; ( "--allow_undefined"
      , Arg.Unit
          (fun () ->
            Semantic_check.check_that_all_functions_have_definition := false )
      , " Do not fail if a function is declared but not defined" )
    ; ( "--include_paths"
      , Arg.String
          (fun str ->
            Preprocessor.include_paths := String.split_on_chars ~on:[','] str
            )
      , " Takes a comma-separated list of directories that may contain a file \
         in an #include directive (default = \"\")" ) ]

(** Add a file to the list of files to be loaded, and record whether it should
      be processed in interactive mode. *)
let add_file filename = files := filename :: !files

(** ad directives from the given file. *)
let use_file filename =
  let _ =
    if !Semantic_check.model_name = "" then
      Semantic_check.model_name :=
        String.drop_suffix
          (List.hd_exn (List.rev (String.split filename ~on:'/')))
          5
        ^ "_model"
  in
  let ast =
    try Parse.parse_file Parser.Incremental.program filename
    with Errors.SyntaxError err ->
      Errors.report_syntax_error err ;
      exit 1
  in
  let _ = Debugging.ast_logger ast in
  if !pretty_print_program then
    print_endline (Pretty_printing.pretty_print_program ast)
  else
    let typed_ast =
      try Semantic_check.semantic_check_program ast
      with Errors.SemanticError err ->
        Errors.report_semantic_error err ;
        exit 1
    in
    let _ = Debugging.typed_ast_logger typed_ast in
    let mir = Ast_to_Mir.trans_prog filename typed_ast in
    if !dump_mir then
      Sexp.pp_hum Format.std_formatter [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
    let cpp = Format.asprintf "%a" Stan_math_backend.emit_prog mir in
    print_string cpp

let main () =
  (* Parse the arguments. *)
  Arg.parse options add_file usage ;
  (* Files were listed in the wrong order, so we reverse them *)
  files := List.rev !files ;
  (* Run and load all the specified files. *)
  let _ = List.map ~f:use_file !files in
  ()

let _ = main ()
