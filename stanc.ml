open Stanclib

(** The main program. *)
let version = "stanc version 3.0 alpha"

let name = "stanc"

(** The usage message. *)
let usage = "Usage: " ^ name ^ " [option] ... [file] ..."

(** A list of files to be loaded and run. *)
let files = ref []

(** Some example command-line options here *)
let options =
  Arg.align
    [ ( "--debug-lex"
      , Arg.Unit (fun () -> Debug.lexer_logging := true)
      , " For debugging purposes: print the lexer actions" )
    ; ( "--debug-parse"
      , Arg.Unit (fun () -> Debug.grammar_logging := true)
      , " For debugging purposes: print the parser actions" )
    ; ( "--debug-ast"
      , Arg.Unit (fun () -> Debug.ast_printing := true)
      , " For debugging purposes: print the undecorated AST, before semantic \
         checking" )
    ; ( "--debug-decorated-ast"
      , Arg.Unit (fun () -> Debug.typed_ast_printing := true)
      , " For debugging purposes: print the decorated AST, after semantic \
         checking" )
    ; ( "--auto-format"
      , Arg.Unit (fun () -> Debug.pretty_print_program := true)
      , " Pretty prints the program to the console" )
    ; ( "--version"
      , Arg.String
          (fun _ ->
            print_endline "TODO: not yet implemented" ;
            assert false )
      , " Display stanc version number" )
    ; ( "--name"
      , Arg.String
          (fun _ ->
            print_endline "TODO: not yet implemented" ;
            assert false )
      , " Take a string to set the model name (default = \
         \"$model_filename_model\")" )
    ; ( "--o"
      , Arg.Unit
          (fun _ ->
            print_endline "TODO: not yet implemented" ;
            assert false )
      , " Take the path to an output file for generated C++ code (default = \
         \"$name.cpp\")" )
    ; ( "--allow_undefined"
      , Arg.Unit
          (fun () ->
            print_endline (version ^ " " ^ "(" ^ Sys.os_type ^ ")") ;
            exit 0 )
      , " Do not fail if a function is declared but not defined" )
    ; ( "--include_paths"
      , Arg.String
          (fun str -> Lexer.include_paths := String.split_on_char ',' str)
      , " Takes a comma-separated list of directories that may contain a file \
         in an #include directive" ) ]

(** Add a file to the list of files to be loaded, and record whether it should
      be processed in interactive mode. *)
let add_file filename = files := filename :: !files

(** ad directives from the given file. *)
let use_file filename =
  let ast =
    try Parse.parse_file Parser.Incremental.program filename
    with Errors.SyntaxError err ->
      Errors.report_syntax_error err ;
      exit 1
  in
  let _ = Debug.ast_logger ast in
  let _ = Debug.auto_formatter ast in
  let typed_ast =
    try Semantic_check.semantic_check_program ast
    with Errors.SemanticError err ->
      Errors.report_semantic_error err ;
      exit 1
  in
  let _ = Debug.typed_ast_logger typed_ast in
  ()

(** Main program *)
let main () =
  (* Parse the arguments. *)
  Arg.parse options add_file usage ;
  (* Files were listed in the wrong order, so we reverse them *)
  files := List.rev !files ;
  (* Run and load all the specified files. *)
  let _ = List.map use_file !files in
  ()

let _ = main ()
