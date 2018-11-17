(** The main program. *)
open Ast

let name = "stan"

(** The command that actually executes a command. *)
let exec _ p =
  let _ = Debug.typed_ast_logger (Semantic_check.semantic_check_program p) in
  ()

(** This file contains some generic code for a command line application compiler. *)
open Errors

(** The usage message. *)
let usage = "Usage: " ^ name ^ " [option] ... [file] ..."

(** A list of files to be loaded and run. *)
let files = ref []

(** Add a file to the list of files to be loaded, and record whether it should
      be processed in interactive mode. *)
let add_file interactive filename = files := (filename, interactive) :: !files

(** Some example command-line options here *)
let options =
  Arg.align
    [ ( "--debug-parse"
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
    ; ( "-v"
      , Arg.Unit
          (fun () ->
            print_endline (name ^ " " ^ "(" ^ Sys.os_type ^ ")") ;
            exit 0 )
      , " Print language information and exit" ) ]

(** Some hard work to get the custom syntax errors out of Menhir's Incremental
    API *)
let parse parse_fun lexbuf =
  (* see the Menhir manual for the description of
     error messages support *)
  let open MenhirLib.General in
  let module Interp = Parser.MenhirInterpreter in
  let input = Interp.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let success prog = prog in
  let failure error_state =
    let env =
      match[@warning "-4"] error_state with
      | Interp.HandlingError env -> env
      | _ -> assert false
    in
    match Interp.stack env with
    | (lazy Nil) -> assert false
    | (lazy (Cons (Interp.Element (state, _, start_pos, end_pos), _))) ->
        let message =
          try Some (Parsing_errors.message (Interp.number state))
          with Not_found -> None
        in
        raise (SyntaxError (Parsing (message, start_pos, end_pos)))
  in
  try
    Interp.loop_handle success failure input
      (parse_fun lexbuf.Lexing.lex_curr_p)
  with Lexer.Error (input, pos) -> raise (SyntaxError (Lexing (input, pos)))

let parse_file parse_fun path =
  let chan = open_in path in
  let lexbuf =
    let open Lexing in
    let lexbuf = from_channel chan in
    lexbuf.lex_start_p
    <- {pos_fname= path; pos_lnum= 1; pos_bol= 0; pos_cnum= 0} ;
    lexbuf.lex_curr_p <- lexbuf.lex_start_p ;
    lexbuf
  in
  try parse parse_fun lexbuf with SyntaxError err ->
    report_error lexbuf err ; exit 1

(** ad directives from the given file. *)
let use_file ctx (filename, interactive) =
  let cmds = parse_file Parser.Incremental.file filename in
  List.fold_left exec ctx cmds

(** Main program *)
let main () =
  (* Intercept Ctrl-C by the user *)
  Sys.catch_break true ;
  (* Parse the arguments. *)
  Arg.parse options (add_file true) usage ;
  (* Files were listed in the wrong order, so we reverse them *)
  files := List.rev !files ;
  (* Set the maximum depth of pretty-printing, after which it prints ellipsis. *)
  Format.set_max_boxes 42 ;
  Format.set_ellipsis_text "..." ;
  try
    (* Run and load all the specified files. *)
    let _ = List.fold_left use_file () !files in
    ()
  with Error err -> print_error err ; exit 1

let _ = main ()
