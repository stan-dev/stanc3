(** This file contains some generic code for a command line application compiler. *)
module type LANGUAGE = sig
  val name : string

  type command

  type environment

  val options : (Arg.key * Arg.spec * Arg.doc) list

  val initial_environment : environment

  val file_parser : (Lexing.lexbuf -> command list) option

  val toplevel_parser : (Lexing.lexbuf -> command) option

  val exec : environment -> command -> environment
end

module Main (L : LANGUAGE) = struct
  open Errors

  (** The command-line wrappers that we look for. *)
  let wrapper = ref (Some ["rlwrap"; "ledit"])

  (** The usage message. *)
  let usage =
    match L.file_parser with
    | Some _ -> "Usage: " ^ L.name ^ " [option] ... [file] ..."
    | None -> "Usage:" ^ L.name ^ " [option] ..."

  (** A list of files to be loaded and run. *)
  let files = ref []

  (** Add a file to the list of files to be loaded, and record whether it should
      be processed in interactive mode. *)
  let add_file interactive filename =
    files := (filename, interactive) :: !files

  (** Some example command-line options here *)
  let options =
    Arg.align
      ( [ ( "--debug-parse"
          , Arg.Unit (fun () -> Debug.grammar_logging := true)
          , " For debugging purposes: print the parser actions" )
        ; ( "--debug-ast"
          , Arg.Unit (fun () -> Debug.ast_printing := true)
          , " For debugging purposes: print the undecorated AST, before \
             semantic checking" )
        ; ( "--debug-decorated-ast"
          , Arg.Unit (fun () -> Debug.typed_ast_printing := true)
          , " For debugging purposes: print the decorated AST, after semantic \
             checking" )
        ; ( "-v"
          , Arg.Unit
              (fun () ->
                print_endline (L.name ^ " " ^ "(" ^ Sys.os_type ^ ")") ;
                exit 0 )
          , " Print language information and exit" ) ]
      @ L.options )

  (** Treat anonymous arguments as files to be run. *)
  let anonymous str = add_file true str

  (** Parse the contents from a file, using a given [parser]. *)
  let read_file parser fn =
    try
      let fh = open_in fn in
      let lex = Lexing.from_channel fh in
      lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname= fn} ;
      try
        let terms = parser lex in
        close_in fh ; terms
      with (* Close the file in case of any parsing errors. *)
      | Error err -> close_in fh ; raise (Error err)
    
    with
    (* Any errors when opening or closing a file are fatal. *)
    | Sys_error msg
    -> fatal_error "%s" msg

  (** Parser wrapper that catches syntax-related errors and converts them to errors. *)
  let wrap_syntax_errors parser lex =
    try parser lex with
    | Failure _ ->
        syntax_error ~loc:(location_of_lex lex) "unrecognised symbol"
    | _ -> syntax_error ~loc:(location_of_lex lex) "syntax error"

  (** Load directives from the given file. *)
  let use_file ctx (filename, interactive) =
    match L.file_parser with
    | Some f ->
        let cmds = read_file (wrap_syntax_errors f) filename in
        List.fold_left L.exec ctx cmds
    | None ->
        fatal_error "Cannot load files, only interactive shell is available"

  (** Main program *)
  let main () =
    (* Intercept Ctrl-C by the user *)
    Sys.catch_break true ;
    (* Parse the arguments. *)
    Arg.parse options anonymous usage ;
    (* Files were listed in the wrong order, so we reverse them *)
    files := List.rev !files ;
    (* Set the maximum depth of pretty-printing, after which it prints ellipsis. *)
    Format.set_max_boxes 42 ;
    Format.set_ellipsis_text "..." ;
    try
      (* Run and load all the specified files. *)
      let _ = List.fold_left use_file L.initial_environment !files in
      ()
    with Error err -> print_error err ; exit 1
end
