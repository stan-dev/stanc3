(** Some complicated stuff to get the custom syntax errors out of Menhir's Incremental
    API *)

open Core_kernel
open Middle

let parse parse_fun lexbuf =
  Input_warnings.init () ;
  Lexer.comments := [] ;
  (* see the Menhir manual for the description of
     error messages support *)
  let module Interp = Parser.MenhirInterpreter in
  Stack.push Preprocessor.include_stack lexbuf ;
  let input () =
    (Interp.lexer_lexbuf_to_supplier Lexer.token
       (Stack.top_exn Preprocessor.include_stack))
      ()
  in
  let success prog =
    Result.Ok {prog with Ast.comments= List.rev !Lexer.comments}
  in
  let failure error_state =
    let env =
      match[@warning "-4"] error_state with
      | Interp.HandlingError env -> env
      | _ -> assert false
    in
    let message =
      match Interp.top env with
      | None ->
          "Expected \"functions {\" or \"data {\" or \"transformed data {\" \
           or \"parameters {\" or \"transformed parameters {\" or \"model {\" \
           or \"generated quantities {\".\n"
      | Some (Interp.Element (state, _, _, _)) -> (
        try
          Parsing_errors.message (Interp.number state)
          ^
          if !Debugging.grammar_logging then
            "(Parse error state " ^ string_of_int (Interp.number state) ^ ")"
          else ""
        with
        | Not_found_s _ ->
            if !Debugging.grammar_logging then
              "(Parse error state " ^ string_of_int (Interp.number state) ^ ")"
            else ""
        | _ ->
            "(Parse error state " ^ string_of_int (Interp.number state) ^ ")" )
    in
    Errors.Parsing
      ( message
      , Location_span.of_positions_exn
          ( Lexing.lexeme_start_p (Stack.top_exn Preprocessor.include_stack)
          , Lexing.lexeme_end_p (Stack.top_exn Preprocessor.include_stack) ) )
    |> Result.Error
  in
  let result =
    try
      parse_fun lexbuf.Lexing.lex_curr_p
      |> Interp.loop_handle success failure input
      |> Result.map_error ~f:(fun e -> Errors.Syntax_error e)
    with Errors.SyntaxError err -> Result.Error (Errors.Syntax_error err)
  in
  Lexer.comments := [] ;
  (result, Input_warnings.collect ())

let parse_string parse_fun str =
  let lexbuf =
    let open Lexing in
    let lexbuf = from_string str in
    lexbuf.lex_start_p
    <- {pos_fname= "string"; pos_lnum= 1; pos_bol= 0; pos_cnum= 0} ;
    lexbuf.lex_curr_p <- lexbuf.lex_start_p ;
    lexbuf
  in
  parse parse_fun lexbuf

let parse_file parse_fun path =
  let chan =
    try Ok (In_channel.create path) with _ -> Error (Errors.FileNotFound path)
  in
  match chan with
  | Error err -> (Error err, [])
  | Ok chan ->
      let lexbuf =
        let open Lexing in
        let lexbuf = from_channel chan in
        lexbuf.lex_start_p
        <- {pos_fname= path; pos_lnum= 1; pos_bol= 0; pos_cnum= 0} ;
        lexbuf.lex_curr_p <- lexbuf.lex_start_p ;
        lexbuf
      in
      parse parse_fun lexbuf
