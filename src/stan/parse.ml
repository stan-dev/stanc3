(** Some complicated stuff to get the custom syntax errors out of Menhir's Incremental
    API *)

open Errors

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
    (* TODO: insert position here. *)
    | (lazy Nil) ->
        let message =
          Some
            "Expected \"functions {\" or \"data {\" or \"transformed data {\" \
             or \"parameters {\" or \"transformed parameters {\" or \"model \
             {\" or \"generated quantities {\"."
        in
        raise
          (SyntaxError (Parsing (message, Lexing.dummy_pos, Lexing.dummy_pos)))
    | (lazy (Cons (Interp.Element (state, _, start_pos, end_pos), _))) ->
        let message =
          try
            Some
              ( Parsing_errors.message (Interp.number state)
              ^ "(Parse error state "
              ^ string_of_int (Interp.number state)
              ^ ")" )
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
    report_syntax_error err ; exit 1
