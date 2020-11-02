(** Some complicated stuff to get the custom syntax errors out of Menhir's Incremental
    API *)

open Core_kernel
open Middle

let parse parse_fun lexbuf =
  (* see the Menhir manual for the description of
     error messages support *)
  let open MenhirLib.General in
  let module Interp = Parser.MenhirInterpreter in
  Stack.push Preprocessor.include_stack lexbuf ;
  let input _ =
    (Interp.lexer_lexbuf_to_supplier Lexer.token
       (Stack.top_exn Preprocessor.include_stack))
      ()
  in
  let success prog = Result.Ok prog in
  let failure error_state =
    let env =
      match[@warning "-4"] error_state with
      | Interp.HandlingError env -> env
      | _ -> assert false
    in
    match Interp.stack env with
    | (lazy Nil) ->
        let message =
          "Expected \"functions {\" or \"data {\" or \"transformed data {\" \
           or \"parameters {\" or \"transformed parameters {\" or \"model {\" \
           or \"generated quantities {\".\n"
        in
        Errors.Parsing
          ( message
          , Option.value_exn
              (Location_span.of_positions_opt
                 (Lexing.lexeme_start_p
                    (Stack.top_exn Preprocessor.include_stack))
                 (Lexing.lexeme_end_p
                    (Stack.top_exn Preprocessor.include_stack))) )
        |> Result.Error
    | (lazy (Cons (Interp.Element (state, _, start_pos, end_pos), _))) ->
        let message =
          try
            Parsing_errors.message (Interp.number state)
            ^
            if !Debugging.grammar_logging then
              "(Parse error state " ^ string_of_int (Interp.number state) ^ ")"
            else ""
          with
          | Not_found_s _ ->
              if !Debugging.grammar_logging then
                "(Parse error state "
                ^ string_of_int (Interp.number state)
                ^ ")"
              else ""
          | _ ->
              "(Parse error state " ^ string_of_int (Interp.number state) ^ ")"
        in
        Errors.Parsing
          (message, Location_span.of_positions_exn (start_pos, end_pos))
        |> Result.Error
  in
  Interp.loop_handle success failure input (parse_fun lexbuf.Lexing.lex_curr_p)

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
  let chan = In_channel.create path in
  let lexbuf =
    let open Lexing in
    let lexbuf = from_channel chan in
    lexbuf.lex_start_p
    <- {pos_fname= path; pos_lnum= 1; pos_bol= 0; pos_cnum= 0} ;
    lexbuf.lex_curr_p <- lexbuf.lex_start_p ;
    lexbuf
  in
  parse parse_fun lexbuf
