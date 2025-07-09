(** Some complicated stuff to get the custom syntax errors out of Menhir's Incremental
    API *)

open Core
open Common.Let_syntax.Result

(** Internal (private) exception used for some out-of-band errors. *)
exception ParserException of Errors.syntax_error

(* Defining the exception and module here lets us hide the implementation from
   the world outside this module. No other code can raise [ParserException],
   or even observe that it exists. *)
module ParserExns : Errors.ParserExn = struct
  let error e = raise (ParserException e)

  let parse_error ~loc msg =
    error (Errors.Parsing (msg, Preprocessor.location_span_of_positions loc))

  let current_location () =
    Preprocessor.location_of_position
      (Lexing.lexeme_start_p (Preprocessor.current_buffer ()))

  let include_error msg = error (Errors.Include (msg, current_location ()))
  let unexpected_eof () = error (Errors.UnexpectedEOF (current_location ()))
  let unexpected_character () = error (Errors.Lexing (current_location ()))
end

module Lexer = Lexer.Make (ParserExns)
module Parser = Parser.Make (ParserExns)
module Interp = Parser.MenhirInterpreter

let drive_parser parse_fun =
  let input () =
    Interp.lexer_lexbuf_to_supplier Lexer.token
      (Preprocessor.current_buffer ())
      () in
  let success prog = Ok {prog with Ast.comments= Preprocessor.get_comments ()} in
  let failure error_state =
    (* see the Menhir manual for the description of
       error messages support *)
    let env =
      match error_state with
      | Interp.HandlingError env -> env
      | _ ->
          Common.ICE.internal_compiler_error
            [%message "Parser failed but is not in an error state "] in
    let message =
      let state = Interp.current_state_number env in
      try
        Fmt.str "%s%a"
          (Parsing_errors.message state)
          (Fmt.if' !Debugging.grammar_logging (fun ppf ->
               Fmt.pf ppf "(Parse error state %d)"))
          state
      with _ ->
        Common.ICE.internal_compiler_error
          [%message
            "Failed to find error for parser error state " (state : int)] in
    let location =
      Preprocessor.location_span_of_positions (Interp.positions env) in
    Error (Errors.Syntax_error (Errors.Parsing (message, location))) in
  let startp = (Preprocessor.current_buffer ()).lex_curr_p in
  try Interp.loop_handle success failure input (parse_fun startp)
  with ParserException e -> Error (Errors.Syntax_error e)

let to_lexbuf file_or_code =
  match file_or_code with
  | `File path ->
      let+ chan =
        try Ok (In_channel.create path)
        with _ -> Error (Errors.FileNotFound path) in
      (Lexing.from_channel chan, path)
  | `Code code -> Ok (Lexing.from_string code, "string")

let parse parse_fun file_or_code =
  Input_warnings.init ();
  let result =
    let* lexbuf, name = to_lexbuf file_or_code in
    Preprocessor.init lexbuf name;
    drive_parser parse_fun in
  (result, Input_warnings.collect ())

let parse_stanfunctions file_or_code =
  parse Parser.Incremental.functions_only file_or_code

let parse_program file_or_code = parse Parser.Incremental.program file_or_code
