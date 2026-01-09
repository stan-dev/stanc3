(** Some complicated stuff to get the custom syntax errors out of Menhir's
    Incremental API *)

open Core
open Common.Let_syntax.Result
module Interp = Parser.MenhirInterpreter

let drive_parser parse_fun =
  let input () =
    Interp.lexer_lexbuf_to_supplier Lexer.token
      (Preprocessor.current_buffer ())
      () in
  let success prog = {prog with Ast.comments= Preprocessor.get_comments ()} in
  let failure error_state =
    (* see the Menhir manual for the description of error messages support *)
    let env =
      match error_state with
      | Interp.HandlingError env -> env
      | _ ->
          Common.ICE.internal_compiler_error
            [%message "Parser failed but is not in an error state "] in
    let message =
      let state = Interp.current_state_number env in
      try
        Parsing_errors.message state
        ^^
        if !Debugging.grammar_logging then
          Scanf.format_from_string
            ("(Parse error state " ^ string_of_int state ^ ")\n")
            ""
        else ""
      with _ ->
        Common.ICE.internal_compiler_error
          [%message
            "Failed to find error for parser error state " (state : int)] in
    let location =
      Preprocessor.location_span_of_positions (Interp.positions env) in
    Syntax_error.parse_error message location in
  let startp = (Preprocessor.current_buffer ()).lex_curr_p in
  Syntax_error.try_with (fun () ->
      Interp.loop_handle success failure input (parse_fun startp))

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
    drive_parser parse_fun
    |> Result.map_error ~f:(fun e -> Errors.Syntax_error e) in
  (result, Input_warnings.collect ())

let parse_stanfunctions file_or_code =
  parse Parser.Incremental.functions_only file_or_code

let parse_program file_or_code = parse Parser.Incremental.program file_or_code
