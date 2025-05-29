(** Some complicated stuff to get the custom syntax errors out of Menhir's Incremental
    API *)

open Core
module Interp = Parser.MenhirInterpreter

let drive_parser parse_fun =
  Input_warnings.init ();
  (* see the Menhir manual for the description of
     error messages support *)
  let input () =
    Interp.lexer_lexbuf_to_supplier Lexer.token
      (Preprocessor.current_buffer ())
      () in
  let success prog =
    Result.Ok {prog with Ast.comments= Preprocessor.get_comments ()} in
  let failure error_state =
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
  let result =
    let startp = (Preprocessor.current_buffer ()).lex_curr_p in
    try Interp.loop_handle success failure input (parse_fun startp)
    with Errors.SyntaxError err -> Result.Error (Errors.Syntax_error err) in
  (result, Input_warnings.collect ())

let parse_string parse_fun str =
  let lexbuf = Lexing.from_string str in
  Preprocessor.init lexbuf "string";
  drive_parser parse_fun

let parse_file parse_fun path =
  let chan =
    try Ok (In_channel.create path) with _ -> Error (Errors.FileNotFound path)
  in
  match chan with
  | Error err -> (Error err, [])
  | Ok chan ->
      let lexbuf = Lexing.from_channel chan in
      Preprocessor.init lexbuf path;
      drive_parser parse_fun

let parse parse_fun file_or_code =
  match file_or_code with
  | `File path -> parse_file parse_fun path
  | `Code code -> parse_string parse_fun code

let parse_stanfunctions file_or_code =
  parse Parser.Incremental.functions_only file_or_code

let parse_program file_or_code = parse Parser.Incremental.program file_or_code
