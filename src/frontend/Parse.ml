(** Some complicated stuff to get the custom syntax errors out of Menhir's
    Incremental API *)

open Result.Syntax
module Interp = Parser.MenhirInterpreter

let drive_parser parse_fun =
  let input () =
    let token = Lexer.token (Preprocessor.current_buffer ()) in
    let lexbuf =
      (* Lexer.token can modify the state of the preprocessor, so we ask for the
         current lexbuf again now *)
      Preprocessor.current_buffer () in
    let startp = lexbuf.Lexing.lex_start_p and endp = lexbuf.Lexing.lex_curr_p in
    (token, startp, endp) in
  let success prog = {prog with Ast.comments= Preprocessor.get_comments ()} in
  let failure prev error_state =
    (* see the Menhir manual for the description of error messages support *)
    let env =
      match error_state with
      | Interp.HandlingError env -> env
      | _ ->
          Common.ICE.internal_error "Parser failed but is not in an error state"
          [@coverage off] in
    let message =
      let state = Interp.current_state_number env in
      try
        Parsing_errors.message state
        ^^
        if !Debugging.grammar_logging then
          Scanf.format_from_string
            ("(Parse error state " ^ Int.to_string state ^ ")\n")
            ""
        else ""
      with _ ->
        Common.ICE.internal_errorf "Failed to find error for parser state %d"
          [state] [@coverage off] in
    let location =
      let env =
        match prev with
        (* if we errored because of one of our UNREACHABLE token, use the
           location immediately preceding for a more informative error *)
        | Interp.InputNeeded prev_env
          when Interp.acceptable prev Parser.UNREACHABLE Lexing.dummy_pos ->
            prev_env
        | _ -> env in
      Preprocessor.location_span_of_positions (Interp.positions env) in
    Syntax_error.parse_error message location in
  let startp = (Preprocessor.current_buffer ()).lex_curr_p in
  Syntax_error.try_with (fun () ->
      Interp.loop_handle_undo success failure input (parse_fun startp))

let to_lexbuf file_or_code =
  match file_or_code with
  | `File path ->
      let+ chan =
        try Ok (In_channel.open_bin path)
        with _ -> Error (Errors.FileNotFound path) in
      let lexbuf = Lexing.from_channel chan in
      Gc.finalise (fun _ -> In_channel.close_noerr chan) lexbuf;
      (lexbuf, path)
  | `Code code -> Ok (Lexing.from_string code, "string")

let parse parse_fun file_or_code =
  Input_warnings.init ();
  let result =
    let* lexbuf, name = to_lexbuf file_or_code in
    Preprocessor.init lexbuf name;
    drive_parser parse_fun |> Result.map_error (fun e -> Errors.Syntax_error e)
  in
  (result, Input_warnings.collect ())

let parse_stanfunctions file_or_code =
  parse Parser.Incremental.functions_only file_or_code

let parse_program file_or_code = parse Parser.Incremental.program file_or_code
