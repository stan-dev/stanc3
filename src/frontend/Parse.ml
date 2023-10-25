(** Some complicated stuff to get the custom syntax errors out of Menhir's Incremental
    API *)

open Core

let parse parse_fun lexbuf =
  Input_warnings.init ();
  (* see the Menhir manual for the description of
     error messages support *)
  let module Interp = Parser.MenhirInterpreter in
  let input () =
    (Interp.lexer_lexbuf_to_supplier Lexer.token
       (Preprocessor.current_buffer ()))
      () in
  let success prog =
    Result.Ok {prog with Ast.comments= Preprocessor.get_comments ()} in
  let failure error_state =
    let env =
      match[@warning "-4"] error_state with
      | Interp.HandlingError env -> env
      | _ -> assert false in
    let message =
      match Interp.top env with
      | None ->
          "Expected \"functions {\" or \"data {\" or \"transformed data {\" or \
           \"parameters {\" or \"transformed parameters {\" or \"model {\" or \
           \"generated quantities {\".\n"
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
                "(Parse error state "
                ^ string_of_int (Interp.number state)
                ^ ")"
              else ""
          | _ ->
              "(Parse error state " ^ string_of_int (Interp.number state) ^ ")")
    in
    Errors.Parsing
      ( message
      , Preprocessor.location_span_of_positions
          ( Lexing.lexeme_start_p (Preprocessor.current_buffer ())
          , Lexing.lexeme_end_p (Preprocessor.current_buffer ()) ) )
    |> Result.Error in
  let result =
    try
      parse_fun lexbuf.Lexing.lex_curr_p
      |> Interp.loop_handle success failure input
      |> Result.map_error ~f:(fun e -> Errors.Syntax_error e)
    with Errors.SyntaxError err -> Result.Error (Errors.Syntax_error err) in
  (result, Input_warnings.collect ())

let parse_string parse_fun str =
  let lexbuf = Lexing.from_string str in
  Preprocessor.init lexbuf "string";
  parse parse_fun lexbuf

let parse_file parse_fun path =
  let chan =
    try Ok (In_channel.create path) with _ -> Error (Errors.FileNotFound path)
  in
  match chan with
  | Error err -> (Error err, [])
  | Ok chan ->
      let lexbuf = Lexing.from_channel chan in
      Preprocessor.init lexbuf path;
      parse parse_fun lexbuf
