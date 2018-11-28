(** The preprocessor to handle #include directives *)

(* The Lexstack type. *)
type 'a lexstack =
  { stack: Lexing.lexbuf Stack.t
  ; mutable lexbuf: Lexing.lexbuf
  ; lexfunc: Lexing.lexbuf -> 'a
  ; path: string }

(* Get position of current lexeme. *)
let current_pos ls =
  let open Lexing in
  let pos = lexeme_end_p ls.lexbuf in
  { pos_fname= pos.pos_fname
  ; pos_lnum= pos.pos_lnum
  ; pos_bol= pos.pos_bol + String.length (lexeme ls.lexbuf)
  ; pos_cnum= pos.pos_cnum }

(*
** Create a lexstack with an initial lexbuf and the
** lexer function.
*)
let create lexbuf lexer_function =
  { stack= Stack.create ()
  ; path= Filename.dirname (Lexing.lexeme_end_p lexbuf).Lexing.pos_fname
  ; lexbuf
  ; lexfunc= lexer_function }

(*
** The the next token. Need to accept an unused dummy lexbuf so that
** a closure consisting of the function and a lexstack can be passed
** to the menhir generated parser.
*)
let rec get_token ls dummy_lexbuf =
  match ls.lexfunc ls.lexbuf with
  | Parser.INCLUDE fname ->
      Stack.push ls.lexbuf ls.stack ;
      let chan = open_in (ls.path ^ "/" ^ fname) in
      (* TODO: here we should look through all the include paths instead *)
      let open Lexing in
      ls.lexbuf <- from_channel chan ;
      (ls.lexbuf).lex_start_p
      <- {pos_fname= ls.path ^ "/" ^ fname; pos_lnum= 1; pos_bol= 0; pos_cnum= 0} ;
      (ls.lexbuf).lex_curr_p <- ls.lexbuf.lex_start_p ;
      get_token ls dummy_lexbuf
  | Parser.EOF -> (
      if Stack.is_empty ls.stack then Parser.EOF
      else
        match Stack.pop ls.stack with lb ->
          ls.lexbuf <- lb ;
          get_token ls dummy_lexbuf )
  | other_token -> other_token

(* Get current lexbuf *)
let current_lexbuf ls = ls.lexbuf

