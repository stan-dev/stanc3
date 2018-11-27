(** The preprocessor to handle #include directives *)

(* The Lexstack type. *)
type 'a lexstack =
  { stack: (string * in_channel * Lexing.lexbuf) Stack.t
  ; mutable filename: string
  ; mutable chan: in_channel
  ; mutable lexbuf: Lexing.lexbuf
  ; lexfunc: Lexing.lexbuf -> 'a }

(*
** Create a lexstack with an initial top level filename and the
** lexer function.
*)
let create top_filename lexer_function =
  let chan = open_in top_filename in
  let open Lexing in
  let lexbuf = from_channel chan in
  lexbuf.lex_start_p
  <- {pos_fname= top_filename; pos_lnum= 1; pos_bol= 0; pos_cnum= 0} ;
  lexbuf.lex_curr_p <- lexbuf.lex_start_p ;
  { stack= Stack.create ()
  ; filename= top_filename
  ; chan
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
      Stack.push (ls.filename, ls.chan, ls.lexbuf) ls.stack ;
      ls.filename <- fname ;
      ls.chan <- open_in fname ;
      ls.lexbuf <- Lexing.from_channel ls.chan ;
      get_token ls dummy_lexbuf
  | Parser.EOF -> (
      if Stack.is_empty ls.stack then Parser.EOF
      else
        match Stack.pop ls.stack with fn, ch, lb ->
          ls.filename <- fn ;
          ls.chan <- ch ;
          ls.lexbuf <- lb ;
          get_token ls dummy_lexbuf )
  | anything -> anything

(* Get the current lexeme. *)
let lexeme ls = Lexing.lexeme ls.lexbuf

(* Get position of current lexeme. *)
let current_pos ls =
  let open Lexing in
  let pos = lexeme_end_p ls.lexbuf in
  { pos_fname= ls.filename
  ; pos_lnum= pos.pos_lnum
  ; pos_bol= pos.pos_bol + String.length (lexeme ls.lexbuf)
  ; pos_cnum= pos.pos_cnum }

let current_lexbuf ls = ls.lexbuf
