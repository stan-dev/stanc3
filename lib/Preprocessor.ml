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
  { stack= Stack.create ()
  ; filename= top_filename
  ; chan
  ; lexbuf= Lexing.from_channel chan
  ; lexfunc= lexer_function }

(*
** The the next token. Need to accept an unused dummy lexbuf so that
** a closure consisting of the function and a lexstack can be passed
** to the ocamlyacc generated parser.
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

(* Get filename, line number and column number of current lexeme. *)
let current_pos ls =
  let pos = Lexing.lexeme_end_p ls.lexbuf in
  let linepos =
    pos.Lexing.pos_cnum - pos.Lexing.pos_bol
    - String.length (Lexing.lexeme ls.lexbuf)
  in
  (ls.filename, pos.Lexing.pos_lnum, linepos)
