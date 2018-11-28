(** The preprocessor to handle #include directives *)

(* The Lexstack type. *)
type 'a lexstack

(*
** Create a lexstack with an initial lexbuf and the
** lexer function.
*)
val create : Lexing.lexbuf -> (Lexing.lexbuf -> 'a) -> 'a lexstack

(*
** The the next token. Need to accept an unused dummy lexbuf so that
** a closure consisting of the function and a lexstack can be passed
** to the menhir generated parser.
*)
val get_token :
     Parser.MenhirInterpreter.token lexstack
  -> Lexing.lexbuf
  -> Parser.MenhirInterpreter.token

(* Get start position of current lexeme. *)
val start_pos : 'a lexstack -> Lexing.position

(* Get end position of current lexeme. *)
val end_pos : 'a lexstack -> Lexing.position

(* Get current lexbuf *)
val current_lexbuf : 'a lexstack -> Lexing.lexbuf
