(** The preprocessor to handle #include directives *)

(* The Lexstack type. *)
type 'a lexstack

(*
** Create a lexstack with an initial top level filename and the
** lexer function.
*)
val create : string -> (Lexing.lexbuf -> 'a) -> 'a lexstack

(*
** The the next token. Need to accept an unused dummy lexbuf so that
** a closure consisting of the function and a lexstack can be passed
** to the menhir generated parser.
*)
val get_token : Parser.MenhirInterpreter.token lexstack -> Lexing.lexbuf -> Parser.MenhirInterpreter.token

(* Get the current lexeme. *)
val lexeme : 'a lexstack -> string

(* Get filename, line number and column number of current lexeme. *)
val current_pos : 'a lexstack -> string * int * int
