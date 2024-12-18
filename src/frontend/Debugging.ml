(** Some helpers for debugging *)

(** Controls whether the lexing operations get logged *)
let lexer_logging = ref false

let lexer_logger s = if !lexer_logging then print_endline ("Lexer: " ^ s)

let lexer_pos_logger (pos : Lexing.position) =
  if !lexer_logging then
    print_endline
      ("{fname=" ^ pos.pos_fname ^ "; line=" ^ string_of_int pos.pos_lnum ^ "}")

(** Controls whether the parsing operations get logged *)
let grammar_logging = ref false

let grammar_logger s = if !grammar_logging then print_endline ("Parser: " ^ s)
