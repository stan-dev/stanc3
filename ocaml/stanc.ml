let (fprintf) = Core_kernel.fprintf
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s line %d col %d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let () =
let lexbuf = Lexing.from_channel stdin in
try
  let ()  = print_newline () in
  let exp = parse_with_error lexbuf in
  print_string "hi";
  print_string (Lexing.lexeme lexbuf)
with
  | End_of_file -> print_string "EOF"
