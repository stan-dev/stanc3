(* lexer.mll -*- tuareg -*- *)
{
  open Core_kernel
  open Lexing
  open Parser

  exception SyntaxError of string

  let keywords = String.Map.of_alist_exn
      [
        "def", DEF;
        "real", REALTYPE;
        "int",  INTTYPE;
        "vector", VECTOR;
        "matrix", MATRIX;
        "ordered", ORDERED;
        "positive_ordered", POSITIVE_ORDERED;
        "simplex", SIMPLEX;
        "unit_vector", UNIT_VECTOR;
        "row_vector", ROW_VECTOR;
        "cholesky_factor_corr", CHOLESKY_FACTOR_CORR;
        "cholesky_factor_cov", CHOLESKY_FACTOR_COV;
        "corr_matrix", CORR_MATRIX;
        "cov_matrix", COV_MATRIX;
        "data", DATA;
        "return",  RET;
        "if", IF;
        "else", ELSE;
        "while", WHILE;
        "for", FOR;
        "transformed data", TRANSFORMED_DATA;
        "parameters", PARAMETERS;
        "transformed parameters", TRANSFORMED_PARAMETERS;
        "model", MODEL;
        "generated quantities", GENERATED_QUANTITIES;
        "genquant", GENQUANT;
        "in", IN;
      ]

  let ops =  String.Map.of_alist_exn
      [
        "=", EQ;
        "~", SIM;
        "+", PLUS;
        "*", MULT;
        "-", MINUS;
        "/", DIV;
        ".*", ELMULT;
        "./", ELDIV;
        ">", GE;
        "<", LE;
        ">=", GEQ;
        "<=", LEQ;
        "+=", PLUS_EQ;
        "*=", MULT_EQ;
        "/=", DIV_EQ;
      ]

  let lexeme = Lexing.lexeme

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }

}

let char = ['a'-'z' 'A'-'Z']
let int = ['-' '+']? ['0'-'9']+
          let digit = ['0'-'9']
let frac = '.' digit*
           let float =  digit* frac?

let identifier  = char(char|digit|['-' '_' '.'])*
let operator    = "~" | "=" | "+" | "*" | "-" | "/" | ".*" | "./" | ">" | "<" | ">=" | "<="

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | whitespace    { read lexbuf }
  | newline  { new_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (lexeme lexbuf)) }
  | float    { REAL (float_of_string (lexeme lexbuf)) }
  | operator      { String.Map.find_exn ops (lexeme lexbuf) }
  | identifier    { match String.Map.find keywords (lexeme lexbuf) with
        | Some(token) -> token
        | None -> IDENT(lexeme lexbuf) }
  | '{'		    { LBRACE }
  | '}'		    { RBRACE }
  | '('		    { PLEFT }
  | ')'		    { PRIGHT }
  | '['		    { SQLEFT }
  | ']'		    { SQRIGHT }
  | ','       { COMMA }
  | ';'			  { SEMICOLON }
  | '?'			  { QMARK }
  | ':'			  { COLON }
  | '!'			  { BANG }
  | eof       { EOF }
  | _ { raise (SyntaxError
                 (sprintf "SyntaxError: Unexpected char: '%s' Line: %d Column: %d"
                    (lexeme lexbuf) (lexbuf.lex_start_p.pos_lnum + 1)
                    lexbuf.lex_start_p.pos_cnum))
      }
