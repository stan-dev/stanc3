(** The lexer that will feed into the parser. An OCamllex file. *)

{
  module Stack = Core_kernel.Stack
  open Lexing
  open Debugging
  open Preprocessor

(* Boilerplate for getting line numbers for errors *)
  let incr_linenum lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
}

(* Some auxiliary definition for variables and constants *)
let string_literal = '"' [^'"']* '"'
let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*   (* TODO: We should probably expand the alphabet *)

let integer_constant =  ['0'-'9']+

let exp_literal = ['e' 'E'] ['+' '-']? integer_constant
let real_constant1 = integer_constant? '.' ['0'-'9']* exp_literal?
let real_constant2 = '.' ['0'-'9']+ exp_literal?
let real_constant3 = integer_constant exp_literal
let real_constant = real_constant1 | real_constant2 | real_constant3
let space = ' ' | '\t' | '\012' | '\r'
let non_space_or_newline = [^' ' '\t' '\012' '\r' '\n']

rule token = parse
(* White space, line numers and comments *)
    '\n'                      { lexer_logger "newline" ;
                                incr_linenum lexbuf ; token lexbuf }
  | space                     { lexer_logger "space" ; token lexbuf }
  | "/*"                      { lexer_logger "multicomment" ;
                                multiline_comment lexbuf ; token lexbuf }
  | "//"                      { lexer_logger "single comment" ;
                                singleline_comment lexbuf ; token lexbuf }
  | "#include"
    ( ( space | '\n')+)
    ( '"' [^ '"']* '"'
    | non_space_or_newline*
    as fname)                 { lexer_logger ("include " ^ fname) ;
                                let new_lexbuf =
                                  try_get_new_lexbuf fname lexbuf.lex_curr_p in
                                token new_lexbuf }
  | "#"                       { lexer_logger "#comment" ;
                                Errors.warn_deprecated
                                  (lexbuf.lex_curr_p, "Comments beginning with \
                                                       # are deprecated. \
                                                       Please use // in place \
                                                       of # for line \
                                                       comments.") ;
                                singleline_comment lexbuf; token lexbuf } (* deprecated *)
(* Program blocks *)
  | "functions"               { lexer_logger "functions" ;
                                Parser.FUNCTIONBLOCK }
  | "data"                    { lexer_logger "data" ; Parser.DATABLOCK }
  | "transformed data"        { lexer_logger "transformed data" ;
                                Parser.TRANSFORMEDDATABLOCK }
  | "parameters"              { lexer_logger "parameters" ;
                                Parser.PARAMETERSBLOCK }
  | "transformed parameters"  { lexer_logger "transformed parameters" ;
                                Parser.TRANSFORMEDPARAMETERSBLOCK }
  | "model"                   { lexer_logger "model" ; Parser.MODELBLOCK }
  | "generated quantities"    { lexer_logger "generated quantities" ;
                                Parser.GENERATEDQUANTITIESBLOCK }
(* Punctuation *)
  | '{'                       { lexer_logger "{" ; Parser.LBRACE }
  | '}'                       { lexer_logger "}" ; Parser.RBRACE }
  | '('                       { lexer_logger "(" ; Parser.LPAREN }
  | ')'                       { lexer_logger ")" ; Parser.RPAREN }
  | '['                       { lexer_logger "[" ; Parser.LBRACK }
  | ']'                       { lexer_logger "]" ; Parser.RBRACK }
  | '<'                       { lexer_logger "<" ; Parser.LABRACK }
  | '>'                       { lexer_logger ">" ; Parser.RABRACK }
  | ','                       { lexer_logger "," ; Parser.COMMA }
  | ';'                       { lexer_logger ";" ; Parser.SEMICOLON }
  | '|'                       { lexer_logger "|" ; Parser.BAR }
(* Control flow keywords *)
  | "return"                  { lexer_logger "return" ; Parser.RETURN }
  | "if"                      { lexer_logger "if" ; Parser.IF }
  | "else"                    { lexer_logger "else" ; Parser.ELSE }
  | "while"                   { lexer_logger "while" ; Parser.WHILE }
  | "for"                     { lexer_logger "for" ; Parser.FOR }
  | "in"                      { lexer_logger "in" ; Parser.IN }
  | "break"                   { lexer_logger "break" ; Parser.BREAK }
  | "continue"                { lexer_logger "continue" ; Parser.CONTINUE }
(* Types *)
  | "void"                    { lexer_logger "void" ; Parser.VOID }
  | "int"                     { lexer_logger "int" ; Parser.INT }
  | "real"                    { lexer_logger "real" ; Parser.REAL }
  | "vector"                  { lexer_logger "vector" ; Parser.VECTOR }
  | "row_vector"              { lexer_logger "row_vector" ; Parser.ROWVECTOR }
  | "matrix"                  { lexer_logger "matrix" ; Parser.MATRIX }
  | "ordered"                 { lexer_logger "ordered" ; Parser.ORDERED }
  | "positive_ordered"        { lexer_logger "positive_ordered" ;
                                Parser.POSITIVEORDERED }
  | "simplex"                 { lexer_logger "simplex" ; Parser.SIMPLEX }
  | "unit_vector"             { lexer_logger "unit_vector" ; Parser.UNITVECTOR }
  | "cholesky_factor_corr"    { lexer_logger "cholesky_factor_corr" ;
                                Parser.CHOLESKYFACTORCORR }
  | "cholesky_factor_cov"     { lexer_logger "cholesky_factor_cov" ;
                                Parser.CHOLESKYFACTORCOV }
  | "corr_matrix"             { lexer_logger "corr_matrix" ; Parser.CORRMATRIX }
  | "cov_matrix"              { lexer_logger "cov_matrix" ; Parser.COVMATRIX }
(* Transformation keywords *)
  | "lower"                   { lexer_logger "lower" ; Parser.LOWER }
  | "upper"                   { lexer_logger "upper" ; Parser.UPPER }
  | "offset"                  { lexer_logger "offset" ; Parser.OFFSET }
  | "multiplier"              { lexer_logger "multiplier" ; Parser.MULTIPLIER }
(* Operators *)
  | '?'                       { lexer_logger "?" ; Parser.QMARK }
  | ':'                       { lexer_logger ":" ; Parser.COLON }
  | '!'                       { lexer_logger "!" ; Parser.BANG }
  | '-'                       { lexer_logger "-" ; Parser.MINUS }
  | '+'                       { lexer_logger "+" ; Parser.PLUS }
  | '^'                       { lexer_logger "^" ; Parser.HAT }
  | '\''                      { lexer_logger "\'" ; Parser.TRANSPOSE }
  | '*'                       { lexer_logger "*" ; Parser.TIMES }
  | '/'                       { lexer_logger "/" ; Parser.DIVIDE }
  | '%'                       { lexer_logger "%" ; Parser.MODULO }
  | "\\"                      { lexer_logger "\\" ; Parser.LDIVIDE }
  | ".*"                      { lexer_logger ".*" ; Parser.ELTTIMES }
  | "./"                      { lexer_logger "./" ; Parser.ELTDIVIDE }
  | "||"                      { lexer_logger "||" ; Parser.OR }
  | "&&"                      { lexer_logger "&&" ; Parser.AND }
  | "=="                      { lexer_logger "==" ; Parser.EQUALS }
  | "!="                      { lexer_logger "!=" ; Parser.NEQUALS }
  | "<="                      { lexer_logger "<=" ; Parser.LEQ }
  | ">="                      { lexer_logger ">=" ; Parser.GEQ }
  | "~"                       { lexer_logger "~" ; Parser.TILDE }
(* Assignments *)
  | '='                       { lexer_logger "=" ; Parser.ASSIGN }
  | "+="                      { lexer_logger "+=" ; Parser.PLUSASSIGN }
  | "-="                      { lexer_logger "-=" ; Parser.MINUSASSIGN }
  | "*="                      { lexer_logger "*=" ; Parser.TIMESASSIGN }
  | "/="                      { lexer_logger "/=" ; Parser.DIVIDEASSIGN }
  | ".*="                     { lexer_logger ".*=" ; Parser.ELTTIMESASSIGN }
  | "./="                     { lexer_logger "./=" ; Parser.ELTDIVIDEASSIGN }
  | "<-"                      { lexer_logger "<-" ;
                                Errors.warn_deprecated
                                  (lexbuf.lex_curr_p, "assignment operator <- \
                                                       is deprecated in the \
                                                       Stan language; use = \
                                                       instead.") ;
                                Parser.ARROWASSIGN } (* deprecated *)
  | "increment_log_prob"      { lexer_logger "increment_log_prob" ;
                                Errors.warn_deprecated
                                  (lexbuf.lex_curr_p, "increment_log_prob(...)\
                                                       ; is deprecated and \
                                                       will be removed in the \
                                                       future. Use target \
                                                       += ...; instead.") ;
                                Parser.INCREMENTLOGPROB } (* deprecated *)
(* Effects *)
  | "print"                   { lexer_logger "print" ; Parser.PRINT }
  | "reject"                  { lexer_logger "reject" ; Parser.REJECT }
  | 'T'                       { lexer_logger "T" ; Parser.TRUNCATE } (* TODO: this is a hack; we should change to something like truncate and make it a reserved keyword *)
(* Constants and identifiers *)
  | integer_constant as i     { lexer_logger ("int_constant " ^ i) ;
                                Parser.INTNUMERAL (lexeme lexbuf) }
  | real_constant as r        { lexer_logger ("real_constant " ^ r) ;
                                Parser.REALNUMERAL (lexeme lexbuf) }
  | "target"                  { lexer_logger "target" ; Parser.TARGET } (* NB: the stanc2 parser allows variables to be named target. I think it's a bad idea and have disallowed it. *)
  | "get_lp"                  { lexer_logger "get_lp" ;
                                Errors.warn_deprecated
                                  (lexbuf.lex_curr_p, "get_lp() function is \
                                                       deprecated. It will be \
                                                       removed in a future \
                                                       release. Use target() \
                                                       instead.") ;
                                Parser.GETLP } (* deprecated *)
  | string_literal as s       { lexer_logger ("string_literal " ^ s) ;
                                Parser.STRINGLITERAL (lexeme lexbuf) }
  | identifier as id          { lexer_logger ("identifier " ^ id) ;
                                Parser.IDENTIFIER (lexeme lexbuf) }
(* End of file *)
  | eof                       { lexer_logger "eof" ;
                                if Stack.length include_stack = 1
                                then Parser.EOF
                                else
                                  let _ = (Stack.pop_exn include_stack) in
                                  let old_lexbuf =
                                    (Stack.top_exn include_stack) in
                                      token old_lexbuf }

  | _                         { raise (Errors.SyntaxError
                                (Lexing (lexeme (Stack.top_exn include_stack),
                                        Errors.location_of_position
                                        (lexeme_start_p
                                          (Stack.top_exn include_stack))))) }

(* Multi-line comment terminated by "*/" *)
and multiline_comment = parse
  | "*/"   { () }
  | eof    { failwith "unterminated comment" }
  | '\n'   { incr_linenum lexbuf; multiline_comment lexbuf }
  | _      { multiline_comment lexbuf }

(* Single-line comment terminated by a newline *)
and singleline_comment = parse
  | '\n'   { incr_linenum lexbuf }
  | eof    { () }
  | _      { singleline_comment lexbuf }

{
}
