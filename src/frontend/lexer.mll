(** The lexer that will feed into the parser. An OCamllex file. *)

{
  module Stack = Core.Stack
  module Queue = Core.Queue
  open Lexing
  open Debugging
  open Preprocessor

(* Boilerplate for getting line numbers for errors *)
  let incr_linenum lexbuf =
    lexer_pos_logger lexbuf.lex_curr_p;
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum } ;
    update_start_positions lexbuf.lex_curr_p


  let note_deprecated_comment pos =
    let loc_span = location_span_of_positions (pos, pos) in
    Deprecation_removals.pound_comment_usages := loc_span :: !Deprecation_removals.pound_comment_usages

  (* Store comments *)
  let add_line_comment (begin_pos, buffer) end_pos =
    add_comment
    @@ LineComment
         ( Buffer.contents buffer
         , location_span_of_positions (begin_pos, end_pos) )

  let add_multi_comment begin_pos lines end_pos =
    add_comment
    @@ Ast.BlockComment (lines, location_span_of_positions (begin_pos, end_pos))

  let add_separator lexbuf =
    add_comment @@ Separator (location_of_position lexbuf.lex_curr_p)

  let add_include fname lexbuf =
    add_comment
    @@ Include
         ( fname
         , location_span_of_positions (lexbuf.lex_start_p, lexbuf.lex_curr_p) )
}

(* Some auxiliary definition for variables and constants *)
let string_literal = '"' [^ '"' '\r' '\n']* '"'
let identifier = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*   (* TODO: We should probably expand the alphabet *)

let integer_constant =  ['0'-'9']+ ('_' ['0'-'9']+)*

let exp_literal = ['e' 'E'] ['+' '-']? integer_constant
let real_constant1 = integer_constant '.' integer_constant? exp_literal?
let real_constant2 = '.' integer_constant exp_literal
let real_constant3 = integer_constant exp_literal
let real_constant_dot = '.' integer_constant
let real_constant = real_constant1 | real_constant2 | real_constant3
let imag_constant = (integer_constant | real_constant | real_constant_dot) 'i'
let space = ' ' | '\t' | '\012'
let newline = '\r' | '\n' | '\r'*'\n'
let non_space_or_newline =  [^ ' ' '\t' '\012' '\r' '\n' ]

rule token = parse
(* White space, line numbers and comments *)
  | newline                   { lexer_logger "newline" ;
                                incr_linenum lexbuf ; token lexbuf }
  | space                     { lexer_logger "space" ; token lexbuf }
  | "/*"                      { lexer_logger "multicomment" ;
                                multiline_comment ((lexbuf.lex_curr_p, []), Buffer.create 16) lexbuf ; token lexbuf }
  | "//"                      { lexer_logger "single comment" ;
                                singleline_comment (lexbuf.lex_curr_p, Buffer.create 16) lexbuf ;
                                token lexbuf }
  | "#include"
    ( ( space | newline)+)
    ( '"' ([^ '"' '\r' '\n']* as fname) '"'
    | '<' ([^ '>' '\r' '\n']* as fname) '>'
    | (non_space_or_newline* as fname)
    )                         { lexer_logger ("include " ^ fname) ;
                                add_include fname lexbuf ;
                                let new_lexbuf =
                                  try_get_new_lexbuf fname in
                                token new_lexbuf }
  | "#"                       { lexer_logger "#comment" ;
                                note_deprecated_comment lexbuf.lex_curr_p ;
                                singleline_comment (lexbuf.lex_curr_p, Buffer.create 16) lexbuf;
                                token lexbuf } (* deprecated *)
(* Program blocks *)
  | "functions"               { lexer_logger "functions" ;
                                Parser.FUNCTIONBLOCK }
  | "data"                    { lexer_logger "data" ; Parser.DATABLOCK }
  | "transformed"
      ( space+ )
      "data"                  { lexer_logger "transformed data" ;
                                Parser.TRANSFORMEDDATABLOCK }
  | "parameters"              { lexer_logger "parameters" ;
                                Parser.PARAMETERSBLOCK }
  | "transformed"
      ( space+ )
      "parameters"            { lexer_logger "transformed parameters" ;
                                Parser.TRANSFORMEDPARAMETERSBLOCK }
  | "model"                   { lexer_logger "model" ; Parser.MODELBLOCK }
  | "generated"
      ( space+ )
      "quantities"    { lexer_logger "generated quantities" ;
                                Parser.GENERATEDQUANTITIESBLOCK }
(* Punctuation *)
  | '{'                       { lexer_logger "{" ; Parser.LBRACE }
  | '}'                       { lexer_logger "}" ; Parser.RBRACE }
  | '('                       { lexer_logger "(" ; Parser.LPAREN }
  | ')'                       { lexer_logger ")" ; Parser.RPAREN }
  | '['                       { lexer_logger "[" ; Parser.LBRACK }
  | ']'                       { lexer_logger "]" ; Parser.RBRACK }
  | '<'                       { lexer_logger "<" ; add_separator lexbuf ; Parser.LABRACK }
  | '>'                       { lexer_logger ">" ; add_separator lexbuf ; Parser.RABRACK }
  | ','                       { lexer_logger "," ; add_separator lexbuf ; Parser.COMMA }
  | ';'                       { lexer_logger ";" ; Parser.SEMICOLON }
  | '|'                       { lexer_logger "|" ; add_separator lexbuf ; Parser.BAR }
(* Control flow keywords *)
  | "return"                  { lexer_logger "return" ; Parser.RETURN }
  | "if"                      { lexer_logger "if" ; Parser.IF }
  | "else"                    { lexer_logger "else" ; add_separator lexbuf ; Parser.ELSE }
  | "while"                   { lexer_logger "while" ; Parser.WHILE }
  | "profile"                 { lexer_logger "profile" ; Parser.PROFILE }
  | "for"                     { lexer_logger "for" ; Parser.FOR }
  | "in"                      { lexer_logger "in" ; Parser.IN }
  | "break"                   { lexer_logger "break" ; Parser.BREAK }
  | "continue"                { lexer_logger "continue" ; Parser.CONTINUE }
(* Types *)
  | "void"                    { lexer_logger "void" ; Parser.VOID }
  | "int"                     { lexer_logger "int" ; Parser.INT }
  | "real"                    { lexer_logger "real" ; Parser.REAL }
  | "complex"                 { lexer_logger "complex" ; Parser.COMPLEX }
  | "vector"                  { lexer_logger "vector" ; Parser.VECTOR }
  | "row_vector"              { lexer_logger "row_vector" ; Parser.ROWVECTOR }
  | "complex_vector"          { lexer_logger "complex_vector" ; Parser.COMPLEXVECTOR }
  | "complex_row_vector"      { lexer_logger "complex_row_vector" ; Parser.COMPLEXROWVECTOR }
  | "tuple"                   { lexer_logger "tuple" ; Parser.TUPLE }
  | "array"                   { lexer_logger "array" ; Parser.ARRAY }
  | "matrix"                  { lexer_logger "matrix" ; Parser.MATRIX }
  | "complex_matrix"          { lexer_logger "complex_matrix" ; Parser.COMPLEXMATRIX }
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
  | '?'                       { lexer_logger "?" ; add_separator lexbuf ; Parser.QMARK }
  | ':'                       { lexer_logger ":" ; Parser.COLON }
  | '!'                       { lexer_logger "!" ; Parser.BANG }
  | '-'                       { lexer_logger "-" ; add_separator lexbuf ; Parser.MINUS }
  | '+'                       { lexer_logger "+" ; add_separator lexbuf ; Parser.PLUS }
  | '^'                       { lexer_logger "^" ; add_separator lexbuf ; Parser.HAT }
  | '\''                      { lexer_logger "\'" ; Parser.TRANSPOSE }
  | '*'                       { lexer_logger "*" ; add_separator lexbuf ; Parser.TIMES }
  | '/'                       { lexer_logger "/" ; add_separator lexbuf ; Parser.DIVIDE }
  | '%'                       { lexer_logger "%" ; add_separator lexbuf ; Parser.MODULO }
  | "%/%"                     { lexer_logger "%/%" ; add_separator lexbuf ; Parser.IDIVIDE }
  | "\\"                      { lexer_logger "\\" ; add_separator lexbuf ; Parser.LDIVIDE }
  | ".*"                      { lexer_logger ".*" ; add_separator lexbuf ; Parser.ELTTIMES }
  | ".^"                      { lexer_logger ".^" ; add_separator lexbuf ; Parser.ELTPOW }
  | "./"                      { lexer_logger "./" ; add_separator lexbuf ; Parser.ELTDIVIDE }
  | "||"                      { lexer_logger "||" ; add_separator lexbuf ; Parser.OR }
  | "&&"                      { lexer_logger "&&" ; add_separator lexbuf ; Parser.AND }
  | "=="                      { lexer_logger "==" ; add_separator lexbuf ; Parser.EQUALS }
  | "!="                      { lexer_logger "!=" ; add_separator lexbuf ; Parser.NEQUALS }
  | "<="                      { lexer_logger "<=" ; add_separator lexbuf ; Parser.LEQ }
  | ">="                      { lexer_logger ">=" ; add_separator lexbuf ; Parser.GEQ }
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
                                Parser.ARROWASSIGN } (* deprecated *)
  | "increment_log_prob"      { lexer_logger "increment_log_prob" ;
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
  | real_constant_dot as r    { lexer_logger ("real_constant_dot " ^ r) ;
                                (* Separated out because ".1" could be a number or a tuple projection *)
                                Parser.DOTNUMERAL (lexeme lexbuf) }
  | imag_constant as z        { lexer_logger ("imag_constant " ^ z) ;
                                Parser.IMAGNUMERAL (lexeme lexbuf) }
  | "target"                  { lexer_logger "target" ; Parser.TARGET } (* NB: the stanc2 parser allows variables to be named target. I think it's a bad idea and have disallowed it. *)
  | "get_lp"                  { lexer_logger "get_lp" ;
                                Parser.GETLP } (* deprecated *)
  | string_literal as s       { lexer_logger ("string_literal " ^ s) ;
                                Parser.STRINGLITERAL (lexeme lexbuf) }
  | identifier as id          { lexer_logger ("identifier " ^ id) ;
                                lexer_pos_logger (lexeme_start_p lexbuf);
                                Parser.IDENTIFIER (lexeme lexbuf) }
(* End of file *)
  | eof                       { lexer_logger "eof" ;
                                if Preprocessor.size () = 1
                                then Parser.EOF
                                else
                                  let old_lexbuf = restore_prior_lexbuf () in
                                  token old_lexbuf }

  | _                         { raise (Errors.SyntaxError
                                        (Errors.Lexing
                                          (location_of_position
                                            (lexeme_start_p
                                              (current_buffer ()))))) }

(* Multi-line comment terminated by "*/" *)
and multiline_comment state = parse
  | "*/"     { let ((pos, lines), buffer) = state in
               let lines = (Buffer.contents buffer) :: lines in
               add_multi_comment pos (List.rev lines) lexbuf.lex_curr_p;
               update_start_positions lexbuf.lex_curr_p }
  | eof      { raise (Errors.SyntaxError
                      (Errors.UnexpectedEOF
                        (location_of_position lexbuf.lex_curr_p))) }
  | newline  { incr_linenum lexbuf;
               let ((pos, lines), buffer) = state in
               let lines = (Buffer.contents buffer) :: lines in
               let newbuf = Buffer.create 16 in
               multiline_comment ((pos, lines), newbuf) lexbuf }
  | _        { Buffer.add_string (snd state) (lexeme lexbuf) ; multiline_comment state lexbuf }

(* Single-line comment terminated by a newline *)
and singleline_comment state = parse
  | newline  { add_line_comment state lexbuf.lex_curr_p ; incr_linenum lexbuf }
  | eof      { add_line_comment state lexbuf.lex_curr_p ; update_start_positions lexbuf.lex_curr_p }
  | _        { Buffer.add_string (snd state) (lexeme lexbuf) ; singleline_comment state lexbuf }

{
}
