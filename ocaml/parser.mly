%{
    open Ast
    %}
%token <int> INT
%token <float> REAL
%token <string> IDENT

%token DATA INTTYPE REALTYPE
%token TRANSFORMED_DATA
%token PARAMETERS
%token TRANSFORMED_PARAMETERS
%token MODEL
%token GENERATED_QUANTITIES
%token VECTOR MATRIX ORDERED POSITIVE_ORDERED SIMPLEX UNIT_VECTOR ROW_VECTOR
%token CHOLESKY_FACTOR_CORR CHOLESKY_FACTOR_COV CORR_MATRIX COV_MATRIX
%token IF ELSE WHILE FOR
%token EQ SIM PLUS MINUS MULT DIV LE GE LEQ GEQ
%token PLUS_EQ MINUS_EQ MULT_EQ DIV_EQ
%token ELMULT ELDIV
%token DEF RET
%token LBRACE RBRACE
%token PLEFT PRIGHT
%token SQLEFT SQRIGHT
%token COMMA SEMICOLON
%token QMARK COLON BANG IN
%token GENQUANT
%left PLUS PLUS_EQ
%left MINUS
%left MULT MULT_EQ
%left DIV DIV_EQ
%left NEG
%token EOF
%start <Ast.stanProg> prog

%%

(*
prog: MODEL statement { Prog([], $2, Unit) }
*)
prog: expression SEMICOLON { Prog($1) }
statement: atomic_statement { $1 } | nested_statement { $1 }

atomic_statement: atomic_statement_body SEMICOLON { $1 }

atomic_statement_body:
  | lhs assignment_op expression { Assign($2, $1, $3) }
  | typePrim lhs { VarDecl($1, $2) }
(*
  | expression '~' identifier '(' expressions ')' ?truncation
  | function_literal '(' expressions ')'
  | 'increment_log_prob' '(' expression ')'
  | 'target' '+=' expression
  | 'break'
  | 'continue'
  | 'print' '(' (expression | string_literal)* ')'
  | 'reject' '(' (expression | string_literal)* ')'
  | 'return' expression
  | ''
*)

nested_statement:
  | IF PLEFT expression PRIGHT statement { If($3, $5, Unit) }
  | IF PLEFT expression PRIGHT statement ELSE statement { If($3, $5, $7) }
  | WHILE PLEFT expression PRIGHT statement { While($3, $5) }
  | FOR PLEFT IDENT IN expression COLON expression PRIGHT statement { For($3, $5, $7, $9) }
//  | '{' var_decl* statement+ '}'

lhs: IDENT { $1 }

assignment_op: EQ { Eq }
  | PLUS_EQ { Add }
  | MINUS_EQ { Sub }
  | DIV_EQ { Div }
  | MULT_EQ { Mul }

expression: IDENT { Var $1 }
  | INT { IntLit $1 }
  | REAL { NumLit $1 }
  | expression QMARK expression COLON expression { If($1, $3, $5) }
  | expression PLUS expression { FnApp("add", [$1; $3]) }
  | expression MINUS expression { FnApp("sub", [$1; $3]) }
  | expression MULT expression { FnApp("mul", [$1; $3]) }
  | expression DIV expression { FnApp("div", [$1; $3]) }
  | MINUS expression { FnApp("Neg", [$2]) }
  | BANG expression { FnApp("Not", [$2]) }

typePrim: REALTYPE { TReal } | INTTYPE { TInt } | VECTOR expression { TVector $2 }

%%
