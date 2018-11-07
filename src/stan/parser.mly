(* The parser for Stan *)

%{
open Semantic_actions
open Syntax
open Debug
%}

%token FUNCTIONBLOCK DATABLOCK TRANSFORMEDDATABLOCK PARAMETERSBLOCK
       TRANSFORMEDPARAMETERSBLOCK MODELBLOCK GENERATEDQUANTITIESBLOCK
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK LABRACK RABRACK COMMA SEMICOLON
       BAR
%token RETURN IF ELSE WHILE FOR IN BREAK CONTINUE
%token VOID INT REAL VECTOR ROWVECTOR MATRIX ORDERED POSITIVEORDERED SIMPLEX
       UNITVECTOR CHOLESKYFACTORCORR CHOLESKYFACTORCOV CORRMATRIX COVMATRIX
%token LOWER UPPER LOCATION SCALE
%token <string> INTNUMERAL
%token <string> REALNUMERAL
%token <string> STRINGLITERAL
%token <string> IDENTIFIER
%token TARGET
%token QMARK COLON BANG MINUS PLUS HAT QR TIMES DIVIDE MODULO LDIVIDE ELTTIMES
       ELTDIVIDE OR AND EQUALS NEQUALS LEQ GEQ TILDE
%token ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN ELTTIMESASSIGN
       ELTDIVIDEASSIGN
%token ARROWASSIGN INCREMENTLOGPROB GETLP (* deprecated *)
%token PRINT REJECT
%token TRUNCATE
%token EOF

(* TODO: Resolve remaining ambiguities in the grammar *)
%left LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%left QR
%right HAT
%nonassoc unary_over_binary BANG
%left ELTTIMES ELTDIVIDE
%left LDIVIDE
%left TIMES DIVIDE MODULO
%left PLUS MINUS
%left LEQ LABRACK GEQ RABRACK
%left EQUALS NEQUALS
%left AND
%left OR
%right QMARK COLON
%right TILDE ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN
       ELTTIMESASSIGN ELTDIVIDEASSIGN ARROWASSIGN
%right COMMA
%nonassoc below_ELSE
%nonassoc ELSE



(* TODO: deal properly with multi-file model *)

(* TODO: propagate line numbers through properly for reporting semantic errors *)

(* TODO: create decent parsing error messages: use menhir --list-errors to get overview of all possible errors to write custom messages for *)

(* Top level rule *)
%start program file
%type <Syntax.program> program
%type <Syntax.program list> file (* TODO: fix that a Stan file can only contain one program *)

%%

(* Helpers *)

(* [option(X)] represents a choice between nothing and [X].
   [option(X)] is the same thing, but is inlined at its use site,
   which in some cases is necessary in order to avoid a conflict.
   By convention, [X?] is syntactic sugar for [option(X)]. *)

%inline ioption(X):
  | (* nothing *)
    { None }
  | x=X
    { Some x }

option(X):
  | o = ioption(X)
    { o }

list(X):
  | (* nothing *)
    { [] }
  | x=X xs=list(X)
    { x::xs }

pair(X, Y):
  | x=X y=Y
    { (x,y) }

sep_list(X, Y):
  | (* nothing *)
    { [] }
  | x=X xys=list(pair(Y, X))
    { x :: List.map snd xys }

nonempty_sep_list(X, Y):
  | x=X xys=list(pair(Y, X))
    { x :: List.map snd xys }

(* Grammar *)

(* file-program *)
file:
  | p=program
    { ast_logger p ; [p]}

program:
  | obf=option(function_block)
    obd=option(data_block)
    obtd=option(transformed_data_block)
    obp=option(parameters_block)
    obtp=option(transformed_parameters_block)
    obm=option(model_block)
    obg=option(generated_quantities_block)
    EOF
    {
      grammar_logger "program" ; construct_program obf obd obtd obp obtp obm obg
    }

(* blocks *)
function_block:
  | FUNCTIONBLOCK LBRACE fd=list(function_def) RBRACE
    {  grammar_logger "function_block" ; Some fd}

data_block:
  | DATABLOCK LBRACE tvd=list(top_var_decl_no_assign) RBRACE
    { grammar_logger "data_block" ; Some tvd }

transformed_data_block:
  | TRANSFORMEDDATABLOCK LBRACE tvds=list(top_vardecl_or_statement) RBRACE
    {  grammar_logger "transformed_data_block" ; Some  tvds }
    (* NOTE: We are choosing to allow mixing of statements and top_var_decls *)

parameters_block:
  | PARAMETERSBLOCK LBRACE tvd=list(top_var_decl_no_assign) RBRACE
    { grammar_logger "parameters_block" ; Some tvd }

transformed_parameters_block:
  | TRANSFORMEDPARAMETERSBLOCK LBRACE tvds=list(top_vardecl_or_statement) RBRACE
    { grammar_logger "transformed_parameters_block" ; Some tvds }

model_block:
  | MODELBLOCK LBRACE vds=list(vardecl_or_statement) RBRACE
    { grammar_logger "model_block" ; Some vds  }

generated_quantities_block:
  | GENERATEDQUANTITIESBLOCK LBRACE tvds=list(top_vardecl_or_statement) RBRACE
    { grammar_logger "generated_quantities_block" ; Some  tvds }

(* function definitions *)
function_def:
  | rt=return_type name=IDENTIFIER LPAREN args=sep_list(arg_decl, COMMA) RPAREN
    b=statement
    { 
      grammar_logger "function_def" ;
      {returntype = rt; name = name; arguments = args; body=b;}
    }

return_type:
  | VOID
    { grammar_logger "return_type VOID" ; Void }
  | ut=unsized_type
    {  grammar_logger "return_type unsized_type" ; ReturnType ut }

arg_decl:
  | od=option(DATABLOCK) ut=unsized_type id=IDENTIFIER
    {  grammar_logger "arg_decl" ; construct_arg_decl od ut id }

unsized_type:
  | bt=basic_type ud=option(unsized_dims)
    {  grammar_logger "unsized_type" ; construct_unsized_type bt ud  }

basic_type:
  | INT
    {  grammar_logger "basic_type INT" ; Int  }
  | REAL
    {  grammar_logger "basic_type REAL"  ; Real }
  | VECTOR
    {  grammar_logger "basic_type VECTOR" ; Vector  }
  | ROWVECTOR
    {  grammar_logger "basic_type ROWVECTOR" ; RowVector  }
  | MATRIX
    {  grammar_logger "basic_type MATRIX" ; Matrix  }

unsized_dims:
  | LBRACK cs=list(COMMA) RBRACK
    { grammar_logger "unsized_dims" ; List.length(cs) }

(* declarations *)
var_decl:
  | sbt=sized_basic_type id=IDENTIFIER d=option(dims)
    ae=option(pair(ASSIGN, expression)) SEMICOLON
    { grammar_logger "var_decl" ; construct_var_decl sbt id d ae  }

sized_basic_type:
  | INT
    { grammar_logger "INT_var_type" ; SInt }
  | REAL
    { grammar_logger "REAL_var_type" ; SReal }
  | VECTOR LBRACK e=expression RBRACK
    { grammar_logger "VECTOR_var_type" ; SVector e }
  | ROWVECTOR LBRACK e=expression RBRACK
    { grammar_logger "ROWVECTOR_var_type" ; SRowVector e  }
  | MATRIX LBRACK e1=expression COMMA e2=expression RBRACK
    { grammar_logger "MATRIX_var_type" ; SMatrix (e1, e2) }

top_var_decl_no_assign:
  | tvt=top_var_type id=IDENTIFIER d=option(dims) SEMICOLON
    {
      grammar_logger "top_var_decl" ;
      construct_top_var_decl_no_assign tvt id d
    }

top_var_decl:
  | tvt=top_var_type id=IDENTIFIER d=option(dims)
    ass=option(pair(ASSIGN, expression)) SEMICOLON
    { grammar_logger "top_var_decl"  ; construct_top_var_decl tvt id d ass }

top_var_type:
  | INT r=range_constraint
    { grammar_logger "INT_top_var_type" ; (SInt, r) }
  | REAL c=type_constraint
    { grammar_logger "REAL_top_var_type" ; (SReal, c) }
  | VECTOR c=type_constraint LBRACK e=expression RBRACK
    { grammar_logger "VECTOR_top_var_type" ; (SVector e, c) }
  | ROWVECTOR c=type_constraint LBRACK e=expression RBRACK
    { grammar_logger "ROWVECTOR_top_var_type" ; (SRowVector e, c) }
  | MATRIX c=type_constraint LBRACK e1=expression COMMA e2=expression RBRACK
    { grammar_logger "MATRIX_top_var_type" ; (SMatrix (e1, e2), c) }
  | ORDERED LBRACK e=expression RBRACK
    { grammar_logger "ORDERED_top_var_type" ; (SVector e, Ordered) }
  | POSITIVEORDERED LBRACK e=expression RBRACK
    {
      grammar_logger "POSITIVEORDERED_top_var_type" ;
      (SVector e, PositiveOrdered)
    }
  | SIMPLEX LBRACK e=expression RBRACK
    { grammar_logger "SIMPLEX_top_var_type" ; (SVector e, Simplex) }
  | UNITVECTOR LBRACK e=expression RBRACK
    { grammar_logger "UNITVECTOR_top_var_type" ; (SVector e, UnitVector) }
  | CHOLESKYFACTORCORR LBRACK e=expression RBRACK
    { 
      grammar_logger "CHOLESKYFACTORCORR_top_var_type" ;
      (SMatrix (e, e), CholeskyCorr)
    }
  | CHOLESKYFACTORCOV LBRACK e1=expression oe2=option(pair(COMMA, expression))
    RBRACK
    {
      grammar_logger "CHOLESKYFACTORCOV_top_var_type" ;
      match oe2 with Some (_,e2) -> ( SMatrix (e1, e2), CholeskyCov)
                   | _           ->  (SMatrix (e1, e1),  CholeskyCov)
    }
  | CORRMATRIX LBRACK e=expression RBRACK
    { grammar_logger "CORRMATRIX_top_var_type" ; (SMatrix (e, e), Correlation) }
  | COVMATRIX LBRACK e=expression RBRACK
    { grammar_logger "COVMATRIX_top_var_type" ; (SMatrix (e, e), Covariance) }

type_constraint:
  | r=range_constraint
    {  grammar_logger "type_constraint_range" ; r }
  | LABRACK l=loc_scale RABRACK
    {  grammar_logger "type_constraint_loc_scale" ; l }

range_constraint:
  | (* nothing *)
    { grammar_logger "empty_constraint" ; Identity }
  | LABRACK r=range RABRACK
    {  grammar_logger "range_constraint" ; r }

range:
  | LOWER ASSIGN e1=constr_expression COMMA UPPER ASSIGN e2=constr_expression
    { grammar_logger "lower_upper_range" ; LowerUpper (e1, e2) }
  | LOWER ASSIGN e=constr_expression
    {  grammar_logger "lower_range" ; Lower e }
  | UPPER ASSIGN e=constr_expression
    { grammar_logger "upper_range" ; Upper e }

loc_scale:
  | LOCATION ASSIGN e1=constr_expression COMMA SCALE ASSIGN e2=constr_expression
    { grammar_logger "loc_scale" ; LocationScale (e1, e2) }
  | LOCATION ASSIGN e=constr_expression
    {  grammar_logger "loc" ; LocationScale (e, (RealNumeral "1.", None)) }
  | SCALE ASSIGN e=constr_expression
    { grammar_logger "scale" ; LocationScale ((RealNumeral "0.", None), e) }

dims:
  | LBRACK l=sep_list(expression, COMMA) RBRACK
    { grammar_logger "dims" ; l  }

(* expressions *)
expression:
  | l=lhs
    { 
      grammar_logger "lhs_expression" ;
      (Indexed ((Variable (fst l), None), snd l), None)
    }
  | e=non_lhs
    { grammar_logger "non_lhs_expression" ; (e, None)}

non_lhs: (* to avoid shift/reduce conflict with lhs when doing assignments *)
  | e1=expression QMARK e2=expression COLON e3=expression 
    { grammar_logger "ifthenelse_expr" ; Conditional (e1, e2, e3) }
  | e1=expression op=infixOp e2=expression 
    { grammar_logger "infix_expr" ; InfixOp (e1, op, e2)  }
  | op=prefixOp e=expression %prec unary_over_binary 
    { grammar_logger "prefix_expr" ; PrefixOp (op, e) }
  | e=expression op=postfixOp 
    { grammar_logger "postfix_expr" ; PostfixOp (e, op)}
  | ue=non_lhs LBRACK i=indexes RBRACK 
    {  grammar_logger "expression_indexed" ; Indexed ((ue, None), i)}
  | e=common_expression 
    { grammar_logger "common_expr" ; e }

(* TODO: why do we not simply disallow greater than in constraints? No need to disallow all logical operations, right? *)
constr_expression:
  | e1=constr_expression op=arithmeticInfixOp e2=constr_expression 
    { 
      grammar_logger "constr_expression_arithmetic" ;
      (InfixOp (e1, op, e2), None)
    }
  | op=prefixOp e=constr_expression %prec unary_over_binary 
    {  grammar_logger "constr_expression_prefixOp" ; (PrefixOp (op, e), None) }
  | e=constr_expression op=postfixOp 
    {  grammar_logger "constr_expression_postfix" ; (PostfixOp (e, op), None) }
  | e=constr_expression LBRACK i=indexes RBRACK 
    {  grammar_logger "constr_expression_indexed" ; (Indexed (e, i), None) }
  | e=common_expression 
    {  grammar_logger "constr_expression_common_expr" ; (e, None) }
  | id=IDENTIFIER 
    { grammar_logger "constr_expression_identifier" ; (Variable id, None) }

common_expression:
  | i=INTNUMERAL 
    {  grammar_logger "intnumeral" ; IntNumeral i }
  | r=REALNUMERAL 
    {  grammar_logger "realnumeral" ; RealNumeral r }
  | LBRACE xs=nonempty_sep_list(expression, COMMA) RBRACE 
    {  grammar_logger "array_expression" ; ArrayExpr xs  } (* potential shift/reduce conflict with blocks *)
  | LBRACK xs=nonempty_sep_list(expression, COMMA) RBRACK 
    {  grammar_logger "row_vector_expression" ; RowVectorExpr xs } (* potential shift/reduce conflict with indexing *)
  | id=IDENTIFIER LPAREN args=sep_list(expression, COMMA) RPAREN 
    {  grammar_logger "fun_app" ; FunApp (id, args) }
  | TARGET LPAREN RPAREN 
    { grammar_logger "target_read" ; GetTarget }
  | GETLP LPAREN RPAREN 
    { grammar_logger "get_lp" ; GetLP } (* deprecated *)
  | id=IDENTIFIER LPAREN e=expression BAR args=sep_list(expression, COMMA)
    RPAREN 
    {  grammar_logger "conditional_dist_app" ; CondFunApp (id, e :: args) }
  | LPAREN e=expression RPAREN 
    { grammar_logger "extra_paren" ; Paren e }

prefixOp:
  | BANG 
    {   grammar_logger "prefix_bang" ; Not }
  | MINUS 
    {  grammar_logger "prefix_minus" ; UMinus }
  | PLUS 
    {   grammar_logger "prefix_plus" ; UPlus }

postfixOp:
  | QR 
    {  grammar_logger "postfix_transpose" ; Transpose }

infixOp:
  | a=arithmeticInfixOp 
    {   grammar_logger "infix_arithmetic" ; a }
  | l=logicalInfixOp 
    {  grammar_logger "infix_logical" ; l }

arithmeticInfixOp:
  | PLUS 
    {  grammar_logger "infix_plus" ; Plus }
  | MINUS 
    {   grammar_logger "infix_minus" ; Minus }
  | TIMES 
    {  grammar_logger "infix_times" ; Times }
  | DIVIDE 
    {  grammar_logger "infix_divide" ; Divide }
  | MODULO 
    {  grammar_logger "infix_modulo" ; Modulo }
  | LDIVIDE 
    {  grammar_logger "infix_ldivide" ; LDivide }
  | ELTTIMES 
    {  grammar_logger "infix_elttimes" ; EltTimes }
  | ELTDIVIDE 
    {   grammar_logger "infix_eltdivide" ; EltDivide }
  | HAT 
    {  grammar_logger "infix_hat" ; Exp }

logicalInfixOp:
  | OR 
    {   grammar_logger "infix_or" ; Or }
  | AND 
    {   grammar_logger "infix_and" ; And }
  | EQUALS 
    {   grammar_logger "infix_equals" ; Equals }
  | NEQUALS 
    {   grammar_logger "infix_nequals" ; NEquals}
  | LABRACK 
    {   grammar_logger "infix_less" ; Less }
  | LEQ 
    {   grammar_logger "infix_leq" ; Leq }
  | RABRACK 
    {   grammar_logger "infix_greater" ; Greater }
  | GEQ 
    {   grammar_logger "infix_geq" ; Geq }

indexes:
  | (* nothing *) 
    {   grammar_logger "index_nothing" ; [All] }
  | COLON 
    {   grammar_logger "index_all" ; [All] }
  | e=expression 
    {  grammar_logger "index_single" ; [Single e] }
  | e=expression COLON 
    {  grammar_logger "index_upper" ; [Upfrom e] }
  | COLON e=expression 
    {   grammar_logger "index_lower" ; [Downfrom e] }
  | e1=expression COLON e2=expression
    {  grammar_logger "index_twosided" ; [Between (e1, e2)] }
  | i1=indexes COMMA i2=indexes 
    {  grammar_logger "indexes" ; i1 @ i2 }

printables:
  | e=expression 
    {  grammar_logger "printable expression" ; [PExpr e] }
  | s=string_literal 
    {  grammar_logger "printable string" ; [PString s] }
  | p1=printables COMMA p2=printables 
    { grammar_logger "printables" ; p1 @ p2 }

(* L-values *)
lhs:
  | id=IDENTIFIER   
    {  grammar_logger "lhs_identifier" ; (id, []) }
  | l=lhs LBRACK id=indexes RBRACK 
    {  grammar_logger "lhs_index" ; (fst l, (snd l)@id) }

(* statements *)
statement:
  | s=atomic_statement   
    {  grammar_logger "atomic_statement" ; (s, None) }
  | s=nested_statement 
    {  grammar_logger "nested_statement" ; (s, None) }

atomic_statement:
  | l=lhs op=assignment_op e=expression SEMICOLON  
    {  grammar_logger "assignment_statement" ; Assignment (l, op, e) }
  | id=IDENTIFIER LPAREN args=sep_list(expression, COMMA) RPAREN SEMICOLON 
    {  grammar_logger "funapp_statement" ; NRFunApp (id, args)  }
  | INCREMENTLOGPROB LPAREN e=expression RPAREN SEMICOLON 
    {   grammar_logger "incrementlogprob_statement" ; IncrementLogProb e } (* deprecated *)
  | e=expression TILDE id=IDENTIFIER LPAREN es=sep_list(expression, COMMA)
    RPAREN ot=option(truncation) SEMICOLON 
    {  grammar_logger "tilde_statement" ; construct_tilde_statement e id es ot }
  | TARGET PLUSASSIGN e=expression SEMICOLON 
    {   grammar_logger "targetpe_statement" ; TargetPE e }
  | BREAK SEMICOLON 
    {  grammar_logger "break_statement" ; Break }
  | CONTINUE SEMICOLON 
    {  grammar_logger "continue_statement" ; Continue }
  | PRINT LPAREN l=printables RPAREN SEMICOLON 
    {  grammar_logger "print_statement" ; Print l }
  | REJECT LPAREN l=printables RPAREN SEMICOLON 
    {  grammar_logger "reject_statement" ; Reject l  }
  | RETURN e=expression SEMICOLON 
    {  grammar_logger "return_statement" ; Return e }
  | RETURN SEMICOLON 
    {  grammar_logger "return_nothing_statement" ; Skip }
  | SEMICOLON 
    {  grammar_logger "skip" ; Skip }

assignment_op:
  | ASSIGN 
    {  grammar_logger "assign_plain" ; Assign }
  | PLUSASSIGN 
    { grammar_logger "assign_plus" ; PlusAssign }
  | MINUSASSIGN 
    { grammar_logger "assign_minus" ; MinusAssign }
  | TIMESASSIGN 
    { grammar_logger "assign_times"  ; TimesAssign }
  | DIVIDEASSIGN 
    { grammar_logger "assign_divide" ; DivideAssign }
  | ELTTIMESASSIGN 
    { grammar_logger "assign_elttimes"  ; EltTimesAssign }
  | ELTDIVIDEASSIGN 
    { grammar_logger "assign_eltdivide" ; EltDivideAssign  }
  | ARROWASSIGN 
    { grammar_logger "assign_arrow" ; ArrowAssign  } (* deprecated *)

string_literal:
  | s=STRINGLITERAL 
    {  grammar_logger "string_literal" ; s }

truncation:
  | TRUNCATE e1=option(expression) COMMA e2=option(expression) RBRACK 
    {  grammar_logger "truncation" ; construct_truncation e1 e2 }

nested_statement:
  | IF LPAREN e=expression RPAREN s1=statement ELSE s2=statement 
    {  grammar_logger "ifelse_statement" ; IfThenElse (e, s1, s2) }
  | IF LPAREN e=expression RPAREN s=statement %prec below_ELSE 
    {  grammar_logger "if_statement" ; IfThen (e, s) }
  | WHILE LPAREN e=expression RPAREN s=statement 
    {  grammar_logger "while_statement" ; While (e, s) }
  | FOR LPAREN id=IDENTIFIER IN e1=expression COLON e2=expression RPAREN
    s=statement 
    {
      grammar_logger "for_statement" ;
      For {loop_variable = id;
           lower_bound= e1;
           upper_bound =  e2;
           loop_body = s;}
    }
  | FOR LPAREN id=IDENTIFIER IN e=expression RPAREN s=statement 
    {  grammar_logger "foreach_statement" ; ForEach (id, e, s) }
  | LBRACE l=list(vardecl_or_statement)  RBRACE  
    {  grammar_logger "block_statement" ; Block l } (* NOTE: I am choosing to allow mixing of statements and var_decls *)

(* statement or var decls *)
vardecl_or_statement:
  | s=statement 
    { grammar_logger "vardecl_or_statement_statement" ; Stmt s }
  | v=var_decl 
    { grammar_logger "vardecl_or_statement_vardecl" ; v }

top_vardecl_or_statement:
  | s=statement 
    { grammar_logger "top_vardecl_or_statement_statement" ; TStmt s }
  | v=top_var_decl 
    { grammar_logger "top_vardecl_or_statement_top_vardecl" ; v }