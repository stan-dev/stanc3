(** The parser for Stan. A Menhir file. *)

%{
open Core_kernel
open Mir
open Ast
open Debugging
open Errors
(* Takes a sized_basic_type and a list of sizes and repeatedly applies then
   SArray constructor, taking sizes off the list *)
let reducearray (sbt, l) =
  List.fold l ~f:(fun y z -> SArray (y, z)) ~init:sbt
%}

%token FUNCTIONBLOCK DATABLOCK TRANSFORMEDDATABLOCK PARAMETERSBLOCK
       TRANSFORMEDPARAMETERSBLOCK MODELBLOCK GENERATEDQUANTITIESBLOCK
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK LABRACK RABRACK COMMA SEMICOLON
       BAR
%token RETURN IF ELSE WHILE FOR IN BREAK CONTINUE
%token VOID INT REAL VECTOR ROWVECTOR MATRIX ORDERED POSITIVEORDERED SIMPLEX
       UNITVECTOR CHOLESKYFACTORCORR CHOLESKYFACTORCOV CORRMATRIX COVMATRIX
%token LOWER UPPER OFFSET MULTIPLIER
%token <string> INTNUMERAL
%token <string> REALNUMERAL
%token <string> STRINGLITERAL
%token <string> IDENTIFIER
%token TARGET
%token QMARK COLON BANG MINUS PLUS HAT TRANSPOSE TIMES DIVIDE MODULO LDIVIDE
       ELTTIMES ELTDIVIDE OR AND EQUALS NEQUALS LEQ GEQ TILDE
%token ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN
   ELTDIVIDEASSIGN ELTTIMESASSIGN
%token ARROWASSIGN INCREMENTLOGPROB GETLP (* all of these are deprecated *)
%token PRINT REJECT
%token TRUNCATE
%token EOF

%right COMMA
%right QMARK COLON
%left OR
%left AND
%left EQUALS NEQUALS
%left LEQ LABRACK GEQ RABRACK
%left PLUS MINUS
%left TIMES DIVIDE MODULO ELTTIMES ELTDIVIDE
%left LDIVIDE
%nonassoc unary_over_binary
%right HAT
%left TRANSPOSE
%left LBRACK
%nonassoc below_ELSE
%nonassoc ELSE

(* Top level rule *)

%start <Ast.untyped_program> program
%%


(* Grammar *)

(* program *)
program:
  | ofb=option(function_block)
    odb=option(data_block)
    otdb=option(transformed_data_block)
    opb=option(parameters_block)
    otpb=option(transformed_parameters_block)
    omb=option(model_block)
    ogb=option(generated_quantities_block)
    EOF
    {
      grammar_logger "program" ;
      { functionblock= ofb
      ; datablock= odb
      ; transformeddatablock= otdb
      ; parametersblock= opb
      ; transformedparametersblock= otpb
      ; modelblock= omb
      ; generatedquantitiesblock= ogb }
    }

(* blocks *)
function_block:
  | FUNCTIONBLOCK LBRACE fd=list(function_def) RBRACE
    {  grammar_logger "function_block" ; fd}

data_block:
  | DATABLOCK LBRACE tvd=list(top_var_decl_no_assign) RBRACE
    { grammar_logger "data_block" ; tvd }

transformed_data_block:
  | TRANSFORMEDDATABLOCK LBRACE tvds=list(top_vardecl_or_statement) RBRACE
    {  grammar_logger "transformed_data_block" ;  tvds }
    (* NOTE: this allows mixing of statements and top_var_decls *)

parameters_block:
  | PARAMETERSBLOCK LBRACE tvd=list(top_var_decl_no_assign) RBRACE
    { grammar_logger "parameters_block" ; tvd }

transformed_parameters_block:
  | TRANSFORMEDPARAMETERSBLOCK LBRACE tvds=list(top_vardecl_or_statement) RBRACE
    { grammar_logger "transformed_parameters_block" ; tvds }

model_block:
  | MODELBLOCK LBRACE vds=list(vardecl_or_statement) RBRACE
    { grammar_logger "model_block" ; vds  }

generated_quantities_block:
  | GENERATEDQUANTITIESBLOCK LBRACE tvds=list(top_vardecl_or_statement) RBRACE
    { grammar_logger "generated_quantities_block" ; tvds }

(* function definitions *)
identifier:
  | id=IDENTIFIER
    {
      grammar_logger ("identifier " ^ id) ;
      {name=id; id_loc=loc_span_of_pos $startpos $endpos}
    }
  | TRUNCATE
    {
      grammar_logger "identifier T" ;
      {name="T"; id_loc=loc_span_of_pos $startpos $endpos}
    }

function_def:
  | rt=return_type name=identifier LPAREN args=separated_list(COMMA, arg_decl)
    RPAREN b=statement
    {
      grammar_logger "function_def" ;
      {stmt=FunDef {returntype = rt; funname = name;
                           arguments = args; body=b;};
       smeta={loc=loc_span_of_pos $startpos $endpos}
      }
    }

return_type:
  | VOID
    { grammar_logger "return_type VOID" ; Void }
  | ut=unsized_type
    {  grammar_logger "return_type unsized_type" ; ReturnType ut }

arg_decl:
  | od=option(DATABLOCK) ut=unsized_type id=identifier
    {  grammar_logger "arg_decl" ;
       match od with None -> (AutoDiffable, ut, id) | _ -> (DataOnly, ut, id)  }

unsized_type:
  | bt=basic_type ud=option(unsized_dims)
    {  grammar_logger "unsized_type" ;
       let rec reparray n x =
           if n <= 0 then x else reparray (n-1) (UArray x) in
       let size =
         match ud with Some d -> 1 + d | None -> 0
       in
       reparray size bt    }

basic_type:
  | INT
    {  grammar_logger "basic_type INT" ; UInt  }
  | REAL
    {  grammar_logger "basic_type REAL"  ; UReal }
  | VECTOR
    {  grammar_logger "basic_type VECTOR" ; UVector }
  | ROWVECTOR
    {  grammar_logger "basic_type ROWVECTOR" ; URowVector }
  | MATRIX
    {  grammar_logger "basic_type MATRIX" ; UMatrix }

unsized_dims:
  | LBRACK cs=list(COMMA) RBRACK
    { grammar_logger "unsized_dims" ; List.length(cs) }

(* declarations *)
var_decl:
  | sbt=sized_basic_type id=identifier d=option(dims)
    ae=option(pair(ASSIGN, expression)) SEMICOLON
    { grammar_logger "var_decl" ;
      let sizes = match d with None -> [] | Some l -> l in
      {stmt=
         VarDecl {sizedtype= reducearray (sbt, sizes);
                  transformation= Identity;
                  identifier= id;
                  initial_value=Option.map ~f:snd ae;
                  is_global= false};
       smeta= {loc = loc_span_of_pos $startpos $endpos}}
    }

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
  | tvt=top_var_type id=identifier d=option(dims) SEMICOLON
    {
      grammar_logger "top_var_decl_no_assign" ;
      let sizes = match d with None -> [] | Some l -> l in
      {stmt=
          VarDecl {sizedtype= reducearray (fst tvt, sizes);
                   transformation=  snd tvt;
                   identifier= id;
                   initial_value= None;
                   is_global= true};
       smeta={loc= loc_span_of_pos $startpos $endpos}
      }
    }

top_var_decl:
  | tvt=top_var_type id=identifier d=option(dims)
    ass=option(pair(ASSIGN, expression)) SEMICOLON
    { grammar_logger "top_var_decl" ;
      let sizes = match d with None -> [] | Some l -> l in
      {stmt=
              VarDecl {sizedtype= reducearray (fst tvt, sizes);
                       transformation=  snd tvt;
                       identifier= id;
                       initial_value= Option.map ~f:snd ass;
                       is_global= true};
       smeta= {loc=loc_span_of_pos $startpos $endpos}}
    }

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
  | LABRACK l=offset_mult RABRACK
    {  grammar_logger "type_constraint_offset_mult" ; l }

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

offset_mult:
  | OFFSET ASSIGN e1=constr_expression COMMA MULTIPLIER ASSIGN e2=constr_expression
    { grammar_logger "offset_mult" ; OffsetMultiplier (e1, e2) }
  | OFFSET ASSIGN e=constr_expression
    { grammar_logger "offset" ; Offset e }
  | MULTIPLIER ASSIGN e=constr_expression
    { grammar_logger "multiplier" ; Multiplier e }

dims:
  | LBRACK l=separated_nonempty_list(COMMA, expression) RBRACK
    { grammar_logger "dims" ; l  }

(* expressions *)
%inline expression:
  | l=lhs
    {
      grammar_logger "lhs_expression" ;
      match snd l with
        | [] -> {expr=Variable (fst l);
                 emeta = { loc=loc_span_of_pos $startpos $endpos}
                }
        | i ->
          {expr =
             Indexed
               ({expr =Variable (fst l);
                 emeta = { loc=loc_span_of_pos $startpos $endpos}
               }, i);
           emeta = { loc=loc_span_of_pos $startpos $endpos}
          }
    }
  | e=non_lhs
    { grammar_logger "non_lhs_expression" ;
      {expr=e;
       emeta={loc= loc_span_of_pos $startpos $endpos}}}

non_lhs:
  | e1=expression  QMARK e2=expression COLON e3=expression
    { grammar_logger "ifthenelse_expr" ; TernaryIf (e1, e2, e3) }
  | e1=expression op=infixOp e2=expression
    { grammar_logger "infix_expr" ; BinOp (e1, op, e2)  }
  | op=prefixOp e=expression %prec unary_over_binary
    { grammar_logger "prefix_expr" ; PrefixOp (op, e) }
  | e=expression op=postfixOp
    { grammar_logger "postfix_expr" ; PostfixOp (e, op)}
  | ue=non_lhs LBRACK i=indexes RBRACK
    {  grammar_logger "expression_indexed" ;
       Indexed ({expr=ue;
                 emeta={loc= loc_span_of_pos $startpos(ue)
                                                        $endpos(ue)}}, i)}
  | e=common_expression
    { grammar_logger "common_expr" ; e }

(* TODO: why do we not simply disallow greater than in constraints? No need to disallow all logical operations, right? *)
constr_expression:
  | e1=constr_expression op=arithmeticBinOp e2=constr_expression
    {
      grammar_logger "constr_expression_arithmetic" ;
      {expr=BinOp (e1, op, e2);
       emeta={loc=loc_span_of_pos $startpos $endpos}
      }
    }
  | op=prefixOp e=constr_expression %prec unary_over_binary
    {
      grammar_logger "constr_expression_prefixOp" ;
      {expr=PrefixOp (op, e);
       emeta={loc=loc_span_of_pos $startpos $endpos}}
    }
  | e=constr_expression op=postfixOp
    {
      grammar_logger "constr_expression_postfix" ;
      {expr=PostfixOp (e, op);
       emeta={loc=loc_span_of_pos $startpos $endpos}}
    }
  | e=constr_expression LBRACK i=indexes RBRACK
    {
      grammar_logger "constr_expression_indexed" ;
      {expr=Indexed (e, i);
       emeta={loc=loc_span_of_pos $startpos $endpos}}
    }
  | e=common_expression
    {
      grammar_logger "constr_expression_common_expr" ;
      {expr=e;
       emeta={loc= loc_span_of_pos $startpos $endpos}}
    }
  | id=identifier
    {
      grammar_logger "constr_expression_identifier" ;
      {expr=Variable id;
       emeta={loc=loc_span_of_pos $startpos $endpos}}
    }

common_expression:
  | i=INTNUMERAL
    {  grammar_logger ("intnumeral " ^ i) ; IntNumeral i }
  | r=REALNUMERAL
    {  grammar_logger ("realnumeral " ^ r) ; RealNumeral r }
  | LBRACE xs=separated_nonempty_list(COMMA, expression) RBRACE
    {  grammar_logger "array_expression" ; ArrayExpr xs  }
  | LBRACK xs=separated_nonempty_list(COMMA, expression) RBRACK
    {  grammar_logger "row_vector_expression" ; RowVectorExpr xs }
  | id=identifier LPAREN args=separated_list(COMMA, expression) RPAREN
    {  grammar_logger "fun_app" ; FunApp (UserDefined, id, args) }
  | TARGET LPAREN RPAREN
    { grammar_logger "target_read" ; GetTarget }
  | GETLP LPAREN RPAREN
    { grammar_logger "get_lp" ; GetLP } (* deprecated *)
  | id=identifier LPAREN e=expression BAR args=separated_list(COMMA, expression)
    RPAREN
    {  grammar_logger "conditional_dist_app" ; CondDistApp (id, e :: args) }
  | LPAREN e=expression RPAREN
    { grammar_logger "extra_paren" ; Paren e }

%inline prefixOp:
  | BANG
    {   grammar_logger "prefix_bang" ; PNot }
  | MINUS
    {  grammar_logger "prefix_minus" ; PMinus }
  | PLUS
    {   grammar_logger "prefix_plus" ; PPlus }

%inline postfixOp:
  | TRANSPOSE
    {  grammar_logger "postfix_transpose" ; Transpose }

%inline infixOp:
  | a=arithmeticBinOp
    {   grammar_logger "infix_arithmetic" ; a }
  | l=logicalBinOp
    {  grammar_logger "infix_logical" ; l }

%inline arithmeticBinOp:
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
    {  grammar_logger "infix_hat" ; Pow }

%inline logicalBinOp:
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
  | id=identifier
    {  grammar_logger "lhs_identifier" ; (id, []) }
  | l=lhs LBRACK id=indexes RBRACK
    {  grammar_logger "lhs_index" ; (fst l, (snd l)@id) }

(* statements *)
statement:
  | s=atomic_statement
    {  grammar_logger "atomic_statement" ;
       {stmt= s;
        smeta= { loc=loc_span_of_pos $startpos $endpos} }
    }
  | s=nested_statement
    {  grammar_logger "nested_statement" ;
       {stmt= s;
        smeta={loc = loc_span_of_pos $startpos $endpos} }
    }

atomic_statement:
  | l=lhs op=assignment_op e=expression SEMICOLON
    {  grammar_logger "assignment_statement" ; match l with (id, indices) ->
       Assignment {assign_identifier=id;
                   assign_indices=indices;
                   assign_op=op;
                   assign_rhs=e} }
  | id=identifier LPAREN args=separated_list(COMMA, expression) RPAREN SEMICOLON
    {  grammar_logger "funapp_statement" ; NRFunApp (UserDefined,id, args)  }
  | INCREMENTLOGPROB LPAREN e=expression RPAREN SEMICOLON
    {   grammar_logger "incrementlogprob_statement" ; IncrementLogProb e } (* deprecated *)
  | e=expression TILDE id=identifier LPAREN es=separated_list(COMMA, expression)
    RPAREN ot=option(truncation) SEMICOLON
    {  grammar_logger "tilde_statement" ;
       let t = match ot with Some tt -> tt | None -> NoTruncate in
       Tilde {arg= e; distribution= id; args= es; truncation= t  }
    }
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
    {  grammar_logger "return_nothing_statement" ; ReturnVoid }
  | SEMICOLON
    {  grammar_logger "skip" ; Skip }

%inline assignment_op:
  | ASSIGN
    {  grammar_logger "assign_plain" ; Assign }
  | ARROWASSIGN
    { grammar_logger "assign_arrow" ; ArrowAssign  } (* deprecated *)
  | PLUSASSIGN
    { grammar_logger "assign_plus" ; OperatorAssign Plus }
  | MINUSASSIGN
    { grammar_logger "assign_minus" ; OperatorAssign Minus }
  | TIMESASSIGN
    { grammar_logger "assign_times"  ; OperatorAssign Times }
  | DIVIDEASSIGN
    { grammar_logger "assign_divide" ; OperatorAssign Divide }
  | ELTTIMESASSIGN
    { grammar_logger "assign_elttimes"  ; OperatorAssign EltTimes }
  | ELTDIVIDEASSIGN
    { grammar_logger "assign_eltdivide" ; OperatorAssign EltDivide  }

string_literal:
  | s=STRINGLITERAL
    {  grammar_logger ("string_literal " ^ s) ; s }

truncation:
  | TRUNCATE LBRACK e1=option(expression) COMMA e2=option(expression)
    RBRACK
    {  grammar_logger "truncation" ;
       match (e1, e2) with
       | Some tt1, Some tt2 -> TruncateBetween (tt1, tt2)
       | Some tt1, None -> TruncateUpFrom tt1
       | None, Some tt2 -> TruncateDownFrom tt2
       | None, None -> NoTruncate  }

nested_statement:
  | IF LPAREN e=expression RPAREN s1=statement ELSE s2=statement
    {  grammar_logger "ifelse_statement" ; IfThenElse (e, s1, Some s2) }
  | IF LPAREN e=expression RPAREN s=statement %prec below_ELSE
    {  grammar_logger "if_statement" ; IfThenElse (e, s, None) }
  | WHILE LPAREN e=expression RPAREN s=statement
    {  grammar_logger "while_statement" ; While (e, s) }
  | FOR LPAREN id=identifier IN e1=expression COLON e2=expression RPAREN
    s=statement
    {
      grammar_logger "for_statement" ;
      For {loop_variable= id;
           lower_bound= e1;
           upper_bound= e2;
           loop_body= s;}
    }
  | FOR LPAREN id=identifier IN e=expression RPAREN s=statement
    {  grammar_logger "foreach_statement" ; ForEach (id, e, s) }
  | LBRACE l=list(vardecl_or_statement)  RBRACE
    {  grammar_logger "block_statement" ; Block l } (* NOTE: I am choosing to allow mixing of statements and var_decls *)

(* statement or var decls *)
vardecl_or_statement:
  | s=statement
    { grammar_logger "vardecl_or_statement_statement" ; s }
  | v=var_decl
    { grammar_logger "vardecl_or_statement_vardecl" ; v }

top_vardecl_or_statement:
  | s=statement
    { grammar_logger "top_vardecl_or_statement_statement" ;  s }
  | v=top_var_decl
    { grammar_logger "top_vardecl_or_statement_top_vardecl" ; v }
