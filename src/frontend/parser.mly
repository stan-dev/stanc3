(** The parser for Stan. A Menhir file. *)

%{
open Core_kernel
open Middle
open Ast
open Debugging

(* Takes a sized_basic_type and a list of sizes and repeatedly applies then
   SArray constructor, taking sizes off the list *)
let reducearray (sbt, l) =
  List.fold_right l ~f:(fun z y -> SizedType.SArray (y, z)) ~init:sbt

let build_id id loc =
  grammar_logger ("identifier " ^ id);
  {name=id; id_loc=Location_span.of_positions_exn loc}

let rec iterate_n f x = function
  | 0 -> x
  | n -> iterate_n f (f x) (n - 1)
let nest_unsized_array basic_type n =
  iterate_n (fun t -> UnsizedType.UArray t) basic_type n

(* $sloc and $symbolstartpos generates code using !=, which
    Core_kernel considers to be an error.
 *)
let (!=) = Stdlib.(!=)
%}

(* Token definitions. The quoted strings are aliases, used in the examples generated in
   parser.messages. They have no semantic meaning; see
   http://gallium.inria.fr/~fpottier/menhir/manual.html#sec%3Atokens
*)
%token FUNCTIONBLOCK "functions" DATABLOCK "data"
       TRANSFORMEDDATABLOCK "transformed data" PARAMETERSBLOCK "parameters"
       TRANSFORMEDPARAMETERSBLOCK "transformed parameters" MODELBLOCK "model"
       GENERATEDQUANTITIESBLOCK "generated quantities"
%token LBRACE "{" RBRACE "}" LPAREN "(" RPAREN ")" LBRACK "[" RBRACK "]"
       LABRACK "<" RABRACK ">" COMMA "," SEMICOLON ";" BAR "|"
%token RETURN "return" IF "if" ELSE "else" WHILE "while" FOR "for" IN "in"
       BREAK "break" CONTINUE "continue" PROFILE "profile"
%token VOID "void" INT "int" REAL "real" COMPLEX "complex" VECTOR "vector"
       ROWVECTOR "row_vector" ARRAY "array" MATRIX "matrix" ORDERED "ordered"
       POSITIVEORDERED "positive_ordered" SIMPLEX "simplex" UNITVECTOR "unit_vector"
       CHOLESKYFACTORCORR "cholesky_factor_corr" CHOLESKYFACTORCOV "cholesky_factor_cov"
       CORRMATRIX "corr_matrix" COVMATRIX "cov_matrix"
%token LOWER "lower" UPPER "upper" OFFSET "offset" MULTIPLIER "multiplier"
%token <string> INTNUMERAL "24"
%token <string> REALNUMERAL "3.1415"
%token <string> IMAGNUMERAL "1i"
%token <string> STRINGLITERAL "\"hello world\""
%token <string> IDENTIFIER "foo"
%token TARGET "target"
%token QMARK "?" COLON ":" BANG "!" MINUS "-" PLUS "+" HAT "^" ELTPOW ".^" TRANSPOSE "'"
       TIMES "*" DIVIDE "/" MODULO "%" IDIVIDE "%/%" LDIVIDE "\\" ELTTIMES ".*"
       ELTDIVIDE "./" OR "||" AND "&&" EQUALS "==" NEQUALS "!=" LEQ "<=" GEQ ">=" TILDE "~"
%token ASSIGN "=" PLUSASSIGN "+=" MINUSASSIGN "-=" TIMESASSIGN "*="
       DIVIDEASSIGN "/=" ELTDIVIDEASSIGN "./=" ELTTIMESASSIGN ".*="
%token ARROWASSIGN "<-" INCREMENTLOGPROB "increment_log_prob" GETLP "get_lp" (* all of these are deprecated *)
%token PRINT "print" REJECT "reject"
%token TRUNCATE "T"
%token EOF ""

(* UNREACHABLE tokens will never be produced by the lexer, so we can use them as
   "a thing that will never parse". This is useful in a few places. For example,
   when we the parser to differentiate between different failing states for
   error message purposes, we can partially accept one of them and then fail by
   requiring an UNREACHABLE token.
 *)
%token UNREACHABLE "<<<<UNREACHABLE>>>"

%right COMMA
%right QMARK COLON
%left OR
%left AND
%left EQUALS NEQUALS
%left LEQ LABRACK GEQ RABRACK
%left PLUS MINUS
%left TIMES DIVIDE MODULO ELTTIMES ELTDIVIDE
%left IDIVIDE LDIVIDE
%nonassoc unary_over_binary
%right HAT ELTPOW
%left TRANSPOSE
%nonassoc ARRAY (* resolves shift-reduce with array keyword in declarations *)
%left LBRACK
%nonassoc below_ELSE
%nonassoc ELSE

(* Top level rule *)
%start <Ast.untyped_program> program functions_only
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
      (* check for empty programs*)
      let () =
        match (ofb, odb, otdb, opb, otpb, omb, ogb) with
        | None, None, None, None, None, None, None ->
            Input_warnings.empty ($startpos).pos_fname
        | _ -> ()
      in
      { functionblock= ofb
      ; datablock= odb
      ; transformeddatablock= otdb
      ; parametersblock= opb
      ; transformedparametersblock= otpb
      ; modelblock= omb
      ; generatedquantitiesblock= ogb
      ; comments= [] }
    }

functions_only:
  | fd = list(function_def) EOF
    { grammar_logger "functions_only";
      { functionblock= Some {stmts= fd; xloc= Location_span.of_positions_exn $loc}
      ; datablock= None
      ; transformeddatablock= None
      ; parametersblock= None
      ; transformedparametersblock= None
      ; modelblock= None
      ; generatedquantitiesblock= None
      ; comments= [] }
    }

(* blocks *)
function_block:
  | FUNCTIONBLOCK LBRACE fd=list(function_def) RBRACE
    { grammar_logger "function_block" ;
      {stmts= fd; xloc= Location_span.of_positions_exn $loc} }

data_block:
  | DATABLOCK LBRACE tvd=list(top_var_decl_no_assign) RBRACE
    { grammar_logger "data_block" ;
      {stmts= List.concat tvd; xloc= Location_span.of_positions_exn $loc} }

transformed_data_block:
  | TRANSFORMEDDATABLOCK LBRACE tvds=list(top_vardecl_or_statement) RBRACE
    { grammar_logger "transformed_data_block" ;
      {stmts= List.concat tvds; xloc= Location_span.of_positions_exn $loc} }
    (* NOTE: this allows mixing of statements and top_var_decls *)

parameters_block:
  | PARAMETERSBLOCK LBRACE tvd=list(top_var_decl_no_assign) RBRACE
    { grammar_logger "parameters_block" ;
      {stmts= List.concat tvd; xloc= Location_span.of_positions_exn $loc} }

transformed_parameters_block:
  | TRANSFORMEDPARAMETERSBLOCK LBRACE tvds=list(top_vardecl_or_statement) RBRACE
    { grammar_logger "transformed_parameters_block" ;
      {stmts= List.concat tvds; xloc= Location_span.of_positions_exn $loc} }

model_block:
  | MODELBLOCK LBRACE vds=list(vardecl_or_statement) RBRACE
    { grammar_logger "model_block" ;
      {stmts= List.concat vds; xloc= Location_span.of_positions_exn $loc} }

generated_quantities_block:
  | GENERATEDQUANTITIESBLOCK LBRACE tvds=list(top_vardecl_or_statement) RBRACE
    { grammar_logger "generated_quantities_block" ;
      {stmts= List.concat tvds; xloc= Location_span.of_positions_exn $loc} }

(* function definitions *)
identifier:
  | id=IDENTIFIER { build_id id $loc }
  | TRUNCATE { build_id "T" $loc}
  | id_and_v = future_keyword
    {
      let id, v = id_and_v in
      Input_warnings.future_keyword id.name v $loc;
      id
    }

future_keyword:
  | OFFSET { build_id "offset" $loc, "2.32.0" }
  | MULTIPLIER { build_id "multiplier" $loc, "2.32.0" }
  | LOWER { build_id "lower" $loc, "2.32.0" }
  | UPPER { build_id "upper" $loc, "2.32.0" }
  | ARRAY
    { build_id "array" $loc, "2.32.0" }

decl_identifier:
  | id=identifier { id }
  | id=reserved_word { id }

reserved_word:
  (* Keywords cannot be identifiers but
     semantic check produces a better error message. *)
  | FUNCTIONBLOCK { build_id "functions" $loc }
  | DATABLOCK { build_id "data" $loc }
  | PARAMETERSBLOCK { build_id "parameters" $loc }
  | MODELBLOCK { build_id "model" $loc }
  | RETURN { build_id "return" $loc }
  | IF { build_id "if" $loc }
  | ELSE { build_id "else" $loc }
  | WHILE { build_id "while" $loc }
  | FOR { build_id "for" $loc }
  | IN { build_id "in" $loc }
  | BREAK { build_id "break" $loc }
  | CONTINUE { build_id "continue" $loc }
  | VOID { build_id "void" $loc }
  | INT { build_id "int" $loc }
  | REAL { build_id "real" $loc }
  | COMPLEX { build_id "complex" $loc }
  | VECTOR { build_id "vector" $loc }
  | ROWVECTOR { build_id "row_vector" $loc }
  | MATRIX { build_id "matrix" $loc }
  | ORDERED { build_id "ordered" $loc }
  | POSITIVEORDERED { build_id "positive_ordered" $loc }
  | SIMPLEX { build_id "simplex" $loc }
  | UNITVECTOR { build_id "unit_vector" $loc }
  | CHOLESKYFACTORCORR { build_id "cholesky_factor_corr" $loc }
  | CHOLESKYFACTORCOV { build_id "cholesky_factor_cov" $loc }
  | CORRMATRIX { build_id "corr_matrix" $loc }
  | COVMATRIX { build_id "cov_matrix" $loc }
  | PRINT { build_id "print" $loc }
  | REJECT { build_id "reject" $loc }
  | TARGET { build_id "target" $loc }
  | GETLP { build_id "get_lp" $loc }
  | PROFILE { build_id "profile" $loc }

function_def:
  | rt=return_type name=decl_identifier LPAREN args=separated_list(COMMA, arg_decl)
    RPAREN b=statement
    {
      grammar_logger "function_def" ;
      {stmt=FunDef {returntype = rt; funname = name;
                           arguments = args; body=b;};
       smeta={loc=Location_span.of_positions_exn $loc}
      }
    }

return_type:
  | VOID
    { grammar_logger "return_type VOID" ; Void }
  | ut=unsized_type
    {  grammar_logger "return_type unsized_type" ; ReturnType ut }

arg_decl:
  | od=option(DATABLOCK) ut=unsized_type id=decl_identifier
    {  grammar_logger "arg_decl" ;
       match od with None -> (UnsizedType.AutoDiffable, ut, id) | _ -> (DataOnly, ut, id)  }

unsized_type:
  | ARRAY n=unsized_dims bt=basic_type
      {  grammar_logger "unsized_type";
       nest_unsized_array bt n
    }
  | bt=basic_type n_opt=option(unsized_dims)
    {  grammar_logger "unsized_type";
       if Option.is_some n_opt then
        Input_warnings.array_syntax ~unsized:true $loc;
       nest_unsized_array bt (Option.value n_opt ~default:0)
    }

basic_type:
  | INT
    {  grammar_logger "basic_type INT" ; UnsizedType.UInt  }
  | REAL
    {  grammar_logger "basic_type REAL"  ; UnsizedType.UReal }
  | COMPLEX
    { grammar_logger "basic_type COMPLEX" ; UnsizedType.UComplex }
  | VECTOR
    {  grammar_logger "basic_type VECTOR" ; UnsizedType.UVector }
  | ROWVECTOR
    {  grammar_logger "basic_type ROWVECTOR" ; UnsizedType.URowVector }
  | MATRIX
    {  grammar_logger "basic_type MATRIX" ; UnsizedType.UMatrix }

unsized_dims:
  | LBRACK cs=list(COMMA) RBRACK
    { grammar_logger "unsized_dims" ; List.length(cs) + 1 }

(* Never accept this rule, but return the same type as expression *)
no_assign:
  | UNREACHABLE
    { (* This code will never be reached *)
      raise (Failure "This should be unreachable; the UNREACHABLE token should \
                      never be produced")
    }

optional_assignment(rhs):
  | rhs_opt=option(pair(ASSIGN, rhs))
    { Option.map ~f:snd rhs_opt }

id_and_optional_assignment(rhs):
  | id=decl_identifier rhs_opt=optional_assignment(rhs)
    { (id, rhs_opt) }

(*
 * All rules for declaration statements.
 * The first argument matches the type and should return a (type, constraint) pair.
 * The second argument matches the RHS expression and should return an expression
 *   (or use no_assign to never allow a RHS).
 *
 * The value returned is a function from a bool (is_global, which controls
 * whether the declarations should be global variables) to a list of statements
 * (which will always be declarations).
 *
 * The rules match declarations with/without assignments, with/without array
 * dimensions, single/multiple identifiers, and dimensions before/after the
 * identifier.
 *)
decl(type_rule, rhs):
  (* This rule matches the old array syntax, e.g:
       int x[1,2] = ..;

     We need to match it separately because we won't support multiple inline
     declarations using this form.

     This form is deprecated.
   *)
  | ty=type_rule id=decl_identifier dims=dims rhs_opt=optional_assignment(rhs)
      SEMICOLON
    { Input_warnings.array_syntax $loc;
      (fun ~is_global ->
      [{ stmt=
          VarDecl {
              decl_type= Sized (reducearray (fst ty, dims))
            ; transformation= snd ty
            ; identifier= id
            ; initial_value= rhs_opt
            ; is_global
            }
      ; smeta= {
          loc= Location_span.of_positions_exn $loc
        }
    }])
    }

  (* This rule matches non-array declarations and also the new array syntax, e.g:
       array[1,2] int x = ..;
   *)
  (* Note that the array dimensions option must be inlined with ioption, else
     it will conflict with first rule. *)
  | dims_opt=ioption(arr_dims) ty=type_rule
     vs=separated_nonempty_list(COMMA, id_and_optional_assignment(rhs)) SEMICOLON
    { (fun ~is_global ->
      let dims = Option.value ~default:[] dims_opt
      in
      List.map vs ~f:(fun (id, rhs_opt) ->
          { stmt=
              VarDecl {
                  decl_type= Sized (reducearray (fst ty, dims))
                ; transformation= snd ty
                ; identifier= id
                ; initial_value= rhs_opt
                ; is_global
                }
          ; smeta= {
              loc= Location_span.of_positions_exn $sloc
            }
          })
    )}

var_decl:
  | d_fn=decl(sized_basic_type, expression)
    { grammar_logger "var_decl" ;
      d_fn ~is_global:false
    }

top_var_decl:
  | d_fn=decl(top_var_type, expression)
    { grammar_logger "top_var_decl" ;
      d_fn ~is_global:true
    }

top_var_decl_no_assign:
  | d_fn=decl(top_var_type, no_assign)
    { grammar_logger "top_var_decl_no_assign" ;
      d_fn ~is_global:true
    }
  | SEMICOLON
    { grammar_logger "top_var_decl_no_assign_skip";
      [ { stmt= Skip
        ; smeta= { loc= Location_span.of_positions_exn $loc
        }
      }]
    }

sized_basic_type:
  | INT
    { grammar_logger "INT_var_type" ; (SizedType.SInt, Identity) }
  | REAL
    { grammar_logger "REAL_var_type" ; (SizedType.SReal, Identity) }
  | COMPLEX
    { grammar_logger "COMPLEX_var_type" ; (SizedType.SComplex, Identity) }
  | VECTOR LBRACK e=expression RBRACK
    { grammar_logger "VECTOR_var_type" ; (SizedType.SVector (Common.Helpers.AoS, e), Identity) }
  | ROWVECTOR LBRACK e=expression RBRACK
    { grammar_logger "ROWVECTOR_var_type" ; (SizedType.SRowVector (AoS, e) , Identity) }
  | MATRIX LBRACK e1=expression COMMA e2=expression RBRACK
    { grammar_logger "MATRIX_var_type" ; (SizedType.SMatrix (AoS, e1, e2), Identity) }

top_var_type:
  | INT r=range_constraint
    { grammar_logger "INT_top_var_type" ; (SInt, r) }
  | REAL c=type_constraint
    { grammar_logger "REAL_top_var_type" ; (SReal, c) }
  | COMPLEX c=type_constraint
    { grammar_logger "COMPLEX_var_type" ; (SComplex, c) }
  | VECTOR c=type_constraint LBRACK e=expression RBRACK
    { grammar_logger "VECTOR_top_var_type" ; (SVector (AoS, e), c) }
  | ROWVECTOR c=type_constraint LBRACK e=expression RBRACK
    { grammar_logger "ROWVECTOR_top_var_type" ; (SRowVector (AoS, e), c) }
  | MATRIX c=type_constraint LBRACK e1=expression COMMA e2=expression RBRACK
    { grammar_logger "MATRIX_top_var_type" ; (SMatrix (AoS, e1, e2), c) }
  | ORDERED LBRACK e=expression RBRACK
    { grammar_logger "ORDERED_top_var_type" ; (SVector (AoS, e), Ordered) }
  | POSITIVEORDERED LBRACK e=expression RBRACK
    {
      grammar_logger "POSITIVEORDERED_top_var_type" ;
      (SVector (AoS, e), PositiveOrdered)
    }
  | SIMPLEX LBRACK e=expression RBRACK
    { grammar_logger "SIMPLEX_top_var_type" ; (SVector (AoS, e), Simplex) }
  | UNITVECTOR LBRACK e=expression RBRACK
    { grammar_logger "UNITVECTOR_top_var_type" ; (SVector (AoS, e), UnitVector) }
  | CHOLESKYFACTORCORR LBRACK e=expression RBRACK
    {
      grammar_logger "CHOLESKYFACTORCORR_top_var_type" ;
      (SMatrix (AoS, e, e), CholeskyCorr)
    }
  | CHOLESKYFACTORCOV LBRACK e1=expression oe2=option(pair(COMMA, expression))
    RBRACK
    {
      grammar_logger "CHOLESKYFACTORCOV_top_var_type" ;
      match oe2 with Some (_,e2) -> ( SMatrix (AoS, e1, e2), CholeskyCov)
                   | _           ->  (SMatrix (AoS, e1, e1),  CholeskyCov)
    }
  | CORRMATRIX LBRACK e=expression RBRACK
    { grammar_logger "CORRMATRIX_top_var_type" ; (SMatrix (AoS, e, e), Correlation) }
  | COVMATRIX LBRACK e=expression RBRACK
    { grammar_logger "COVMATRIX_top_var_type" ; (SMatrix (AoS, e, e), Covariance) }

type_constraint:
  | r=range_constraint
    {  grammar_logger "type_constraint_range" ; r }
  | LABRACK l=offset_mult RABRACK
    {  grammar_logger "type_constraint_offset_mult" ; l }

range_constraint:
  | (* nothing *)
    { grammar_logger "empty_constraint" ; Transformation.Identity }
  | LABRACK r=range RABRACK
    {  grammar_logger "range_constraint" ; r }

range:
  | LOWER ASSIGN e1=constr_expression COMMA UPPER ASSIGN e2=constr_expression
  | UPPER ASSIGN e2=constr_expression COMMA LOWER ASSIGN e1=constr_expression
    { grammar_logger "lower_upper_range" ; Transformation.LowerUpper (e1, e2) }
  | LOWER ASSIGN e=constr_expression
    {  grammar_logger "lower_range" ; Lower e }
  | UPPER ASSIGN e=constr_expression
    { grammar_logger "upper_range" ; Upper e }

offset_mult:
  | OFFSET ASSIGN e1=constr_expression COMMA MULTIPLIER ASSIGN e2=constr_expression
  | MULTIPLIER ASSIGN e2=constr_expression COMMA OFFSET ASSIGN e1=constr_expression
    { grammar_logger "offset_mult" ; Transformation.OffsetMultiplier (e1, e2) }
  | OFFSET ASSIGN e=constr_expression
    { grammar_logger "offset" ; Offset e }
  | MULTIPLIER ASSIGN e=constr_expression
    { grammar_logger "multiplier" ; Multiplier e }

arr_dims:
  | ARRAY LBRACK l=separated_nonempty_list(COMMA, expression) RBRACK
    { grammar_logger "array dims" ; l  }

dims:
  | LBRACK l=separated_nonempty_list(COMMA, expression) RBRACK
    { grammar_logger "dims" ; l  }

(* expressions *)
%inline expression:
  | l=lhs
    {
      grammar_logger "lhs_expression" ;
      l
    }
  | e=non_lhs
    { grammar_logger "non_lhs_expression" ;
      {expr=e;
       emeta={loc= Location_span.of_positions_exn $loc}}}

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
                 emeta={loc= Location_span.of_positions_exn $loc(ue)}}, i)}
  | e=common_expression
    { grammar_logger "common_expr" ; e }

(* TODO: why do we not simply disallow greater than in constraints? No need to disallow all logical operations, right? *)
constr_expression:
  | e1=constr_expression op=arithmeticBinOp e2=constr_expression
    {
      grammar_logger "constr_expression_arithmetic" ;
      {expr=BinOp (e1, op, e2);
       emeta={loc=Location_span.of_positions_exn $loc}
      }
    }
  | op=prefixOp e=constr_expression %prec unary_over_binary
    {
      grammar_logger "constr_expression_prefixOp" ;
      {expr=PrefixOp (op, e);
       emeta={loc=Location_span.of_positions_exn $loc}}
    }
  | e=constr_expression op=postfixOp
    {
      grammar_logger "constr_expression_postfix" ;
      {expr=PostfixOp (e, op);
       emeta={loc=Location_span.of_positions_exn $loc}}
    }
  | e=constr_expression LBRACK i=indexes RBRACK
    {
      grammar_logger "constr_expression_indexed" ;
      {expr=Indexed (e, i);
       emeta={loc=Location_span.of_positions_exn $loc}}
    }
  | e=common_expression
    {
      grammar_logger "constr_expression_common_expr" ;
      {expr=e;
       emeta={loc= Location_span.of_positions_exn $loc}}
    }
  | id=identifier
    {
      grammar_logger "constr_expression_identifier" ;
      {expr=Variable id;
       emeta={loc=Location_span.of_positions_exn $loc}}
    }

common_expression:
  | i=INTNUMERAL
    {  grammar_logger ("intnumeral " ^ i) ; IntNumeral i }
  | r=REALNUMERAL
    {  grammar_logger ("realnumeral " ^ r) ; RealNumeral r }
  | z=IMAGNUMERAL
    {  grammar_logger ("imagnumeral " ^ z) ; ImagNumeral (String.drop_suffix z 1) }
  | LBRACE xs=separated_nonempty_list(COMMA, expression) RBRACE
    {  grammar_logger "array_expression" ; ArrayExpr xs  }
  | LBRACK xs=separated_list(COMMA, expression) RBRACK
    {  grammar_logger "row_vector_expression" ; RowVectorExpr xs }
  | id=identifier LPAREN args=separated_list(COMMA, expression) RPAREN
    {  grammar_logger "fun_app" ;
       if
         List.length args = 1
         && List.exists ~f:(fun x -> String.is_suffix ~suffix:x id.name) Utils.conditioning_suffices
       then CondDistApp ((), id, args)
       else FunApp ((), id, args) }
  | TARGET LPAREN RPAREN
    { grammar_logger "target_read" ; GetTarget }
  | GETLP LPAREN RPAREN
    { grammar_logger "get_lp" ; GetLP } (* deprecated *)
  | id=identifier LPAREN e=expression BAR args=separated_list(COMMA, expression)
    RPAREN
    {  grammar_logger "conditional_dist_app" ; CondDistApp ((), id, e :: args) }
  | LPAREN e=expression RPAREN
    { grammar_logger "extra_paren" ; Paren e }

%inline prefixOp:
  | BANG
    {   grammar_logger "prefix_bang" ; Operator.PNot }
  | MINUS
    {  grammar_logger "prefix_minus" ; Operator.PMinus }
  | PLUS
    {   grammar_logger "prefix_plus" ; Operator.PPlus }

%inline postfixOp:
  | TRANSPOSE
    {  grammar_logger "postfix_transpose" ; Operator.Transpose }

%inline infixOp:
  | a=arithmeticBinOp
    {   grammar_logger "infix_arithmetic" ; a }
  | l=logicalBinOp
    {  grammar_logger "infix_logical" ; l }

%inline arithmeticBinOp:
  | PLUS
    {  grammar_logger "infix_plus" ; Operator.Plus }
  | MINUS
    {   grammar_logger "infix_minus" ;Operator.Minus }
  | TIMES
    {  grammar_logger "infix_times" ; Operator.Times }
  | DIVIDE
    {  grammar_logger "infix_divide" ; Operator.Divide }
  | IDIVIDE
    {  grammar_logger "infix_intdivide" ; Operator.IntDivide }
  | MODULO
    {  grammar_logger "infix_modulo" ; Operator.Modulo }
  | LDIVIDE
    {  grammar_logger "infix_ldivide" ; Operator.LDivide }
  | ELTTIMES
    {  grammar_logger "infix_elttimes" ; Operator.EltTimes }
  | ELTDIVIDE
    {   grammar_logger "infix_eltdivide" ; Operator.EltDivide }
  | HAT
    {  grammar_logger "infix_hat" ; Operator.Pow }
  | ELTPOW
    {  grammar_logger "infix_eltpow" ; Operator.EltPow }

%inline logicalBinOp:
  | OR
    {   grammar_logger "infix_or" ; Operator.Or }
  | AND
    {   grammar_logger "infix_and" ; Operator.And }
  | EQUALS
    {   grammar_logger "infix_equals" ; Operator.Equals }
  | NEQUALS
    {   grammar_logger "infix_nequals" ; Operator.NEquals}
  | LABRACK
    {   grammar_logger "infix_less" ; Operator.Less }
  | LEQ
    {   grammar_logger "infix_leq" ; Operator.Leq }
  | RABRACK
    {   grammar_logger "infix_greater" ; Operator.Greater }
  | GEQ
    {   grammar_logger "infix_geq" ; Operator.Geq }


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
    {  grammar_logger "lhs_identifier" ;
       {expr=Variable id
       ;emeta = {loc=id.id_loc}}
    }
  | l=lhs LBRACK indices=indexes RBRACK
    {  grammar_logger "lhs_index" ;
      {expr=Indexed (l, indices)
      ;emeta = { loc=Location_span.of_positions_exn $loc}}}

(* statements *)
statement:
  | s=atomic_statement
    {  grammar_logger "atomic_statement" ;
       {stmt= s;
        smeta= { loc=Location_span.of_positions_exn $sloc} }
    }
  | s=nested_statement
    {  grammar_logger "nested_statement" ;
       {stmt= s;
        smeta={loc = Location_span.of_positions_exn $sloc} }
    }

atomic_statement:
  | l=lhs op=assignment_op e=expression SEMICOLON
    {  grammar_logger "assignment_statement" ;
       Assignment {assign_lhs=lvalue_of_expr l;
                   assign_op=op;
                   assign_rhs=e} }
  | id=identifier LPAREN args=separated_list(COMMA, expression) RPAREN SEMICOLON
    {  grammar_logger "funapp_statement" ; NRFunApp ((),id, args)  }
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
  | PROFILE LPAREN st=string_literal RPAREN LBRACE l=list(vardecl_or_statement) RBRACE
    {  grammar_logger "profile_statement" ; Profile (st, List.concat l) }
  | LBRACE l=list(vardecl_or_statement)  RBRACE
    {  grammar_logger "block_statement" ; Block (List.concat l) } (* NOTE: I am choosing to allow mixing of statements and var_decls *)

(* statement or var decls *)
vardecl_or_statement:
  | s=statement
    { grammar_logger "vardecl_or_statement_statement" ; [s] }
  | v=var_decl
    { grammar_logger "vardecl_or_statement_vardecl" ; v }

top_vardecl_or_statement:
  | s=statement
    { grammar_logger "top_vardecl_or_statement_statement" ; [s] }
  | v=top_var_decl
    { grammar_logger "top_vardecl_or_statement_top_vardecl" ; v }
