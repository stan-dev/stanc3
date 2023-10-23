(** The parser for Stan. A Menhir file. *)

%{
open Core
open Middle
open Ast
open Debugging
open Preprocessor

(* Takes a sized_basic_type and a list of sizes and repeatedly applies then
   SArray constructor, taking sizes off the list *)
let reducearray (sbt, l) =
  List.fold_right l ~f:(fun z y -> SizedType.SArray (y, z)) ~init:sbt

let build_id id loc =
  grammar_logger ("identifier " ^ id) ;
  {name= id; id_loc= location_span_of_positions loc}

let reserved (name, loc, _) =
  raise
    (Errors.SyntaxError
       (Errors.Parsing
          ( "Expected a new identifier but found reserved keyword '" ^ name
            ^ "'.\n"
          , location_span_of_positions loc ) ) )

let reserved_decl (name, loc, is_type) =
  if is_type then
    raise
      (Errors.SyntaxError
         (Errors.Parsing
            ( "Found a type ('" ^ name
              ^ "') where an identifier was expected.\n\
                 All variables declared in a comma-separated list must be of \
                 the same type.\n"
            , location_span_of_positions loc ) ) )
  else reserved (name, loc, is_type)

let build_expr expr loc = {expr; emeta= {loc= location_span_of_positions loc}}
let rec iterate_n f x = function 0 -> x | n -> iterate_n f (f x) (n - 1)

let parse_tuple_slot ix_str loc =
  match int_of_string_opt (String.drop_prefix ix_str 1) with
  | None ->
      raise
        (Errors.SyntaxError
           (Errors.Parsing
              ( "Failed to parse integer from string '" ^ ix_str
                ^ "' in tuple index. \nThe index is likely too large.\n"
              , location_span_of_positions loc ) ) )
  | Some ix -> ix

(** Given a parsed expression, try to convert it to
    a valid assignable lvalue. Raises a syntax error on failure *)
let try_convert_to_lvalue expr loc =
  match Ast.lvalue_of_expr_opt expr with
  | Some l -> l
  | None ->
    raise
        (Errors.SyntaxError
           (Errors.Parsing
              ( "Expected an assignable value but found a general expression.\n"
              , location_span_of_positions loc ) ) )

let nest_unsized_array basic_type n =
  iterate_n (fun t -> UnsizedType.UArray t) basic_type n

let note_deprecated_array ?(unsized = false) (pos1, pos2) =
  let loc_span = location_span_of_positions (pos1, pos2) in
  Deprecation_removals.old_array_usages :=
    (loc_span, unsized) :: !Deprecation_removals.old_array_usages
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
       ROWVECTOR "row_vector" ARRAY "array" TUPLE "tuple" MATRIX "matrix" ORDERED "ordered"
       COMPLEXVECTOR "complex_vector" COMPLEXROWVECTOR "complex_row_vector"
       POSITIVEORDERED "positive_ordered" SIMPLEX "simplex" UNITVECTOR "unit_vector"
       CHOLESKYFACTORCORR "cholesky_factor_corr" CHOLESKYFACTORCOV "cholesky_factor_cov"
       CORRMATRIX "corr_matrix" COVMATRIX "cov_matrix" COMPLEXMATRIX "complex_matrix"
%token LOWER "lower" UPPER "upper" OFFSET "offset" MULTIPLIER "multiplier"
%token <string> INTNUMERAL "24"
%token <string> REALNUMERAL "3.1415" DOTNUMERAL ".2"
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
            Input_warnings.empty (location_of_position $startpos).filename
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
      { functionblock= Some {stmts= fd; xloc= location_span_of_positions $loc}
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
      {stmts= fd; xloc= location_span_of_positions $loc} }

data_block:
  | DATABLOCK LBRACE tvd=list(top_var_decl_no_assign) RBRACE
    { grammar_logger "data_block" ;
      {stmts= tvd; xloc= location_span_of_positions $loc} }

transformed_data_block:
  | TRANSFORMEDDATABLOCK LBRACE tvds=list(top_vardecl_or_statement) RBRACE
    { grammar_logger "transformed_data_block" ;
      {stmts= tvds; xloc= location_span_of_positions $loc} }

parameters_block:
  | PARAMETERSBLOCK LBRACE tvd=list(top_var_decl_no_assign) RBRACE
    { grammar_logger "parameters_block" ;
      {stmts= tvd; xloc= location_span_of_positions $loc} }

transformed_parameters_block:
  | TRANSFORMEDPARAMETERSBLOCK LBRACE tvds=list(top_vardecl_or_statement) RBRACE
    { grammar_logger "transformed_parameters_block" ;
      {stmts= tvds; xloc= location_span_of_positions $loc} }

model_block:
  | MODELBLOCK LBRACE vds=list(vardecl_or_statement) RBRACE
    { grammar_logger "model_block" ;
      {stmts= vds; xloc= location_span_of_positions $loc} }

generated_quantities_block:
  | GENERATEDQUANTITIESBLOCK LBRACE tvds=list(top_vardecl_or_statement) RBRACE
    { grammar_logger "generated_quantities_block" ;
      {stmts= tvds; xloc= location_span_of_positions $loc} }

(* function definitions *)
identifier:
  | id=IDENTIFIER { build_id id $loc }
  | TRUNCATE { build_id "T" $loc}


decl_identifier:
  | id=identifier { id }
  | err=reserved_word { reserved err }

decl_identifier_after_comma:
  | id=identifier { id }
  | err=reserved_word { reserved_decl err }

reserved_word:
  (* Keywords cannot be identifiers but it is nice to
    let them parse as such to provide a better error *)
  | FUNCTIONBLOCK { "functions", $loc, false }
  | DATABLOCK { "data", $loc, false }
  | PARAMETERSBLOCK { "parameters", $loc, false }
  | MODELBLOCK { "model", $loc, false }
  | RETURN { "return", $loc, false }
  | IF { "if", $loc, false }
  | ELSE { "else", $loc, false }
  | WHILE { "while", $loc, false }
  | FOR { "for", $loc, false }
  | IN { "in", $loc, false }
  | BREAK { "break", $loc, false }
  | CONTINUE { "continue", $loc, false }
  | VOID { "void", $loc, false }
  | INT { "int", $loc, true }
  | REAL { "real", $loc, true }
  | COMPLEX { "complex", $loc, true }
  | VECTOR { "vector", $loc, true }
  | ROWVECTOR { "row_vector", $loc, true }
  | MATRIX { "matrix", $loc, true }
  | COMPLEXVECTOR { "complex_vector", $loc, true }
  | COMPLEXROWVECTOR { "complex_row_vector", $loc, true }
  | COMPLEXMATRIX { "complex_matrix", $loc, true }
  | ORDERED { "ordered", $loc, true }
  | POSITIVEORDERED { "positive_ordered", $loc, true }
  | SIMPLEX { "simplex", $loc, true }
  | UNITVECTOR { "unit_vector", $loc, true }
  | CHOLESKYFACTORCORR { "cholesky_factor_corr", $loc, true }
  | CHOLESKYFACTORCOV { "cholesky_factor_cov", $loc, true }
  | CORRMATRIX { "corr_matrix", $loc, true }
  | COVMATRIX { "cov_matrix", $loc, true  }
  | PRINT { "print", $loc, false }
  | REJECT { "reject", $loc, false }
  | TARGET { "target", $loc, false }
  | GETLP { "get_lp", $loc, false }
  | PROFILE { "profile", $loc, false }
  | TUPLE { "tuple", $loc, true }
  | OFFSET { "offset", $loc, false }
  | MULTIPLIER { "multiplier", $loc, false }
  | LOWER { "lower", $loc, false }
  | UPPER { "upper", $loc, false }
  | ARRAY { "array", $loc, true }

function_def:
  | rt=return_type name=decl_identifier LPAREN args=separated_list(COMMA, arg_decl)
    RPAREN b=statement
    {
      grammar_logger "function_def" ;
      {stmt=FunDef {returntype = rt; funname = name;
                           arguments = args; body=b;};
       smeta={loc=location_span_of_positions $loc}
      }
    }

return_type:
  | VOID
    { grammar_logger "return_type VOID" ; Void }
  | ut=unsized_type
    {  grammar_logger "return_type unsized_type" ; UnsizedType.ReturnType ut }

arg_decl:
  | od=option(DATABLOCK) ut=unsized_type id=decl_identifier
    {  grammar_logger "arg_decl" ;
       match od with None -> (UnsizedType.AutoDiffable, ut, id) | _ -> (DataOnly, ut, id)  }

unsized_type:
  | ARRAY n=unsized_dims t=basic_type
  | ARRAY n=unsized_dims t=unsized_tuple_type
    {  grammar_logger "unsized_type";
        nest_unsized_array t n
    }
  | bt=basic_type n_opt=option(unsized_dims)
    {  grammar_logger "unsized_type";
       if Option.is_some n_opt then
         note_deprecated_array ~unsized:true $loc;
       nest_unsized_array bt (Option.value n_opt ~default:0)
    }
  | t=unsized_tuple_type
    { t }

%inline unsized_tuple_type:
  | TUPLE LPAREN hd=unsized_type COMMA ts=separated_nonempty_list(COMMA, unsized_type) RPAREN
    {  UnsizedType.UTuple (hd::ts)
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
  | COMPLEXVECTOR
    {  grammar_logger "basic_type COMPLEXVECTOR" ; UnsizedType.UComplexVector }
  | COMPLEXROWVECTOR
    {  grammar_logger "basic_type COMPLEXROWVECTOR" ; UnsizedType.UComplexRowVector }
  | COMPLEXMATRIX
    {  grammar_logger "basic_type COMPLEXMATRIX" ; UnsizedType.UComplexMatrix }

unsized_dims:
  | LBRACK cs=list(COMMA) RBRACK
    { grammar_logger "unsized_dims" ; List.length(cs) + 1 }

(* Never accept this rule, but return the same type as expression *)
no_assign:
  | UNREACHABLE
    { (* This code will never be reached *)
       Common.FatalError.fatal_error_msg
          [%message "the UNREACHABLE token should never be produced"]
    }

optional_assignment(rhs):
  | rhs_opt=option(pair(ASSIGN, rhs))
    { Option.map ~f:snd rhs_opt }

id_and_optional_assignment(rhs, decl):
  | identifier=decl initial_value=optional_assignment(rhs)
    { Ast.{identifier; initial_value} }

remaining_declarations(rhs):
  | COMMA decls=separated_nonempty_list(COMMA, id_and_optional_assignment(rhs, decl_identifier_after_comma))
    { decls }

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
    { note_deprecated_array $loc;
      (fun ~is_global ->
      { stmt=
          VarDecl {
              decl_type= (reducearray (fst ty, dims))
            ; transformation= snd ty
            ; variables= [ { identifier= id
                           ; initial_value= rhs_opt
                           } ]
            ; is_global
            }
      ; smeta= {
          loc= location_span_of_positions $loc
        }
    })
    }

  (* This rule matches non-array declarations and also the new array syntax, e.g:
       array[1,2] int x = ..;
   *)
  (* Note that the array dimensions option must be inlined with ioption, else
     it will conflict with first rule. *)
  | ty=higher_type(type_rule)
    (* additional indirection only for better error messaging *)
    v = id_and_optional_assignment(rhs, decl_identifier) vs=option(remaining_declarations(rhs)) SEMICOLON
    { (fun ~is_global ->
      let vs = v :: Option.value vs ~default:[]
      in
      { stmt=
          VarDecl {
              decl_type= fst ty
            ; transformation= snd ty
            ; variables=vs
            ; is_global
            }
      ; smeta= {
          loc= location_span_of_positions $sloc
        }
      })
    }

(* Take a type matched by type_rule and produce that type or any (possibly nested) container of that type *)
(* Can't do the fully recursive array_type(higher_type) because arrays can't hold arrays *)
%inline higher_type(type_rule):
  | ty=array_type(type_rule)
  | ty=tuple_type(type_rule)
  | ty=type_rule
  { grammar_logger "higher_type" ;
    ty
  }

array_type(type_rule):
  | dims=arr_dims ty=type_rule
  | dims=arr_dims ty=tuple_type(type_rule)
  { grammar_logger "array_type" ;
    let (type_, trans) = ty in
    ((reducearray (type_, dims)), trans)
  }

tuple_type(type_rule):
  | TUPLE LPAREN head=higher_type(type_rule) COMMA rest=separated_nonempty_list(COMMA, higher_type(type_rule)) RPAREN
  { grammar_logger "tuple_type" ;
    let ts = head::rest in
    let types, trans = List.unzip ts in
    (SizedType.STuple types, Transformation.TupleTransformation trans)
  }

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
      { stmt= Skip
      ; smeta= { loc= location_span_of_positions $loc }
      }
    }

sized_basic_type:
  | INT
    { grammar_logger "INT_var_type" ; (SizedType.SInt, Identity) }
  | REAL
    { grammar_logger "REAL_var_type" ; (SizedType.SReal, Identity) }
  | COMPLEX
    { grammar_logger "COMPLEX_var_type" ; (SizedType.SComplex, Identity) }
  | VECTOR LBRACK e=expression RBRACK
    { grammar_logger "VECTOR_var_type" ; (SizedType.SVector (Mem_pattern.AoS, e), Identity) }
  | ROWVECTOR LBRACK e=expression RBRACK
    { grammar_logger "ROWVECTOR_var_type" ; (SizedType.SRowVector (AoS, e) , Identity) }
  | MATRIX LBRACK e1=expression COMMA e2=expression RBRACK
    { grammar_logger "MATRIX_var_type" ; (SizedType.SMatrix (AoS, e1, e2), Identity) }
  | COMPLEXVECTOR LBRACK e=expression RBRACK
    { grammar_logger "COMPLEXVECTOR_var_type" ; (SizedType.SComplexVector e, Identity) }
  | COMPLEXROWVECTOR LBRACK e=expression RBRACK
    { grammar_logger "COMPLEXROWVECTOR_var_type" ; (SizedType.SComplexRowVector e , Identity) }
  | COMPLEXMATRIX LBRACK e1=expression COMMA e2=expression RBRACK
    { grammar_logger "COMPLEXMATRIX_var_type" ; (SizedType.SComplexMatrix (e1, e2), Transformation.Identity) }

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
  | COMPLEXVECTOR c=type_constraint LBRACK e=expression RBRACK
    { grammar_logger "COMPLEXVECTOR_top_var_type" ; (SComplexVector e, c) }
  | COMPLEXROWVECTOR c=type_constraint LBRACK e=expression RBRACK
    { grammar_logger "COMPLEXROWVECTOR_top_var_type" ; (SComplexRowVector e, c) }
  | COMPLEXMATRIX c=type_constraint LBRACK e1=expression COMMA e2=expression RBRACK
    { grammar_logger "COMPLEXMATRIX_top_var_type" ; (SComplexMatrix (e1, e2), c) }
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
    { grammar_logger "COVMATRIX_top_var_type" ; (SizedType.SMatrix (AoS, e, e), Transformation.Covariance) }

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

(* Expressions that can be used everywhere except constraint expressions *)
expression:
  | e1=expression  QMARK e2=expression COLON e3=expression
    { grammar_logger "ifthenelse_expr" ; build_expr (TernaryIf (e1, e2, e3)) $loc }
  | e1=expression op=infixOp e2=expression
    { grammar_logger "infix_expr" ; build_expr (BinOp (e1, op, e2)) $loc  }
  | op=prefixOp e=expression %prec unary_over_binary
    { grammar_logger "prefix_expr" ; build_expr (PrefixOp (op, e)) $loc }
  | e=expression op=postfixOp
    { grammar_logger "postfix_expr" ; build_expr (PostfixOp (e, op)) $loc}
  | e=common_expression
    { grammar_logger "common_expr" ; e }

(* Same as the above, but leave out logical binary operators *)
(* TODO: why do we not simply disallow greater than in constraints? No need to disallow all logical operations, right? *)
constr_expression:
  | e1=constr_expression op=arithmeticBinOp e2=constr_expression
    {
      grammar_logger "constr_expression_arithmetic" ;
      build_expr (BinOp (e1, op, e2)) $loc
    }
  | op=prefixOp e=constr_expression %prec unary_over_binary
    {
      grammar_logger "constr_expression_prefixOp" ;
      build_expr (PrefixOp (op, e)) $loc
    }
  | e=constr_expression op=postfixOp
    {
      grammar_logger "constr_expression_postfix" ;
      build_expr (PostfixOp (e, op)) $loc
    }
  | e=common_expression
    { grammar_logger "constr_expression_common_expr" ; e }


common_expression:
  | id=identifier
    { grammar_logger "identifier_expr" ;
      build_expr (Variable id) $loc }
  | i=INTNUMERAL
    { grammar_logger ("intnumeral " ^ i) ;
      build_expr (IntNumeral i) $loc }
  | r=REALNUMERAL
  | r=DOTNUMERAL
    { grammar_logger ("realnumeral " ^ r) ;
      build_expr (RealNumeral r) $loc }
  | z=IMAGNUMERAL
    { grammar_logger ("imagnumeral " ^ z) ;
      build_expr (ImagNumeral (String.drop_suffix z 1)) $loc }
  | LBRACE xs=separated_nonempty_list(COMMA, expression) RBRACE
    { grammar_logger "array_expression" ;
      build_expr (ArrayExpr xs) $loc }
  | LBRACK xs=separated_list(COMMA, expression) RBRACK
    { grammar_logger "row_vector_expression" ;
      build_expr (RowVectorExpr xs) $loc }
  | id=identifier LPAREN args=separated_list(COMMA, expression) RPAREN
    { grammar_logger "fun_app" ;
      let app =
       if
         List.length args = 1
         && List.exists ~f:(fun x -> String.is_suffix ~suffix:x id.name) Utils.conditioning_suffices
       then CondDistApp ((), id, args)
       else FunApp ((), id, args)
       in build_expr app $loc }
  | TARGET LPAREN RPAREN
    { grammar_logger "target_read" ;
      build_expr GetTarget $loc }
  | GETLP LPAREN RPAREN
    { grammar_logger "get_lp" ;
      build_expr GetLP $loc } (* deprecated *)
  | id=identifier LPAREN e=expression BAR args=separated_list(COMMA, expression)
    RPAREN
    { grammar_logger "conditional_dist_app" ;
      build_expr (CondDistApp ((), id, e :: args)) $loc }
  | LPAREN x_head=expression COMMA xs=separated_nonempty_list(COMMA, expression) RPAREN
    { grammar_logger "tuple_expression" ;
      build_expr (TupleExpr (x_head::xs)) $loc  }
  | e=common_expression ix_str=DOTNUMERAL
    { grammar_logger "common_expression_tuple_index" ;
      build_expr (TupleProjection (e, parse_tuple_slot ix_str $loc)) $loc
    }
  | e=common_expression LBRACK indices=indexes RBRACK
    { grammar_logger "common_expression_indexed";
      build_expr (Indexed (e, indices)) $loc }
  | LPAREN e=expression RPAREN
    { grammar_logger "extra_paren" ;
      build_expr (Paren e) $loc }

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

(* statements *)
statement:
  | s=atomic_statement
    {  grammar_logger "atomic_statement" ;
       {stmt= s;
        smeta= { loc=location_span_of_positions $sloc} }
    }
  | s=nested_statement
    {  grammar_logger "nested_statement" ;
       {stmt= s;
        smeta={loc = location_span_of_positions $sloc} }
    }

atomic_statement:
  | l=common_expression op=assignment_op e=expression SEMICOLON
    {  grammar_logger "assignment_statement" ;
       Assignment {assign_lhs=try_convert_to_lvalue l $sloc;
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
  | IF LPAREN e=expression RPAREN s1=vardecl_or_statement ELSE s2=vardecl_or_statement
    {  grammar_logger "ifelse_statement" ; IfThenElse (e, s1, Some s2) }
  | IF LPAREN e=expression RPAREN s=vardecl_or_statement %prec below_ELSE
    {  grammar_logger "if_statement" ; IfThenElse (e, s, None) }
  | WHILE LPAREN e=expression RPAREN s=vardecl_or_statement
    {  grammar_logger "while_statement" ; While (e, s) }
  | FOR LPAREN id=identifier IN e1=expression COLON e2=expression RPAREN
    s=vardecl_or_statement
    {
      grammar_logger "for_statement" ;
      For {loop_variable= id;
           lower_bound= e1;
           upper_bound= e2;
           loop_body= s;}
    }
  | FOR LPAREN id=identifier IN e=expression RPAREN s=vardecl_or_statement
    {  grammar_logger "foreach_statement" ; ForEach (id, e, s) }
  | PROFILE LPAREN st=string_literal RPAREN LBRACE l=list(vardecl_or_statement) RBRACE
    {  grammar_logger "profile_statement" ; Profile (st, l) }
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
    { grammar_logger "top_vardecl_or_statement_statement" ; s }
  | v=top_var_decl
    { grammar_logger "top_vardecl_or_statement_top_vardecl" ; v }
