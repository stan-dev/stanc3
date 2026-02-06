Flags not used elsewhere in the tests

  $ stanc basic.stan --debug-lex
  Lexer: data
  Lexer: space
  Lexer: {
  Lexer: newline
  {fname=basic.stan; line=1}
  Lexer: space
  Lexer: space
  Lexer: int
  Lexer: <
  Lexer: lower
  Lexer: =
  Lexer: int_constant 0
  Lexer: >
  Lexer: space
  Lexer: identifier N
  {fname=basic.stan; line=2}
  Lexer: ;
  Lexer: newline
  {fname=basic.stan; line=2}
  Lexer: space
  Lexer: space
  Lexer: array
  Lexer: [
  Lexer: identifier N
  {fname=basic.stan; line=3}
  Lexer: ]
  Lexer: space
  Lexer: int
  Lexer: <
  Lexer: lower
  Lexer: =
  Lexer: int_constant 0
  Lexer: ,
  Lexer: space
  Lexer: upper
  Lexer: =
  Lexer: int_constant 1
  Lexer: >
  Lexer: space
  Lexer: identifier y
  {fname=basic.stan; line=3}
  Lexer: ;
  Lexer: newline
  {fname=basic.stan; line=3}
  Lexer: }
  Lexer: newline
  {fname=basic.stan; line=4}
  Lexer: parameters
  Lexer: space
  Lexer: {
  Lexer: newline
  {fname=basic.stan; line=5}
  Lexer: space
  Lexer: space
  Lexer: real
  Lexer: <
  Lexer: lower
  Lexer: =
  Lexer: int_constant 0
  Lexer: ,
  Lexer: space
  Lexer: upper
  Lexer: =
  Lexer: int_constant 1
  Lexer: >
  Lexer: space
  Lexer: identifier theta
  {fname=basic.stan; line=6}
  Lexer: ;
  Lexer: newline
  {fname=basic.stan; line=6}
  Lexer: }
  Lexer: newline
  {fname=basic.stan; line=7}
  Lexer: model
  Lexer: space
  Lexer: {
  Lexer: newline
  {fname=basic.stan; line=8}
  Lexer: space
  Lexer: space
  Lexer: identifier theta
  {fname=basic.stan; line=9}
  Lexer: space
  Lexer: ~
  Lexer: space
  Lexer: identifier beta
  {fname=basic.stan; line=9}
  Lexer: (
  Lexer: int_constant 1
  Lexer: ,
  Lexer: space
  Lexer: int_constant 1
  Lexer: )
  Lexer: ;
  Lexer: space
  Lexer: single comment
  {fname=basic.stan; line=9}
  Lexer: space
  Lexer: space
  Lexer: identifier y
  {fname=basic.stan; line=10}
  Lexer: space
  Lexer: ~
  Lexer: space
  Lexer: identifier bernoulli
  {fname=basic.stan; line=10}
  Lexer: (
  Lexer: identifier theta
  {fname=basic.stan; line=10}
  Lexer: )
  Lexer: ;
  Lexer: newline
  {fname=basic.stan; line=10}
  Lexer: }
  Lexer: newline
  {fname=basic.stan; line=11}
  Lexer: eof

  $ stanc basic.stan --debug-parse
  Parser: intnumeral 0
  Parser: constr_expression_common_expr
  Parser: lower_range
  Parser: range_constraint
  Parser: INT_top_var_type
  Parser: identifier N
  Parser: higher_type
  Parser: top_var_decl_no_assign
  Parser: identifier N
  Parser: identifier_expr
  Parser: common_expr
  Parser: array dims
  Parser: intnumeral 0
  Parser: constr_expression_common_expr
  Parser: intnumeral 1
  Parser: constr_expression_common_expr
  Parser: lower_upper_range
  Parser: range_constraint
  Parser: INT_top_var_type
  Parser: array_type
  Parser: identifier y
  Parser: higher_type
  Parser: top_var_decl_no_assign
  Parser: data_block
  Parser: intnumeral 0
  Parser: constr_expression_common_expr
  Parser: intnumeral 1
  Parser: constr_expression_common_expr
  Parser: lower_upper_range
  Parser: range_constraint
  Parser: type_constraint_range
  Parser: REAL_top_var_type
  Parser: identifier theta
  Parser: higher_type
  Parser: top_var_decl_no_assign
  Parser: parameters_block
  Parser: identifier theta
  Parser: identifier_expr
  Parser: common_expr
  Parser: identifier beta
  Parser: intnumeral 1
  Parser: common_expr
  Parser: intnumeral 1
  Parser: common_expr
  Parser: tilde_statement
  Parser: atomic_statement
  Parser: vardecl_or_statement_statement
  Parser: identifier y
  Parser: identifier_expr
  Parser: common_expr
  Parser: identifier bernoulli
  Parser: identifier theta
  Parser: identifier_expr
  Parser: common_expr
  Parser: tilde_statement
  Parser: atomic_statement
  Parser: vardecl_or_statement_statement
  Parser: model_block
  Parser: program

  $ stanc basic.stan --debug-ast
  ((functionblock ())
   (datablock
    (((stmts
       (((stmt
          (VarDecl (decl_type SInt)
           (transformation (Lower ((expr (IntNumeral 0)) (emeta ((loc <opaque>))))))
           (is_global true)
           (variables (((identifier ((name N) (id_loc <opaque>))) (initial_value ()))))))
         (smeta ((loc <opaque>))))
        ((stmt
          (VarDecl
           (decl_type
            (SArray SInt
             ((expr (Variable ((name N) (id_loc <opaque>)))) (emeta ((loc <opaque>))))))
           (transformation
            (LowerUpper ((expr (IntNumeral 0)) (emeta ((loc <opaque>))))
             ((expr (IntNumeral 1)) (emeta ((loc <opaque>))))))
           (is_global true)
           (variables (((identifier ((name y) (id_loc <opaque>))) (initial_value ()))))))
         (smeta ((loc <opaque>))))))
      (xloc <opaque>))))
   (transformeddatablock ())
   (parametersblock
    (((stmts
       (((stmt
          (VarDecl (decl_type SReal)
           (transformation
            (LowerUpper ((expr (IntNumeral 0)) (emeta ((loc <opaque>))))
             ((expr (IntNumeral 1)) (emeta ((loc <opaque>))))))
           (is_global true)
           (variables (((identifier ((name theta) (id_loc <opaque>))) (initial_value ()))))))
         (smeta ((loc <opaque>))))))
      (xloc <opaque>))))
   (transformedparametersblock ())
   (modelblock
    (((stmts
       (((stmt
          (Tilde
           (arg
            ((expr (Variable ((name theta) (id_loc <opaque>)))) (emeta ((loc <opaque>)))))
           (distribution ((name beta) (id_loc <opaque>))) (kind ())
           (args
            (((expr (IntNumeral 1)) (emeta ((loc <opaque>))))
             ((expr (IntNumeral 1)) (emeta ((loc <opaque>))))))
           (truncation NoTruncate)))
         (smeta ((loc <opaque>))))
        ((stmt
          (Tilde
           (arg ((expr (Variable ((name y) (id_loc <opaque>)))) (emeta ((loc <opaque>)))))
           (distribution ((name bernoulli) (id_loc <opaque>))) (kind ())
           (args
            (((expr (Variable ((name theta) (id_loc <opaque>)))) (emeta ((loc <opaque>))))))
           (truncation NoTruncate)))
         (smeta ((loc <opaque>))))))
      (xloc <opaque>))))
   (generatedquantitiesblock ()) (comments <opaque>))
  $ stanc basic.stan --debug-decorated-ast
  ((functionblock ())
   (datablock
    (((stmts
       (((stmt
          (VarDecl (decl_type SInt)
           (transformation
            (Lower
             ((expr (IntNumeral 0))
              (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UInt))))))
           (is_global true)
           (variables (((identifier ((name N) (id_loc <opaque>))) (initial_value ()))))))
         (smeta ((loc <opaque>) (return_type Incomplete))))
        ((stmt
          (VarDecl
           (decl_type
            (SArray SInt
             ((expr (Variable ((name N) (id_loc <opaque>))))
              (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UInt))))))
           (transformation
            (LowerUpper
             ((expr (IntNumeral 0))
              (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UInt))))
             ((expr (IntNumeral 1))
              (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UInt))))))
           (is_global true)
           (variables (((identifier ((name y) (id_loc <opaque>))) (initial_value ()))))))
         (smeta ((loc <opaque>) (return_type Incomplete))))))
      (xloc <opaque>))))
   (transformeddatablock ())
   (parametersblock
    (((stmts
       (((stmt
          (VarDecl (decl_type SReal)
           (transformation
            (LowerUpper
             ((expr (IntNumeral 0))
              (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UInt))))
             ((expr (IntNumeral 1))
              (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UInt))))))
           (is_global true)
           (variables (((identifier ((name theta) (id_loc <opaque>))) (initial_value ()))))))
         (smeta ((loc <opaque>) (return_type Incomplete))))))
      (xloc <opaque>))))
   (transformedparametersblock ())
   (modelblock
    (((stmts
       (((stmt
          (Tilde
           (arg
            ((expr (Variable ((name theta) (id_loc <opaque>))))
             (emeta ((loc <opaque>) (ad_level AutoDiffable) (type_ UReal)))))
           (distribution ((name beta) (id_loc <opaque>))) (kind (StanLib (FnLpdf true)))
           (args
            (((expr
               (Promotion
                ((expr (IntNumeral 1))
                 (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UInt))))
                (UReal DataOnly)))
              (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UReal))))
             ((expr
               (Promotion
                ((expr (IntNumeral 1))
                 (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UInt))))
                (UReal DataOnly)))
              (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UReal))))))
           (truncation NoTruncate)))
         (smeta ((loc <opaque>) (return_type Incomplete))))
        ((stmt
          (Tilde
           (arg
            ((expr (Variable ((name y) (id_loc <opaque>))))
             (emeta ((loc <opaque>) (ad_level DataOnly) (type_ (UArray UInt))))))
           (distribution ((name bernoulli) (id_loc <opaque>)))
           (kind (StanLib (FnLpmf true)))
           (args
            (((expr (Variable ((name theta) (id_loc <opaque>))))
              (emeta ((loc <opaque>) (ad_level AutoDiffable) (type_ UReal))))))
           (truncation NoTruncate)))
         (smeta ((loc <opaque>) (return_type Incomplete))))))
      (xloc <opaque>))))
   (generatedquantitiesblock ()) (comments <opaque>))


  $ stanc basic.stan --debug-mir-pretty
  input_vars {
    int N;
    array[int, N] y; }
  
  prepare_data {
    data int N;
    (FnCheck(trans(Lower 0))(var_name N)(var N))__(0);
    FnValidateSize__("y", "N", N);
    data array[int, N] y;
    (FnCheck(trans(Lower 0))(var_name y)(var y))__(0);
    (FnCheck(trans(Upper 1))(var_name y)(var y))__(1); }
  
  log_prob {
    real theta;
    {
      target += beta_lupdf(theta, promote(1, real, data),
                           promote(1, real, data));
      target += bernoulli_lupmf(y, theta);
    } }
  
  generate_quantities {
    data real theta;
    if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
    if(PNot__(emit_generated_quantities__)) return; }
  output_vars {
    parameters real theta; //real
  }

  $ stanc basic.stan --debug-transformed-mir-pretty
  input_vars {
    int N;
    array[int, N] y; }
  
  prepare_data {
    data int N;
    N = FnReadData__("N")[1];
    (FnCheck(trans(Lower 0))(var_name N)(var N))__(0);
    FnValidateSize__("y", "N", N);
    data array[int, N] y;
    y = FnReadData__("y");
    (FnCheck(trans(Lower 0))(var_name y)(var y))__(0);
    (FnCheck(trans(Upper 1))(var_name y)(var y))__(1); }
  
  log_prob {
    real
      theta = (FnReadParam(constrain(LowerUpper 0 1))(dims())(mem_pattern AoS))__(
      );
    {
      target += beta_lupdf(theta, promote(1, real, data),
                           promote(1, real, data));
      target += bernoulli_lupmf(y, theta);
    } }
  
  rev_log_prob {
    real
      theta = (FnReadParam(constrain(LowerUpper 0 1))(dims())(mem_pattern AoS))__(
      );
    {
      target += beta_lupdf(theta, promote(1, real, data),
                           promote(1, real, data));
      target += bernoulli_lupmf(y, theta);
    } }
  
  generate_quantities {
    data real
      theta = (FnReadParam(constrain(LowerUpper 0 1))(dims())(mem_pattern AoS))__(
      );
    (FnWriteParam(unconstrain_opt())(var theta))__();
    if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
      return;
    }
    if(PNot__(emit_generated_quantities__)) {
      return;
    } }
  
  transform_inits {
    real theta;
    theta = FnReadData__("theta")[1];
    (FnWriteParam(unconstrain_opt((LowerUpper 0 1)))(var theta))__(); }
  output_vars {
    parameters real theta; //real
  }

  $ stanc basic.stan --debug-optimized-mir-pretty
  input_vars {
    int N;
    array[int, N] y; }
  
  prepare_data {
    data int N;
    N = FnReadData__("N")[1];
    (FnCheck(trans(Lower 0))(var_name N)(var N))__(0);
    FnValidateSize__("y", "N", N);
    data array[int, N] y;
    y = FnReadData__("y");
    (FnCheck(trans(Lower 0))(var_name y)(var y))__(0);
    (FnCheck(trans(Upper 1))(var_name y)(var y))__(1); }
  
  log_prob {
    real
      theta = (FnReadParam(constrain(LowerUpper 0 1))(dims())(mem_pattern AoS))__(
      );
    {
      target += beta_lupdf(theta, promote(1, real, data),
                           promote(1, real, data));
      target += bernoulli_lupmf(y, theta);
    } }
  
  rev_log_prob {
    real
      theta = (FnReadParam(constrain(LowerUpper 0 1))(dims())(mem_pattern AoS))__(
      );
    {
      target += beta_lupdf(theta, promote(1, real, data),
                           promote(1, real, data));
      target += bernoulli_lupmf(y, theta);
    } }
  
  generate_quantities {
    data real
      theta = (FnReadParam(constrain(LowerUpper 0 1))(dims())(mem_pattern AoS))__(
      );
    (FnWriteParam(unconstrain_opt())(var theta))__();
    if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
      return;
    }
    if(PNot__(emit_generated_quantities__)) {
      return;
    } }
  
  transform_inits {
    real theta;
    theta = FnReadData__("theta")[1];
    (FnWriteParam(unconstrain_opt((LowerUpper 0 1)))(var theta))__(); }
  output_vars {
    parameters real theta; //real
  }

  $ stanc parse_error.stan --debug-parse
  Syntax error in 'parse_error.stan', line 1, column 0 to column 5, parsing error:
     -------------------------------------------------
       1:  datta {}
           ^
     -------------------------------------------------
  
  Ill-formed program. Expected "functions {", "transformed data {", "parameters {",
  "transformed parameters {", "model {", or "generated quantities {".
  (Parse error state 436)
  [1]
