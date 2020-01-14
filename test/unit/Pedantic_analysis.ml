open Core_kernel
open Frontend
open Analysis_and_optimization.Dependence_analysis
open Analysis_and_optimization.Pedantic_analysis
open Middle
open Analysis_and_optimization.Dataflow_types

let semantic_check_program ast =
  Option.value_exn
    (Result.ok
       (Semantic_check.semantic_check_program
          (Option.value_exn (Result.ok ast))))

let sigma_example =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        parameters {
          real sigma;
          real sigma_a;
          real<lower=0> sigma_b;
          real<lower=0, upper=1> sigma_c;
          real<lower=1> sigma_d;
          real<lower=1, upper=2> sigma_e;
          real logsigma_f;
        }
        model {}
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Unbounded sigma warning" =
  print_warn_pedantic sigma_example ;
  [%expect
    {|
      Warning: Your Stan program has an unconstrained parameter "sigma" whose name begins with "sigma". Parameters with this name are typically scale parameters and constrained to be positive. If this parameter is indeed a scale (or standard deviation or variance) parameter, add lower=0 to its declaration.
      Warning: Your Stan program has an unconstrained parameter "sigma_a" whose name begins with "sigma". Parameters with this name are typically scale parameters and constrained to be positive. If this parameter is indeed a scale (or standard deviation or variance) parameter, add lower=0 to its declaration.
      Warning: Your Stan program has an unconstrained parameter "sigma_d" whose name begins with "sigma". Parameters with this name are typically scale parameters and constrained to be positive. If this parameter is indeed a scale (or standard deviation or variance) parameter, add lower=0 to its declaration.
      Warning: Your Stan program has an unconstrained parameter "sigma_e" whose name begins with "sigma". Parameters with this name are typically scale parameters and constrained to be positive. If this parameter is indeed a scale (or standard deviation or variance) parameter, add lower=0 to its declaration.
      Warning: Your Stan program has a parameter "sigma_e" with hard constraints in its declaration. Hard constraints are not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a hard constraint can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of constraining an elasticity parameter to fall between 0, and 1, leave it unconstrained and give it a normal(0.5,0.5) prior distribution.
      Warning: The parameter logsigma_f was defined but never used.
      Warning: The parameter sigma was defined but never used.
      Warning: The parameter sigma_a was defined but never used.
    |}]

let uniform_example =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        parameters {
          real x;
        }
        model {
          x ~ uniform(0, 1);
          1 ~ uniform(0, x);
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Uniform warning" =
  print_warn_pedantic uniform_example ;
  [%expect
    {|
      Warning: At 'string', line 6, column 10 to column 28, your Stan program has a uniform distribution on variable x. The uniform distribution is not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a uniform density can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of giving an elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5).
    |}]

let unscaled_example =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        functions {
          real f() {
            return 1000;
          }
        }
        parameters {
          real x;
        }
        model {
          x ~ normal(0.001, 10000);
          real z;
          z = -1000 + 0.00001;
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Unscaled warning" =
  print_warn_pedantic unscaled_example ;
  [%expect
    {|
      Warning: At 'string', line 4, column 19 to column 23, you have the constant 1000 which is less than 0.1 or more than 10 in absolute value. This suggests that you might have parameters in your model that have not been scaled to roughly order 1. We suggest rescaling using a multiplier; see section *** of the manual for an example.
      Warning: At 'string', line 11, column 21 to column 26, you have the constant 0.001 which is less than 0.1 or more than 10 in absolute value. This suggests that you might have parameters in your model that have not been scaled to roughly order 1. We suggest rescaling using a multiplier; see section *** of the manual for an example.
      Warning: At 'string', line 11, column 28 to column 33, you have the constant 10000 which is less than 0.1 or more than 10 in absolute value. This suggests that you might have parameters in your model that have not been scaled to roughly order 1. We suggest rescaling using a multiplier; see section *** of the manual for an example.
      Warning: At 'string', line 13, column 15 to column 19, you have the constant 1000 which is less than 0.1 or more than 10 in absolute value. This suggests that you might have parameters in your model that have not been scaled to roughly order 1. We suggest rescaling using a multiplier; see section *** of the manual for an example.
      Warning: At 'string', line 13, column 22 to column 29, you have the constant 0.00001 which is less than 0.1 or more than 10 in absolute value. This suggests that you might have parameters in your model that have not been scaled to roughly order 1. We suggest rescaling using a multiplier; see section *** of the manual for an example. |}]

let multi_twiddle_example =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        parameters {
          real x;
          real y;
        }
        model {
          x ~ normal(0, 1);
          y ~ normal(1, 1);
          x ~ normal(y, 1);
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Multi twiddle warning" =
  print_warn_pedantic multi_twiddle_example ;
  [%expect
    {|
      Warning: The parameter x is on the left-hand side of more than one twiddle statement. |}]

let hard_constrained_example =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        parameters {
          real<lower=0, upper=1> a;
          real<lower=-1, upper=1> b;
          real<lower=0, upper=10> c;
          real<lower=-1, upper=0> d;
        }
        model {
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Hard constraint warning" =
  print_warn_pedantic hard_constrained_example ;
  [%expect
    {|
      Warning: Your Stan program has a parameter "c" with hard constraints in its declaration. Hard constraints are not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a hard constraint can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of constraining an elasticity parameter to fall between 0, and 1, leave it unconstrained and give it a normal(0.5,0.5) prior distribution.
      Warning: Your Stan program has a parameter "d" with hard constraints in its declaration. Hard constraints are not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a hard constraint can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of constraining an elasticity parameter to fall between 0, and 1, leave it unconstrained and give it a normal(0.5,0.5) prior distribution. |}]

let unused_param_example =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        parameters {
          real a;
          real b;
          real c;
          real d;
          real e;
        }
        transformed parameters {
          real f = c;
        }
        model {
          b ~ normal(1, 1);
          a ~ normal(b, 1);
        }
        generated quantities {
          real g = d;
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Unused param warning" =
  print_warn_pedantic unused_param_example ;
  [%expect
    {|
      Warning: The parameter e was defined but never used.
      Warning: The parameter f was defined but never used. |}]

let param_dependant_cf_example =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        parameters {
          real a;
          real b;
        }
        model {
          int x;
          int y = 0;
          if(a > 0) {
            x = 0;
          } else {
            x = 1;
          }
          for(i in 0:x) {
            y = y + 1;
          }
          while ( y > 0 ) {
            b ~ normal(0, 1);
          }
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Parameter dependent control flow warning" =
  print_warn_pedantic param_dependant_cf_example ;
  [%expect
    {|
      Warning: The control flow statement depends on parameter(s): a.
      Warning: The control flow statement depends on parameter(s): a.
      Warning: The control flow statement depends on parameter(s): a. |}]
