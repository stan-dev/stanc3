open Core_kernel
open Frontend
open Analysis_and_optimization.Pedantic_analysis

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
          real x;
          real sigma_a;
          real<lower=0> sigma_b;
          real<lower=-1, upper=1> sigma_c;
          real<lower=0, upper=1> sigma_d;
          real<lower=1, upper=2> sigma_e;
        }
        model {
          x ~ normal (0, sigma_a);
          x ~ normal (0, sigma_b);
          x ~ normal (0, sigma_c);
          x ~ normal (0, sigma_d);
          x ~ normal (0, sigma_e);
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Unbounded sigma warning" =
  print_warn_pedantic sigma_example ;
  [%expect
    {|
      Warning: The parameter x is on the left-hand side of more than one twiddle statement.
      Warning: Your Stan program has a parameter "sigma_e" with hard constraints in its declaration. Hard constraints are not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a hard constraint can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of constraining an elasticity parameter to fall between 0, and 1, leave it unconstrained and give it a normal(0.5,0.5) prior distribution.
      Warning: The parameter x has 5 priors.
      Warning: Parameter sigma_a is used as a scale parameter at 'string', line 11, column 10 to column 34, but is not constrained to be positive.
      Warning: Parameter sigma_c is used as a scale parameter at 'string', line 13, column 10 to column 34, but is not constrained to be positive.
    |}]

let uniform_example =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        parameters {
          real a;
          real<lower=0, upper=1> b;
          real<lower=0> c;
          real<upper=0> d;
        }
        model {
          a ~ uniform(0, 1);
          1 ~ uniform(0, a);
          b ~ uniform(0, 1);
          c ~ uniform(0, 1);
          d ~ uniform(0, 1);
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Uniform warning" =
  print_warn_pedantic uniform_example ;
  [%expect
    {|
      Warning: The parameter a has 2 priors.
      Warning: At 'string', line 12, column 10 to column 28, your Stan program has a uniform distribution on variable c. The uniform distribution is not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a uniform density can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of giving an elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5).
      Warning: At 'string', line 13, column 10 to column 28, your Stan program has a uniform distribution on variable d. The uniform distribution is not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a uniform density can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of giving an elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5).
      Warning: At 'string', line 9, column 10 to column 28, your Stan program has a uniform distribution on variable a. The uniform distribution is not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a uniform density can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of giving an elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5).
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
      Warning: At 'string', line 11, column 21 to column 26, you have the distribution argument 0.001 which is less than 0.1 or more than 10 in magnitude. This suggests that you might have parameters in your model that have not been scaled to roughly order 1. We suggest rescaling using a multiplier; see section *** of the manual for an example.
      Warning: At 'string', line 11, column 28 to column 33, you have the distribution argument 10000 which is less than 0.1 or more than 10 in magnitude. This suggests that you might have parameters in your model that have not been scaled to roughly order 1. We suggest rescaling using a multiplier; see section *** of the manual for an example. |}]

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
      Warning: The parameter x is on the left-hand side of more than one twiddle statement.
      Warning: The parameter x has 2 priors.
      Warning: The parameter y has 2 priors. |}]

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
      Warning: Your Stan program has a parameter "d" with hard constraints in its declaration. Hard constraints are not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a hard constraint can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of constraining an elasticity parameter to fall between 0, and 1, leave it unconstrained and give it a normal(0.5,0.5) prior distribution.
      Warning: The parameter a was declared but does not participate in the model.
      Warning: The parameter b was declared but does not participate in the model.
      Warning: The parameter c was declared but does not participate in the model.
      Warning: The parameter d was declared but does not participate in the model. |}]

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
      Warning: The parameter c was declared but does not participate in the model.
      Warning: The parameter d was declared but does not participate in the model.
      Warning: The parameter e was declared but does not participate in the model.
      Warning: The parameter f was declared but does not participate in the model.
      Warning: The parameter b has 2 priors. |}]

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
      Warning: The control flow statement at 'string', line 9, column 10 to line 13, column 11 depends on parameter(s): a.
      Warning: The control flow statement at 'string', line 14, column 10 to line 16, column 11 depends on parameter(s): a.
      Warning: The control flow statement at 'string', line 17, column 10 to line 19, column 11 depends on parameter(s): a. |}]

let non_one_priors_example =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        data {
          real x;
        }
        parameters {
          real a;
          real b;
          real c;
          real d;
        }
        model
        {
          a ~ normal(0, 1);
          b ~ normal(0, 1);
          c ~ normal(a, b);
          d ~ normal(0, 1);
          x ~ normal(c, d);
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Non-one priors no warning" =
  print_warn_pedantic non_one_priors_example ;
  [%expect
    {|
      Warning: Parameter b is used as a scale parameter at 'string', line 15, column 10 to column 27, but is not constrained to be positive.
      Warning: Parameter d is used as a scale parameter at 'string', line 17, column 10 to column 27, but is not constrained to be positive. |}]

let non_one_priors_example2 =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        data {
          real x;
          real y;
        }
        parameters {
          real a;
          real b;
          real c;
          real d;
          real e;
          real f;
        }
        model
        {
          a ~ normal(0, 1);
          b ~ normal(a, 1);
          x ~ normal(b, 1);
          y ~ normal(c, 1);
          d ~ normal(b, 1);
          e ~ normal(a, 1);
          f ~ normal(a, 1);
          f ~ normal(e, 1);
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Non-one priors warning" =
  print_warn_pedantic non_one_priors_example2 ;
  [%expect
    {|
      Warning: The parameter f is on the left-hand side of more than one twiddle statement.
      Warning: The parameter a has 3 priors.
      Warning: The parameter b has 2 priors.
      Warning: The parameter c has 0 priors.
      Warning: The parameter d has 0 priors.
      Warning: The parameter e has 0 priors.
      Warning: The parameter f has 0 priors. |}]

let gamma_args_example =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        parameters {
          real<lower=0> a;
          real b;
          real<lower=0> c;
          real<lower=0> d;
        }
        model {
          a ~ gamma(0.5, 0.5);
          a ~ gamma(0.1, b);
          a ~ inv_gamma(0.5, 0.5);
          a ~ inv_gamma(0.5, b);
          c ~ gamma(2, 2);
          d ~ gamma(0.4, 0.6);
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Gamma args warning" =
  print_warn_pedantic gamma_args_example ;
  [%expect
    {|
      Warning: The parameter a is on the left-hand side of more than one twiddle statement.
      Warning: The parameter a has 4 priors.
      Warning: The parameter b has 2 priors.
      Warning: At 'string', line 11, column 10 to column 34 your Stan program has a gamma or inverse-gamma model with parameters that are equal to each other and set to values less than 1. This is mathematically acceptable and can make sense in some problems, but typically we see this model used as an attempt to assign a noninformative prior distribution. In fact, priors such as inverse-gamma(.001,.001) can be very strong, as explained by Gelman (2006). Instead we recommend something like a normal(0,1) or student_t(4,0,1), with parameter constrained to be positive.
      Warning: At 'string', line 9, column 10 to column 30 your Stan program has a gamma or inverse-gamma model with parameters that are equal to each other and set to values less than 1. This is mathematically acceptable and can make sense in some problems, but typically we see this model used as an attempt to assign a noninformative prior distribution. In fact, priors such as inverse-gamma(.001,.001) can be very strong, as explained by Gelman (2006). Instead we recommend something like a normal(0,1) or student_t(4,0,1), with parameter constrained to be positive.
      Warning: Parameter b is used as a scale parameter at 'string', line 12, column 10 to column 32, but is not constrained to be positive.
      Warning: Parameter b is used as an inverse scale parameter at 'string', line 10, column 10 to column 28, but is not constrained to be positive.
    |}]

let dist_bounds_example =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        parameters {
          real a;
          real<lower=0> b;
          real c;
          real<lower=0> d;
        }
        model {
          a ~ gamma(2, 2);
          b ~ gamma(2, 2);
          c ~ lognormal(2, 2);
          d ~ lognormal(2, 2);
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Dist bounds warning" =
  print_warn_pedantic dist_bounds_example ;
  [%expect
    {|
      Warning: Parameter a is given a positive distribution at 'string', line 9, column 10 to column 26 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter c is given a positive distribution at 'string', line 11, column 10 to column 30 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
    |}]
