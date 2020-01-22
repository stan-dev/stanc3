open Core_kernel
open Frontend
open Analysis_and_optimization
open Analysis_and_optimization.Pedantic_analysis

let build_program prog =
    (Ast_to_Mir.trans_prog ""
       (Option.value_exn
          (Result.ok
             (Semantic_check.semantic_check_program
                (Option.value_exn
                   (Result.ok
                      (Parse.parse_string Parser.Incremental.program prog)))))))

let sigma_example =
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
          real z = 1 - 2;
          x ~ normal (0, z);
        }
      |}

let%expect_test "Unbounded sigma warning" =
  print_warn_pedantic (build_program sigma_example) ;
  [%expect
    {|
      Warning: The parameter x has 6 priors.
      Warning: Your Stan program has a parameter "sigma_e" with hard constraints in its declaration. Hard constraints are not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a hard constraint can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of constraining an elasticity parameter to fall between 0, and 1, leave it unconstrained and give it a normal(0.5,0.5) prior distribution.
      Warning: The parameter x is on the left-hand side of more than one twiddle statement.
      Warning: Parameter sigma_a is used as a scale parameter in distribution normal at 'string', line 11, column 25 to column 32, but is not constrained to be positive.
      Warning: Parameter sigma_c is used as a scale parameter in distribution normal at 'string', line 13, column 25 to column 32, but is not constrained to be positive.
      Warning: a scale parameter at 'string', line 16, column 19 to column 24 is -1, but a scale parameter should be non-negative.
    |}]

let uniform_example =
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

let%expect_test "Uniform warning" =
  print_warn_pedantic (build_program uniform_example) ;
  [%expect
    {|
      Warning: The parameter a has 2 priors.
      Warning: At 'string', line 9, column 10 to column 28, your Stan program has a uniform distribution on variable a. The uniform distribution is not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a uniform density can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of giving an elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5).
      Warning: At 'string', line 12, column 10 to column 28, your Stan program has a uniform distribution on variable c. The uniform distribution is not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a uniform density can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of giving an elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5).
      Warning: At 'string', line 13, column 10 to column 28, your Stan program has a uniform distribution on variable d. The uniform distribution is not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a uniform density can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of giving an elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5).
    |}]

let unscaled_example =
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

let%expect_test "Unscaled warning" =
  print_warn_pedantic (build_program unscaled_example) ;
  [%expect
    {|
      Warning: At 'string', line 11, column 21 to column 26, you have the distribution argument 0.001 which is less than 0.1 or more than 10 in magnitude. This suggests that you might have parameters in your model that have not been scaled to roughly order 1. We suggest rescaling using a multiplier; see section 22.12 of the manual for an example.
      Warning: At 'string', line 11, column 28 to column 33, you have the distribution argument 10000 which is less than 0.1 or more than 10 in magnitude. This suggests that you might have parameters in your model that have not been scaled to roughly order 1. We suggest rescaling using a multiplier; see section 22.12 of the manual for an example. |}]

let multi_twiddle_example =
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

let%expect_test "Multi twiddle warning" =
  print_warn_pedantic (build_program multi_twiddle_example) ;
  [%expect
    {|
      Warning: The parameter x has 2 priors.
      Warning: The parameter y has 2 priors.
      Warning: The parameter x is on the left-hand side of more than one twiddle statement. |}]

let hard_constrained_example =
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

let%expect_test "Hard constraint warning" =
  print_warn_pedantic (build_program hard_constrained_example) ;
  [%expect
    {|
      Warning: The parameter a was declared but does not participate in the model.
      Warning: The parameter b was declared but does not participate in the model.
      Warning: The parameter c was declared but does not participate in the model.
      Warning: The parameter d was declared but does not participate in the model.
      Warning: Your Stan program has a parameter "c" with hard constraints in its declaration. Hard constraints are not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a hard constraint can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of constraining an elasticity parameter to fall between 0, and 1, leave it unconstrained and give it a normal(0.5,0.5) prior distribution.
      Warning: Your Stan program has a parameter "d" with hard constraints in its declaration. Hard constraints are not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a hard constraint can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of constraining an elasticity parameter to fall between 0, and 1, leave it unconstrained and give it a normal(0.5,0.5) prior distribution. |}]

let unused_param_example =
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

let%expect_test "Unused param warning" =
  print_warn_pedantic (build_program unused_param_example) ;
  [%expect
    {|
      Warning: The parameter b has 2 priors.
      Warning: The parameter c was declared but does not participate in the model.
      Warning: The parameter d was declared but does not participate in the model.
      Warning: The parameter e was declared but does not participate in the model.
      Warning: The parameter f was declared but does not participate in the model. |}]

let param_dependant_cf_example =
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

let%expect_test "Parameter dependent control flow warning" =
  print_warn_pedantic (build_program param_dependant_cf_example) ;
  [%expect
    {|
      Warning: The control flow statement at 'string', line 9, column 10 to line 13, column 11 depends on parameter(s): a.
      Warning: The control flow statement at 'string', line 14, column 10 to line 16, column 11 depends on parameter(s): a.
      Warning: The control flow statement at 'string', line 17, column 10 to line 19, column 11 depends on parameter(s): a. |}]

let non_one_priors_example =
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

let%expect_test "Non-one priors no warning" =
  print_warn_pedantic (build_program non_one_priors_example) ;
  [%expect
    {|
      Warning: Parameter b is used as a scale parameter in distribution normal at 'string', line 15, column 24 to column 25, but is not constrained to be positive.
      Warning: Parameter d is used as a scale parameter in distribution normal at 'string', line 17, column 24 to column 25, but is not constrained to be positive. |}]

let non_one_priors_example2 =
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

let%expect_test "Non-one priors warning" =
  print_warn_pedantic (build_program non_one_priors_example2) ;
  [%expect
    {|
      Warning: The parameter a has 3 priors.
      Warning: The parameter b has 2 priors.
      Warning: The parameter c has 0 priors.
      Warning: The parameter d has 0 priors.
      Warning: The parameter e has 0 priors.
      Warning: The parameter f has 0 priors.
      Warning: The parameter f is on the left-hand side of more than one twiddle statement. |}]

let gamma_args_example =
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

let%expect_test "Gamma args warning" =
  print_warn_pedantic (build_program gamma_args_example) ;
  [%expect
    {|
      Warning: The parameter a has 4 priors.
      Warning: The parameter b has 2 priors.
      Warning: The parameter a is on the left-hand side of more than one twiddle statement.
      Warning: At 'string', line 9, column 10 to column 30 your Stan program has a gamma or inverse-gamma model with parameters that are equal to each other and set to values less than 1. This is mathematically acceptable and can make sense in some problems, but typically we see this model used as an attempt to assign a noninformative prior distribution. In fact, priors such as inverse-gamma(.001,.001) can be very strong, as explained by Gelman (2006). Instead we recommend something like a normal(0,1) or student_t(4,0,1), with parameter constrained to be positive.
      Warning: Parameter b is used as an inverse scale parameter in distribution gamma at 'string', line 10, column 25 to column 26, but is not constrained to be positive.
      Warning: At 'string', line 11, column 10 to column 34 your Stan program has a gamma or inverse-gamma model with parameters that are equal to each other and set to values less than 1. This is mathematically acceptable and can make sense in some problems, but typically we see this model used as an attempt to assign a noninformative prior distribution. In fact, priors such as inverse-gamma(.001,.001) can be very strong, as explained by Gelman (2006). Instead we recommend something like a normal(0,1) or student_t(4,0,1), with parameter constrained to be positive.
      Warning: Parameter b is used as a scale parameter in distribution inv_gamma at 'string', line 12, column 29 to column 30, but is not constrained to be positive.
    |}]

let dist_bounds_example =
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

let%expect_test "Dist bounds warning" =
  print_warn_pedantic (build_program dist_bounds_example) ;
  [%expect
    {|
      Warning: Parameter a is given a positive distribution gamma at 'string', line 9, column 10 to column 11 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter c is given a positive distribution lognormal at 'string', line 11, column 10 to column 11 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
    |}]

let dist_examples =
  {|
parameters {
  real x;
  real<lower=0> x_pos;
  real<lower=0,upper=1> x_unit;
  real unb_p;
  real<lower=0> pos_p;
  real<lower=0, upper=1> unit_p;
  vector[1] vec;
  vector<lower=0>[1] pos_vec;
  vector<lower=0,upper=1>[1] unit_vec;
  matrix[1,1] mat;
}
model {
  x ~ normal(0, unb_p);
  x ~ normal(0, pos_p);
  vec ~ normal_id_glm(mat, vec, vec, unb_p); // missing b.c. lpdf suffix
  vec ~ normal_id_glm(mat, vec, vec, pos_p);
  x ~ exp_mod_normal(0, unb_p, unb_p);
  x ~ exp_mod_normal(0, pos_p, pos_p);
  x ~ skew_normal(0, unb_p, 0);
  x ~ skew_normal(0, pos_p, 0);
  x ~ student_t(unb_p, 0, unb_p);
  x ~ student_t(pos_p, 0, pos_p);
  x ~ cauchy(0, unb_p); // missing
  x ~ cauchy(0, pos_p);
  x ~ double_exponential(0, unb_p);
  x ~ double_exponential(0, pos_p);
  x ~ logistic(0, unb_p);
  x ~ logistic(0, pos_p);
  x ~ gumbel(0, unb_p);
  x ~ gumbel(0, pos_p);
  x ~ lognormal(0, unb_p);
  x_pos ~ lognormal(0, pos_p);
  x ~ chi_square(unb_p);
  x_pos ~ chi_square(pos_p);
  x ~ inv_chi_square(unb_p);
  x_pos ~ inv_chi_square(pos_p);
  x ~ scaled_inv_chi_square(unb_p, unb_p);
  x_pos ~ scaled_inv_chi_square(pos_p, pos_p);
  x ~ exponential(unb_p);
  x_pos ~ exponential(pos_p);
  x ~ gamma(unb_p, unb_p);
  x_pos ~ gamma(pos_p, pos_p);
  x ~ inv_gamma(unb_p, unb_p);
  x_pos ~ inv_gamma(pos_p, pos_p);
  x ~ weibull(unb_p, unb_p);
  x_pos ~ weibull(pos_p, pos_p);
  x ~ frechet(unb_p, unb_p);
  x_pos ~ frechet(pos_p, pos_p);
  x ~ rayleigh(unb_p);
  x_pos ~ rayleigh(pos_p);
  x ~ wiener(unb_p, unb_p, pos_p, 0);
  x_pos ~ wiener(pos_p, pos_p, unit_p, 0);
  x ~ pareto(unb_p, unb_p);
  x_pos ~ pareto(pos_p, pos_p);
  x ~ pareto_type_2(0, unb_p, unb_p);
  x ~ pareto_type_2(0, pos_p, pos_p);
  x ~ beta(unb_p, unb_p);
  x_unit ~ beta(pos_p, pos_p);
  x ~ beta_proportion(unb_p, unb_p);
  x_unit ~ beta_proportion(unit_p, pos_p);
  x ~ von_mises(0, unb_p);
  x ~ von_mises(0, pos_p);
  vec ~ dirichlet(vec);
  unit_vec ~ dirichlet(pos_vec);
}
|}

let%expect_test "Dist warnings" =
  print_warn_pedantic (build_program dist_examples) ;
  [%expect
    {|
      Warning: The parameter mat has 2 priors.
      Warning: The parameter pos_p has 26 priors.
      Warning: The parameter unb_p has 25 priors.
      Warning: The parameter unit_p has 2 priors.
      Warning: The parameter vec has 3 priors.
      Warning: The parameter x has 34 priors.
      Warning: The parameter x_pos has 12 priors.
      Warning: The parameter x_unit has 2 priors.
      Warning: The parameter x is on the left-hand side of more than one twiddle statement.
      Warning: Parameter unb_p is used as a scale parameter in distribution normal at 'string', line 15, column 16 to column 21, but is not constrained to be positive.
      Warning: The parameter vec is on the left-hand side of more than one twiddle statement.
      Warning: Parameter unb_p is used as a scale parameter in distribution normal_id_glm at 'string', line 17, column 37 to column 42, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a scale parameter in distribution exp_mod_normal at 'string', line 19, column 24 to column 29, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a shape parameter in distribution exp_mod_normal at 'string', line 19, column 31 to column 36, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a scale parameter in distribution skew_normal at 'string', line 21, column 21 to column 26, but is not constrained to be positive.
      Warning: Parameter unb_p is used as degrees of freedom in distribution student_t at 'string', line 23, column 16 to column 21, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a scale parameter in distribution student_t at 'string', line 23, column 26 to column 31, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a scale parameter in distribution cauchy at 'string', line 25, column 16 to column 21, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a scale parameter in distribution double_exponential at 'string', line 27, column 28 to column 33, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a scale parameter in distribution logistic at 'string', line 29, column 18 to column 23, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a scale parameter in distribution gumbel at 'string', line 31, column 16 to column 21, but is not constrained to be positive.
      Warning: Parameter x is given a positive distribution lognormal at 'string', line 33, column 2 to column 3 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter unb_p is used as a scale parameter in distribution lognormal at 'string', line 33, column 19 to column 24, but is not constrained to be positive.
      Warning: The parameter x_pos is on the left-hand side of more than one twiddle statement.
      Warning: Parameter x is given a positive distribution chi_square at 'string', line 35, column 2 to column 3 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter unb_p is used as degrees of freedom in distribution chi_square at 'string', line 35, column 17 to column 22, but is not constrained to be positive.
      Warning: Parameter x is given a positive distribution inv_chi_square at 'string', line 37, column 2 to column 3 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter unb_p is used as degrees of freedom in distribution inv_chi_square at 'string', line 37, column 21 to column 26, but is not constrained to be positive.
      Warning: Parameter x is given a positive distribution scaled_inv_chi_square at 'string', line 39, column 2 to column 3 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter unb_p is used as degrees of freedom in distribution scaled_inv_chi_square at 'string', line 39, column 28 to column 33, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a scale parameter in distribution scaled_inv_chi_square at 'string', line 39, column 35 to column 40, but is not constrained to be positive.
      Warning: Parameter x is given a positive distribution exponential at 'string', line 41, column 2 to column 3 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter unb_p is used as a scale parameter in distribution exponential at 'string', line 41, column 18 to column 23, but is not constrained to be positive.
      Warning: Parameter x is given a positive distribution gamma at 'string', line 43, column 2 to column 3 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter unb_p is used as a shape parameter in distribution gamma at 'string', line 43, column 12 to column 17, but is not constrained to be positive.
      Warning: Parameter unb_p is used as an inverse scale parameter in distribution gamma at 'string', line 43, column 19 to column 24, but is not constrained to be positive.
      Warning: Parameter x is given a positive distribution inv_gamma at 'string', line 45, column 2 to column 3 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter unb_p is used as a shape parameter in distribution inv_gamma at 'string', line 45, column 16 to column 21, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a scale parameter in distribution inv_gamma at 'string', line 45, column 23 to column 28, but is not constrained to be positive.
      Warning: Parameter x is given a positive distribution weibull at 'string', line 47, column 2 to column 3 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter unb_p is used as a shape parameter in distribution weibull at 'string', line 47, column 14 to column 19, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a scale parameter in distribution weibull at 'string', line 47, column 21 to column 26, but is not constrained to be positive.
      Warning: Parameter x is given a positive distribution frechet at 'string', line 49, column 2 to column 3 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter unb_p is used as a shape parameter in distribution frechet at 'string', line 49, column 14 to column 19, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a scale parameter in distribution frechet at 'string', line 49, column 21 to column 26, but is not constrained to be positive.
      Warning: Parameter x is given a positive distribution rayleigh at 'string', line 51, column 2 to column 3 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter unb_p is used as a scale parameter in distribution rayleigh at 'string', line 51, column 15 to column 20, but is not constrained to be positive.
      Warning: Parameter x is given a positive distribution wiener at 'string', line 53, column 2 to column 3 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter unb_p is used as a boundary separation parameter in distribution wiener at 'string', line 53, column 13 to column 18, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a non-decision time parameter in distribution wiener at 'string', line 53, column 20 to column 25, but is not constrained to be positive.
      Warning: Parameter pos_p is used as an a-priori bias parameter in distribution wiener at 'string', line 53, column 27 to column 32, but is not constrained to be on [0,1].
      Warning: Parameter x is given a positive distribution pareto at 'string', line 55, column 2 to column 3 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter unb_p is used as a positive minimum parameter in distribution pareto at 'string', line 55, column 13 to column 18, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a shape parameter in distribution pareto at 'string', line 55, column 20 to column 25, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a scale parameter in distribution pareto_type_2 at 'string', line 57, column 23 to column 28, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a shape parameter in distribution pareto_type_2 at 'string', line 57, column 30 to column 35, but is not constrained to be positive.
      Warning: Parameter x is given a [0,1] distribution beta at 'string', line 59, column 2 to column 3 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter unb_p is used as a count parameter in distribution beta at 'string', line 59, column 11 to column 16, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a count parameter in distribution beta at 'string', line 59, column 18 to column 23, but is not constrained to be positive.
      Warning: The parameter x_unit is on the left-hand side of more than one twiddle statement.
      Warning: Parameter x is given a [0,1] distribution beta_proportion at 'string', line 61, column 2 to column 3 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter unb_p is used as a unit mean parameter in distribution beta_proportion at 'string', line 61, column 22 to column 27, but is not constrained to be on [0,1].
      Warning: Parameter unb_p is used as a precision parameter in distribution beta_proportion at 'string', line 61, column 29 to column 34, but is not constrained to be positive.
      Warning: Parameter unb_p is used as a scale parameter in distribution von_mises at 'string', line 63, column 19 to column 24, but is not constrained to be positive.
      Warning: Parameter vec is given a [0,1] distribution dirichlet at 'string', line 65, column 2 to column 5 but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.
      Warning: Parameter vec is used as a count parameter in distribution dirichlet at 'string', line 65, column 18 to column 21, but is not constrained to be positive.
    |}]
