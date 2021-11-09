open Core_kernel
open Frontend
open Analysis_and_optimization.Pedantic_analysis

let build_program prog =
  Frontend_utils.typed_ast_of_string_exn prog |> Ast_to_Mir.trans_prog ""

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

let print_warn_pedantic p =
  p |> warn_pedantic
  |> Fmt.strf "%a" (Warnings.pp_warnings ?printed_filename:None)
  |> print_endline

let%expect_test "Unbounded sigma warning" =
  print_warn_pedantic (build_program sigma_example) ;
  [%expect
    {|
      Warning in 'string', line 16, column 19: A normal distribution is given value -1 as a scale parameter (argument 2), but a scale parameter is not strictly positive.
      Warning in 'string', line 13, column 25: A normal distribution is given parameter sigma_c as a scale parameter (argument 2), but sigma_c was not constrained to be strictly positive.
      Warning in 'string', line 11, column 25: A normal distribution is given parameter sigma_a as a scale parameter (argument 2), but sigma_a was not constrained to be strictly positive.
      Warning in 'string', line 11, column 10: The parameter x is on the left-hand side of more than one twiddle statement.
      Warning: Your Stan program has a parameter sigma_e with a lower and upper bound in its declaration. These hard constraints are not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a hard constraint can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of constraining an elasticity parameter to fall between 0, and 1, leave it unconstrained and give it a normal(0.5,0.5) prior distribution.
      Warning: The parameter x has 6 priors.
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
      Warning in 'string', line 13, column 10: Parameter d is given a uniform distribution. The uniform distribution is not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a uniform density can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of giving an elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5).
      Warning in 'string', line 12, column 10: Parameter c is given a uniform distribution. The uniform distribution is not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a uniform density can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of giving an elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5).
      Warning in 'string', line 9, column 10: Parameter a is given a uniform distribution. The uniform distribution is not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a uniform density can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of giving an elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5).
      Warning: The parameter a has 2 priors.
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
      Warning in 'string', line 11, column 28: Argument 10000 suggests there may be parameters that are not unit scale; consider rescaling with a multiplier (see manual section 22.12).
      Warning in 'string', line 11, column 21: Argument 0.001 suggests there may be parameters that are not unit scale; consider rescaling with a multiplier (see manual section 22.12). |}]

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
      Warning in 'string', line 7, column 10: The parameter x is on the left-hand side of more than one twiddle statement.
      Warning: The parameter y has 2 priors.
      Warning: The parameter x has 2 priors. |}]

let hard_constrained_example =
  {|
        parameters {
          real<lower=0, upper=1> a;
          real<lower=-1, upper=1> b;
          real<lower=0, upper=10> c;
          real<lower=-1, upper=0> d;
          real<lower=0, upper=0> e;
          real<lower=1, upper=-1> f;
        }
        model {
        }
      |}

let%expect_test "Hard constraint warning" =
  print_warn_pedantic (build_program hard_constrained_example) ;
  [%expect
    {|
      Warning: Your Stan program has a parameter d with a lower and upper bound in its declaration. These hard constraints are not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a hard constraint can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of constraining an elasticity parameter to fall between 0, and 1, leave it unconstrained and give it a normal(0.5,0.5) prior distribution.
      Warning: Your Stan program has a parameter c with a lower and upper bound in its declaration. These hard constraints are not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a hard constraint can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of constraining an elasticity parameter to fall between 0, and 1, leave it unconstrained and give it a normal(0.5,0.5) prior distribution.
      Warning: The parameter f was declared but was not used in the density calculation.
      Warning: The parameter e was declared but was not used in the density calculation.
      Warning: The parameter d was declared but was not used in the density calculation.
      Warning: The parameter c was declared but was not used in the density calculation.
      Warning: The parameter b was declared but was not used in the density calculation.
      Warning: The parameter a was declared but was not used in the density calculation.
      Warning: Parameter f has constraints that don't make sense. The lower bound should be strictly less than the upper bound.
      Warning: Parameter e has constraints that don't make sense. The lower bound should be strictly less than the upper bound. |}]

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
      Warning: The parameter e was declared but was not used in the density calculation.
      Warning: The parameter d was declared but was not used in the density calculation.
      Warning: The parameter c was declared but was not used in the density calculation.
      Warning: The parameter b has 2 priors. |}]

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
      Warning in 'string', line 17, column 10: A control flow statement depends on parameter(s): a.
      Warning in 'string', line 14, column 10: A control flow statement depends on parameter(s): a.
      Warning in 'string', line 9, column 10: A control flow statement depends on parameter(s): a. |}]

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
      Warning in 'string', line 17, column 24: A normal distribution is given parameter d as a scale parameter (argument 2), but d was not constrained to be strictly positive.
      Warning in 'string', line 15, column 24: A normal distribution is given parameter b as a scale parameter (argument 2), but b was not constrained to be strictly positive. |}]

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
      Warning in 'string', line 22, column 10: The parameter f is on the left-hand side of more than one twiddle statement.
      Warning: The parameter f has no priors.
      Warning: The parameter e has no priors.
      Warning: The parameter d has no priors.
      Warning: The parameter c has no priors.
      Warning: The parameter b has 2 priors.
      Warning: The parameter a has 3 priors. |}]

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
      Warning in 'string', line 12, column 29: A inv_gamma distribution is given parameter b as a scale parameter (argument 2), but b was not constrained to be strictly positive.
      Warning in 'string', line 11, column 24: There is a gamma or inverse-gamma distribution with parameters that are equal to each other and set to values less than 1. This is mathematically acceptable and can make sense in some problems, but typically we see this model used as an attempt to assign a noninformative prior distribution. In fact, priors such as inverse-gamma(.001,.001) can be very strong, as explained by Gelman (2006). Instead we recommend something like a normal(0,1) or student_t(4,0,1), with parameter constrained to be positive.
      Warning in 'string', line 10, column 25: A gamma distribution is given parameter b as an inverse scale parameter (argument 2), but b was not constrained to be strictly positive.
      Warning in 'string', line 9, column 20: There is a gamma or inverse-gamma distribution with parameters that are equal to each other and set to values less than 1. This is mathematically acceptable and can make sense in some problems, but typically we see this model used as an attempt to assign a noninformative prior distribution. In fact, priors such as inverse-gamma(.001,.001) can be very strong, as explained by Gelman (2006). Instead we recommend something like a normal(0,1) or student_t(4,0,1), with parameter constrained to be positive.
      Warning in 'string', line 9, column 10: The parameter a is on the left-hand side of more than one twiddle statement.
      Warning: The parameter b has 2 priors.
      Warning: The parameter a has 4 priors.
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
      Warning in 'string', line 11, column 10: Parameter c is given a lognormal distribution, which has strictly positive support, but c was not constrained to be strictly positive.
      Warning in 'string', line 9, column 10: Parameter a is given a gamma distribution, which has strictly positive support, but a was not constrained to be strictly positive. |}]

let dist_examples =
  {|
data {
  int unb_d;
  int<lower=0, upper=1> bin_d;
  int<lower=0> pos_d;
  int unb_d_vec[1];
  int<lower=0, upper=1> bin_d_vec[1];
  int<lower=0> pos_d_vec[1];
}
parameters {
  real x;
  real<lower=0> x_pos;
  real<lower=0,upper=1> x_unit;
  real unb_p;
  real<lower=0> pos_p;
  real<lower=0, upper=1> unit_p;
  vector[1] vec;
  vector<lower=0>[1] pos_vec;
  matrix[1,1] mat;
  simplex[1] sim;
  ordered[1] ord;
  cov_matrix[1] cov;
  cholesky_factor_cov[1] chol_cov;
  corr_matrix[1] corr;
  cholesky_factor_corr[1] chol_corr;
}
model {
  unb_d ~ bernoulli(unb_p);
  bin_d ~ bernoulli(unit_p);
  unb_d ~ bernoulli_logit(unb_p);
  bin_d ~ bernoulli_logit(unb_p); // can't catch anything yet
  unb_d_vec ~ bernoulli_logit_glm(mat, vec, vec);
  bin_d_vec ~ bernoulli_logit_glm(mat, vec, vec); // can't catch anything yet
  unb_d ~ binomial(unb_d, unb_p);
  bin_d ~ binomial(pos_d, unit_p);
  unb_d ~ binomial_logit(unb_d, unb_p);
  bin_d ~ binomial_logit(pos_d, unb_p);
  unb_d ~ beta_binomial(unb_d, unb_p, unb_p);
  pos_d ~ beta_binomial(pos_d, pos_p, pos_p);
  unb_d ~ hypergeometric(unb_d, unb_d, unb_d);
  pos_d ~ hypergeometric(pos_d, pos_d, pos_d);
  unb_d ~ categorical(vec);
  pos_d ~ categorical(sim);
  unb_d ~ ordered_logistic(unb_p, vec);
  pos_d ~ ordered_logistic(unb_p, ord);
  unb_d ~ ordered_probit(unb_p, vec);
  pos_d ~ ordered_probit(unb_p, ord);
  unb_d ~ neg_binomial(unb_p, unb_p);
  pos_d ~ neg_binomial(pos_p, pos_p);
  unb_d ~ neg_binomial_2(unb_p, unb_p);
  pos_d ~ neg_binomial_2(pos_p, pos_p);
  unb_d ~ neg_binomial_2_log(unb_p, unb_p);
  pos_d ~ neg_binomial_2_log(unb_p, pos_p);
  unb_d_vec ~ neg_binomial_2_log_glm(mat, vec, vec, unb_p);
  unb_d_vec ~ neg_binomial_2_log_glm(mat, vec, vec, pos_p);
  unb_d ~ poisson(unb_p);
  pos_d ~ poisson(pos_p);
  unb_d ~ poisson_log(unb_p);
  pos_d ~ poisson_log(unb_p);
  unb_d_vec ~ poisson_log_glm(mat, vec, vec);
  pos_d_vec ~ poisson_log_glm(mat, vec, vec);
  unb_d_vec ~ multinomial(vec);
  pos_d_vec ~ multinomial(sim);
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
  vec ~ multi_normal(vec, mat);
  vec ~ multi_normal(vec, cov);
  vec ~ multi_normal_prec(vec, mat);
  vec ~ multi_normal_prec(vec, cov);
  vec ~ multi_normal_cholesky(vec, cov);
  vec ~ multi_normal_cholesky(vec, chol_cov);
  mat ~ multi_gp(mat, vec);
  mat ~ multi_gp(cov, vec);
  mat ~ multi_gp_cholesky(cov, vec);
  mat ~ multi_gp_cholesky(chol_cov, vec);
  vec ~ multi_student_t(unb_p, vec, mat);
  vec ~ multi_student_t(pos_p, vec, cov);
  mat ~ gaussian_dlm_obs(mat, mat, mat, mat, vec, mat);
  mat ~ gaussian_dlm_obs(mat, mat, cov, cov, vec, mat);
  vec ~ dirichlet(vec);
  sim ~ dirichlet(pos_vec);
  mat ~ lkj_corr(unb_p);
  corr ~ lkj_corr(pos_p);
  corr ~ lkj_corr_cholesky(unb_p);
  chol_corr ~ lkj_corr_cholesky(pos_p);
  mat ~ wishart(unb_p, mat);
  cov ~ wishart(pos_p, cov);
  mat ~ inv_wishart(unb_p, mat);
  cov ~ inv_wishart(pos_p, cov);
}
|}

(* Distribution warnings should appear only on alternating lines,
   since the program lines go incorrect,correct,incorrect,correct,etc.*)
let%expect_test "Dist warnings" =
  print_warn_pedantic (build_program dist_examples) ;
  [%expect
    {|
      Warning in 'string', line 136, column 27: A inv_wishart distribution is given parameter mat as a scale matrix (argument 2), but mat was not constrained to be covariance.
      Warning in 'string', line 136, column 20: A inv_wishart distribution is given parameter unb_p as degrees of freedom (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 136, column 2: Parameter mat is given a inv_wishart distribution, which has covariance support, but mat was not constrained to be covariance.
      Warning in 'string', line 135, column 2: The parameter cov is on the left-hand side of more than one twiddle statement.
      Warning in 'string', line 134, column 23: A wishart distribution is given parameter mat as a scale matrix (argument 2), but mat was not constrained to be covariance.
      Warning in 'string', line 134, column 16: A wishart distribution is given parameter unb_p as degrees of freedom (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 134, column 2: Parameter mat is given a wishart distribution, which has covariance support, but mat was not constrained to be covariance.
      Warning in 'string', line 132, column 27: A lkj_corr_cholesky distribution is given parameter unb_p as a shape parameter (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 132, column 2: Parameter corr is given a lkj_corr_cholesky distribution, which has Cholesky factor of correlation support, but corr was not constrained to be Cholesky factor of correlation.
      Warning in 'string', line 131, column 2: The parameter corr is on the left-hand side of more than one twiddle statement.
      Warning in 'string', line 131, column 2: It is suggested to reparameterize your model to replace lkj_corr with lkj_corr_cholesky, the Cholesky factor variant. lkj_corr tends to run slower, consume more memory, and has higher risk of numerical errors.
      Warning in 'string', line 130, column 17: A lkj_corr distribution is given parameter unb_p as a shape parameter (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 130, column 2: It is suggested to reparameterize your model to replace lkj_corr with lkj_corr_cholesky, the Cholesky factor variant. lkj_corr tends to run slower, consume more memory, and has higher risk of numerical errors.
      Warning in 'string', line 130, column 2: Parameter mat is given a lkj_corr distribution, which has correlation support, but mat was not constrained to be correlation.
      Warning in 'string', line 128, column 18: A dirichlet distribution is given parameter vec as a count parameter (argument 1), but vec was not constrained to be strictly positive.
      Warning in 'string', line 128, column 2: Parameter vec is given a dirichlet distribution, which has simplex support, but vec was not constrained to be simplex.
      Warning in 'string', line 126, column 40: A gaussian_dlm_obs distribution is given parameter mat as system covariance matrix (argument 4), but mat was not constrained to be covariance.
      Warning in 'string', line 126, column 35: A gaussian_dlm_obs distribution is given parameter mat as observation covariance matrix (argument 3), but mat was not constrained to be covariance.
      Warning in 'string', line 124, column 36: A multi_student_t distribution is given parameter mat as a scale matrix (argument 3), but mat was not constrained to be covariance.
      Warning in 'string', line 124, column 24: A multi_student_t distribution is given parameter unb_p as degrees of freedom (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 122, column 26: A multi_gp_cholesky distribution is given parameter cov as Cholesky factor of the kernel matrix (argument 1), but cov was not constrained to be Cholesky factor of covariance.
      Warning in 'string', line 120, column 17: A multi_gp distribution is given parameter mat as a kernel matrix (argument 1), but mat was not constrained to be covariance.
      Warning in 'string', line 120, column 2: The parameter mat is on the left-hand side of more than one twiddle statement.
      Warning in 'string', line 118, column 35: A multi_normal_cholesky distribution is given parameter cov as a covariance matrix (argument 2), but cov was not constrained to be Cholesky factor of covariance.
      Warning in 'string', line 116, column 31: A multi_normal_prec distribution is given parameter mat as a precision matrix (argument 2), but mat was not constrained to be covariance.
      Warning in 'string', line 114, column 26: A multi_normal distribution is given parameter mat as a covariance matrix (argument 2), but mat was not constrained to be covariance.
      Warning in 'string', line 112, column 19: A von_mises distribution is given parameter unb_p as a scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 110, column 29: A beta_proportion distribution is given parameter unb_p as a precision parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 110, column 22: A beta_proportion distribution is given parameter unb_p as a unit mean parameter (argument 1), but unb_p was not constrained to be (0,1).
      Warning in 'string', line 110, column 2: Parameter x is given a beta_proportion distribution, which has (0,1) support, but x was not constrained to be (0,1).
      Warning in 'string', line 109, column 2: The parameter x_unit is on the left-hand side of more than one twiddle statement.
      Warning in 'string', line 108, column 18: A beta distribution is given parameter unb_p as a count parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 108, column 11: A beta distribution is given parameter unb_p as a count parameter (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 108, column 2: Parameter x is given a beta distribution, which has (0,1) support, but x was not constrained to be (0,1).
      Warning in 'string', line 106, column 30: A pareto_type_2 distribution is given parameter unb_p as a shape parameter (argument 3), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 106, column 23: A pareto_type_2 distribution is given parameter unb_p as a scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 104, column 20: A pareto distribution is given parameter unb_p as a shape parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 104, column 13: A pareto distribution is given parameter unb_p as a positive minimum parameter (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 104, column 2: Parameter x is given a pareto distribution, which has strictly positive support, but x was not constrained to be strictly positive.
      Warning in 'string', line 102, column 27: A wiener distribution is given parameter pos_p as an a-priori bias parameter (argument 3), but pos_p was not constrained to be [0,1].
      Warning in 'string', line 102, column 20: A wiener distribution is given parameter unb_p as a non-decision time parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 102, column 13: A wiener distribution is given parameter unb_p as a boundary separation parameter (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 102, column 2: Parameter x is given a wiener distribution, which has strictly positive support, but x was not constrained to be strictly positive.
      Warning in 'string', line 100, column 15: A rayleigh distribution is given parameter unb_p as a scale parameter (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 100, column 2: Parameter x is given a rayleigh distribution, which has non-negative support, but x was not constrained to be non-negative.
      Warning in 'string', line 98, column 21: A frechet distribution is given parameter unb_p as a scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 98, column 14: A frechet distribution is given parameter unb_p as a shape parameter (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 98, column 2: Parameter x is given a frechet distribution, which has strictly positive support, but x was not constrained to be strictly positive.
      Warning in 'string', line 96, column 21: A weibull distribution is given parameter unb_p as a scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 96, column 14: A weibull distribution is given parameter unb_p as a shape parameter (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 96, column 2: Parameter x is given a weibull distribution, which has non-negative support, but x was not constrained to be non-negative.
      Warning in 'string', line 94, column 23: A inv_gamma distribution is given parameter unb_p as a scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 94, column 16: A inv_gamma distribution is given parameter unb_p as a shape parameter (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 94, column 2: Parameter x is given a inv_gamma distribution, which has strictly positive support, but x was not constrained to be strictly positive.
      Warning in 'string', line 92, column 19: A gamma distribution is given parameter unb_p as an inverse scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 92, column 12: A gamma distribution is given parameter unb_p as a shape parameter (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 92, column 2: Parameter x is given a gamma distribution, which has strictly positive support, but x was not constrained to be strictly positive.
      Warning in 'string', line 90, column 18: A exponential distribution is given parameter unb_p as a scale parameter (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 90, column 2: Parameter x is given a exponential distribution, which has strictly positive support, but x was not constrained to be strictly positive.
      Warning in 'string', line 88, column 35: A scaled_inv_chi_square distribution is given parameter unb_p as a scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 88, column 28: A scaled_inv_chi_square distribution is given parameter unb_p as degrees of freedom (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 88, column 2: Parameter x is given a scaled_inv_chi_square distribution, which has strictly positive support, but x was not constrained to be strictly positive.
      Warning in 'string', line 86, column 21: A inv_chi_square distribution is given parameter unb_p as degrees of freedom (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 86, column 2: Parameter x is given a inv_chi_square distribution, which has strictly positive support, but x was not constrained to be strictly positive.
      Warning in 'string', line 84, column 17: A chi_square distribution is given parameter unb_p as degrees of freedom (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 84, column 2: Parameter x is given a chi_square distribution, which has strictly positive support, but x was not constrained to be strictly positive.
      Warning in 'string', line 83, column 2: The parameter x_pos is on the left-hand side of more than one twiddle statement.
      Warning in 'string', line 82, column 19: A lognormal distribution is given parameter unb_p as a scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 82, column 2: Parameter x is given a lognormal distribution, which has strictly positive support, but x was not constrained to be strictly positive.
      Warning in 'string', line 80, column 16: A gumbel distribution is given parameter unb_p as a scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 78, column 18: A logistic distribution is given parameter unb_p as a scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 76, column 28: A double_exponential distribution is given parameter unb_p as a scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 74, column 16: A cauchy distribution is given parameter unb_p as a scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 72, column 26: A student_t distribution is given parameter unb_p as a scale parameter (argument 3), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 72, column 16: A student_t distribution is given parameter unb_p as degrees of freedom (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 70, column 21: A skew_normal distribution is given parameter unb_p as a scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 68, column 31: A exp_mod_normal distribution is given parameter unb_p as a shape parameter (argument 3), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 68, column 24: A exp_mod_normal distribution is given parameter unb_p as a scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 66, column 37: A normal_id_glm distribution is given parameter unb_p as a scale parameter (argument 4), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 66, column 2: The parameter vec is on the left-hand side of more than one twiddle statement.
      Warning in 'string', line 64, column 16: A normal distribution is given parameter unb_p as a scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 64, column 2: The parameter x is on the left-hand side of more than one twiddle statement.
      Warning in 'string', line 62, column 26: A multinomial distribution is given parameter vec as a distribution parameter (argument 1), but vec was not constrained to be simplex.
      Warning in 'string', line 61, column 2: The parameter pos_d_vec is on the left-hand side of more than one twiddle statement.
      Warning in 'string', line 56, column 18: A poisson distribution is given parameter unb_p as a rate parameter (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 54, column 52: A neg_binomial_2_log_glm distribution is given parameter unb_p as an inverse overdispersion control parameter (argument 4), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 52, column 36: A neg_binomial_2_log distribution is given parameter unb_p as an inverse overdispersion control parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 50, column 32: A neg_binomial_2 distribution is given parameter unb_p as a precision parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 50, column 25: A neg_binomial_2 distribution is given parameter unb_p as a shape parameter (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 48, column 30: A neg_binomial distribution is given parameter unb_p as an inverse scale parameter (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 48, column 23: A neg_binomial distribution is given parameter unb_p as a shape parameter (argument 1), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 46, column 32: A ordered_probit distribution is given parameter vec as cutpoints (argument 2), but vec was not constrained to be ordered.
      Warning in 'string', line 44, column 34: A ordered_logistic distribution is given parameter vec as cutpoints (argument 2), but vec was not constrained to be ordered.
      Warning in 'string', line 42, column 22: A categorical distribution is given parameter vec as a vector of outcome probabilities (argument 1), but vec was not constrained to be simplex.
      Warning in 'string', line 39, column 2: The parameter pos_d is on the left-hand side of more than one twiddle statement.
      Warning in 'string', line 38, column 38: A beta_binomial distribution is given parameter unb_p as a prior failure count (argument 3), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 38, column 31: A beta_binomial distribution is given parameter unb_p as a prior success count (argument 2), but unb_p was not constrained to be strictly positive.
      Warning in 'string', line 34, column 26: A binomial distribution is given parameter unb_p as chance of success (argument 2), but unb_p was not constrained to be [0,1].
      Warning in 'string', line 32, column 2: The parameter unb_d_vec is on the left-hand side of more than one twiddle statement.
      Warning in 'string', line 29, column 2: The parameter bin_d is on the left-hand side of more than one twiddle statement.
      Warning in 'string', line 28, column 20: A bernoulli distribution is given parameter unb_p as chance of success (argument 1), but unb_p was not constrained to be [0,1].
      Warning in 'string', line 28, column 2: The parameter unb_d is on the left-hand side of more than one twiddle statement.
      Warning: The parameter x_unit has no priors.
      Warning: The parameter x_pos has no priors.
      Warning: The parameter x has no priors.
      Warning: The parameter vec has 2 priors.
      Warning: The parameter unit_p has 2 priors.
      Warning: The parameter unb_p has 15 priors.
      Warning: The parameter sim has 2 priors.
      Warning: The parameter pos_vec has no priors.
      Warning: The parameter pos_p has 19 priors.
      Warning: The parameter ord has no priors.
      Warning: The parameter mat has no priors.
      Warning: The parameter cov has no priors.
      Warning: The parameter corr has no priors.
      Warning: The parameter chol_cov has no priors.
      Warning: The parameter chol_corr has no priors.
    |}]

let fundef_cf_example =
  {|
functions {
  real func(real b) {
    if(b > 0.0) {
      return(1.0);
    } else {
      return(0.0);
    }
  }
}
data {
  int N;
  real x[N];
}
parameters {
  real<lower = 0.0> sigma;
}
model {
  x ~ normal(0, func(sigma));
}
|}

let%expect_test "Function body parameter-dependent control flow" =
  print_warn_pedantic (build_program fundef_cf_example) ;
  [%expect
    {|
      Warning in 'string', line 4, column 4: A control flow statement inside function func depends on argument b. At 'string', line 19, column 21 to column 26, the value of b depends on parameter(s): sigma.
      Warning: The parameter sigma has no priors.
    |}]

let schools_example =
  {|
    data {
      int<lower=0> J;         // number of schools
      real y[J];              // estimated treatment effects
      real<lower=0> sigma[J]; // standard error of effect estimates
    }
    parameters {
      real mu;                // population treatment effect
      real<lower=0> tau;      // standard deviation in treatment effects
      vector[J] eta;          // unscaled deviation from mu by school
    }
    transformed parameters {
      vector[J] theta = mu + tau * eta;        // school treatment effects
    }
    model {
      target += normal_lpdf(eta | 0, 1);       // prior log-density
      target += normal_lpdf(y | theta, sigma); // log-likelihood
    }
  |}

let%expect_test "Missing priors schools warning" =
  print_warn_pedantic (build_program schools_example) ;
  [%expect
    {|
      Warning: The parameter tau has no priors.
      Warning: The parameter mu has no priors.
    |}]
