// Poisson regression with hierarchical intercept ("random effect")
functions {
  real poisson_re_log_ll(vector theta, array[] int y, vector mu) {
    return poisson_lpmf(y | exp(mu + theta));
  }
  matrix cov_fun(real sigma, data int N) {
    return diag_matrix(rep_vector(sigma^2, N));
  }
}
data {
  int<lower=0> N;           // number of data points
  int<lower=0> P;           // number of covariates
  matrix[N,P] X;            // covariates
  array[N] int<lower=0> y;  // target
  vector[N] offsett;        // offset (offset variable name is reserved)
  real integrate_1d_reltol;
}
transformed data {
    // control parameters for Laplace approximation
  real tolerance = 1e-6;
  int max_num_steps = 100, hessian_block_size = 1, solver = 1, max_steps_line_search = 0, allow_fallthrough = 1;
}
parameters {
  real alpha;               // intercept
  vector[P] beta;           // slope
  vector[N] z;              // individual intercept ("random effect")
  real<lower=0> sigmaz;     // prior scale for z
}
model {
  // priors
  alpha ~ normal(0, 3);
  beta ~ normal(0, 3);
  z ~ normal(0, sigmaz);
  sigmaz ~ normal(0, 1);
  // observation model
  y ~ poisson_log_glm(X, z+offsett+alpha, beta);
}
generated quantities {

real log_lik_sum = laplace_marginal_tol(poisson_re_log_ll, (y, offsett + alpha + X*beta), hessian_block_size, cov_fun, (sigmaz, N),
              (rep_vector(0.0, N), tolerance, max_num_steps,
               solver, max_steps_line_search, allow_fallthrough));
}
