functions {
  matrix K_function(array[] vector x, int n_obs, real alpha, real rho) {
    return gp_exp_quad_cov(x, alpha, rho);
  }
}
data {
  int n_obs;
  array[n_obs] int y;
  array[n_obs] vector[2] x;
}
parameters {
  real<lower=0> alpha;
  real<lower=0> rho;
  vector<lower=0>[n_obs] eta;
}
model {
  // the overdispersion must be a scalar, not a vector
  target += laplace_marginal_neg_binomial_2_log_lpmf(y | y, eta,
              rep_vector(0.0, n_obs), 1, K_function, (x, n_obs, alpha, rho));
}
