functions {
  real scalar_tuple(vector theta, tuple(real, real) eta, vector log_ye,
                    array[] int y) {
    return neg_binomial_2_lpmf(y | exp(log_ye + theta), eta.1);
  }

  real arr_vec_tuple(vector theta, real eta, tuple(vector, array[] int) p) {
    return neg_binomial_2_lpmf(p.2 | exp(p.1 + theta), eta);
  }

  real nested(vector theta, tuple(tuple(real, vector), array[] int) p,
              real unused) {
    return neg_binomial_2_lpmf(p.2 | exp(p.1.2 + theta), p.1.1);
  }

  matrix K_function(array[] vector x, int n_obs, real alpha, real rho) {
    matrix[n_obs, n_obs] K = gp_exp_quad_cov(x, alpha, rho);
    for (i in 1 : n_obs)
      K[i, i] += 1e-8;
    return K;
  }
}
data {
  int n_obs;
  int n_coordinates;
  array[n_obs] int y;
  vector[n_obs] ye;
  array[n_obs] vector[n_coordinates] x;
  real rho_location_prior;
  real rho_scale_prior;
  real alpha_location_prior;
  real alpha_scale_prior;
}
transformed data {
  vector[n_obs] log_ye = log(ye);

  // control parameters for Laplace approximation
  real tolerance = 1e-6;
  int max_num_steps = 100;
  int hessian_block_size = 1;
  int solver = 1;
  int max_steps_line_search = 0;
}
parameters {
  real<lower=0> alpha;
  real<lower=0> rho;
  real<lower=0> eta;
}
model {
  rho ~ inv_gamma(rho_location_prior, rho_scale_prior);
  alpha ~ inv_gamma(alpha_location_prior, alpha_scale_prior);
  eta ~ normal(0, 1);

  target += laplace_marginal(scalar_tuple, ((eta, eta), log_ye, y),
                             K_function, (x, n_obs, alpha, rho));

  target += laplace_marginal(arr_vec_tuple, (eta, (log_ye, y)),
                             K_function, (x, n_obs, alpha, rho));

  target += laplace_marginal(nested, (((eta, log_ye), y), eta),
                             K_function, (x, n_obs, alpha, rho));
}
generated quantities {
  vector[n_obs] theta = laplace_latent_rng(scalar_tuple,
                          ((eta, eta), log_ye, y), K_function,
                          (x, n_obs, alpha, rho));

  vector[n_obs] theta2 = laplace_latent_rng(arr_vec_tuple,
                           (eta, (log_ye, y)), K_function,
                           (x, n_obs, alpha, rho));

  vector[n_obs] theta3 = laplace_latent_rng(nested,
                           (((eta, log_ye), y), eta), K_function,
                           (x, n_obs, alpha, rho));
}
