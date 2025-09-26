functions {
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

  vector[n_obs] theta_0 = rep_vector(0.0, n_obs); // initial guess

  // control parameters for Laplace approximation
  real tolerance = 1e-6;
  int max_num_steps = 100;
  int hessian_block_size = 1;
  int solver = 1;
  int max_steps_line_search = 0;

  vector[n_obs] prior_mean = rep_vector(0.0, n_obs);
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

  y ~ laplace_marginal_neg_binomial_2_log(y, log_ye, prior_mean, K_function,
        (x, n_obs, alpha, rho));

  target += laplace_marginal_neg_binomial_2_log_lpmf(y | y, log_ye,
              prior_mean, K_function, (x, n_obs, alpha, rho));

  target += laplace_marginal_neg_binomial_2_log_lupmf(y | y, log_ye,
              prior_mean, K_function, (x, n_obs, alpha, rho));

  y ~ laplace_marginal_tol_neg_binomial_2_log(y, log_ye, prior_mean, K_function,
        (x, n_obs, alpha, rho), theta_0, tolerance, max_num_steps, hessian_block_size,
        solver, max_steps_line_search);

  target += laplace_marginal_tol_neg_binomial_2_log_lpmf(y | y, log_ye,
              prior_mean, K_function, (x, n_obs, alpha, rho), theta_0, tolerance,
              max_num_steps, hessian_block_size, solver,
              max_steps_line_search);

  target += laplace_marginal_tol_neg_binomial_2_log_lupmf(y | y, log_ye,
              prior_mean, K_function, (x, n_obs, alpha, rho), theta_0, tolerance,
              max_num_steps, hessian_block_size, solver,
              max_steps_line_search);
}
generated quantities {
  vector[n_obs] theta = laplace_latent_neg_binomial_2_log_rng(y, y, log_ye,
                          prior_mean, K_function, (x, n_obs, alpha, rho));

  vector[n_obs] theta2 = laplace_latent_tol_neg_binomial_2_log_rng(y, y,
                           log_ye, prior_mean, K_function,
                           (x, n_obs, alpha, rho), theta_0, tolerance, max_num_steps,
                           hessian_block_size, solver, max_steps_line_search);
}
