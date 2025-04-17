functions {
  // specify negative binomial likelihood with mean offset
  real ll_function(vector theta, // latent Gaussian
                   real eta,
                   vector log_ye, // mean offset
                   array[] int y) {
    // observed count
    return neg_binomial_2_lpmf(y | exp(log_ye + theta), eta);
  }

  // specify covariance function
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

  // real alpha = 1.0;
  // real rho = 1.0;
}
parameters {
  real<lower=0> alpha;
  real<lower=0> rho;
  real<lower=0> eta;
}
model {

  rho ~ inv_gamma(10, 1);
  alpha ~ inv_gamma(alpha_location_prior, alpha_scale_prior);
  eta ~ normal(0, 1);

  target += laplace_marginal(ll_function, (eta, log_ye, y),
                                  theta_0,
                                  K_function, (x, n_obs, alpha, rho));

}
generated quantities {

vector[n_obs] theta = laplace_marginal_rng(ll_function, (eta, log_ye, y),
      theta_0);



}
