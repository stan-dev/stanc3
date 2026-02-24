data {
  int n_obs;
  array[n_obs] int y;
}

transformed data {
  vector[n_obs] theta_0 = rep_vector(0.0, n_obs); // initial guess

}
parameters {
  real<lower=0> alpha;
  real<lower=0> rho;
  real<lower=0> eta;
}

generated quantities {

  vector[n_obs] theta = laplace_latent_neg_binomial_2_log_rng(y, {1}, [1.0]', [0.0]'
                      );

}
