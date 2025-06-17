functions {
  // specify negative binomial likelihood with mean offset
  real ll_function(vector theta, // latent Gaussian
                   real eta,
                   vector log_ye, // mean offset
                   array[] int y) {
    // observed count
    return neg_binomial_2_lpmf(y | exp(log_ye + theta), eta);
  }
}

data {
  int n_obs;
  int n_coordinates;
  array[n_obs] int y;
  vector[n_obs] ye;
  array[n_obs] vector[n_coordinates] x;
}

transformed data {
  vector[n_obs] log_ye = log(ye);
}
parameters {
  real<lower=0> alpha;
  real<lower=0> rho;
  real<lower=0> eta;
}
model {
  target += laplace_marginal(ll_function, (eta, log_ye, y));

}

