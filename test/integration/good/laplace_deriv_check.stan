functions {
  void helper(){
    if (0) {
      // a buggy implementation of the derivative check will loop forever here
      helper();
    }
  }

  // specify negative binomial likelihood with mean offset
  real ll_function(vector theta, // latent Gaussian
                   real eta,
                   vector log_ye, // mean offset
                   array[] int y) {
    // observed count
    helper();
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
}

transformed data {
  vector[n_obs] log_ye = log(ye);
  vector[n_obs] theta_0 = rep_vector(0.0, n_obs); // initial guess
}
parameters {
  real<lower=0> alpha;
  real<lower=0> rho;
  real<lower=0> eta;
}

generated quantities {
  vector[n_obs] theta = laplace_latent_rng(ll_function, (eta, log_ye, y),
                        theta_0,
                        K_function, (x, n_obs, alpha, rho));
}
