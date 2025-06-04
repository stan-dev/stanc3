functions {

  vector algebra_system(vector x, vector y, array[] real dat,
                        array[] int dat_int) {
    vector[2] f_x;
    f_x[1] = x[1] - y[1];
    f_x[2] = x[2] - y[2];
    return f_x;
  }


  real helper(real eta){
    array[3] real dat = {eta, 0.2, 0.3};
    dat[cols(algebra_solver(algebra_system, [eta]', [1]', {0.1}, {2}))] = 0.0;

    return dat[1];
  }
  // specify negative binomial likelihood with mean offset
  real ll_function(vector theta, // latent Gaussian
                   real eta,
                   vector log_ye, // mean offset
                   array[] int y) {

    // observed count
    return neg_binomial_2_lpmf(y | exp(log_ye + theta), eta) + helper(eta);
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
