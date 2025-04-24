functions {
  matrix scalar_tuple(array[] vector x, tuple(int, tuple(real, real)) p) {
    matrix[p.1, p.1] K = gp_exp_quad_cov(x, p.2.1, p.2.2);
    for (i in 1 : p.1) 
      K[i, i] += 1e-8;
    return K;
  }
  
  matrix arr_vec_tuple(tuple(array[] vector, array[] vector) x, int n_obs,
                       real alpha, real rho) {
    matrix[n_obs, n_obs] K = gp_exp_quad_cov(x.1, alpha, rho);
    for (i in 1 : n_obs) 
      K[i, i] += 1e-8;
    return K;
  }
  
  matrix arr_and_vec_tuple(tuple(array[] vector, vector) x, int n_obs,
                           real alpha, real rho) {
    matrix[n_obs, n_obs] K = gp_exp_quad_cov(x.1, alpha, rho);
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
  vector[n_obs] theta_0 = rep_vector(0.0, n_obs); // initial guess
}
parameters {
  real<lower=0> alpha;
  real<lower=0> rho;
  real<lower=0> eta;
}
model {
  y ~ laplace_marginal_neg_binomial_2_log(y, ye, theta_0, scalar_tuple,
        (x, (n_obs, (alpha, rho))));
  
  y ~ laplace_marginal_neg_binomial_2_log(y, ye, theta_0, arr_vec_tuple,
        ((x, x), n_obs, alpha, rho));
  
  y ~ laplace_marginal_neg_binomial_2_log(y, ye, theta_0, arr_and_vec_tuple,
        ((x, rep_vector(rho, 10)), n_obs, alpha, rho));
}
generated quantities {
  vector[n_obs] theta = laplace_latent_neg_binomial_2_log_rng(y, y, ye,
                          theta_0, scalar_tuple, (x, (n_obs, (alpha, rho))));
  
  vector[n_obs] theta2 = laplace_latent_neg_binomial_2_log_rng(y, y, ye,
                           theta_0, arr_vec_tuple,
                           ((x, x), n_obs, alpha, rho));
  
  vector[n_obs] theta3 = laplace_latent_neg_binomial_2_log_rng(y, y, ye,
                           theta_0, arr_and_vec_tuple,
                           ((x, rep_vector(rho, 10)), n_obs, alpha, rho));
}
