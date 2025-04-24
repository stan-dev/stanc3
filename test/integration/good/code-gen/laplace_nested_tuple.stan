functions {
  matrix K_function(array[] vector x, tuple(int, tuple(real,real)) p) {
    matrix[p.1, p.1] K = gp_exp_quad_cov(x, p.2.1, p.2.2);
    for (i in 1 : p.1)
      K[i, i] += 1e-8;
    return K;
  }

  matrix K_function(tuple(array[] vector, array[] vector) x, int n_obs, real alpha, real rho) {
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
  y ~ laplace_marginal_bernoulli_logit(y, theta_0, K_function,
        (x, (n_obs, (alpha, rho))));

  y ~ laplace_marginal_neg_binomial_2_log(y, ye, theta_0, K_function,
        ((x, x), n_obs, alpha, rho));
}
