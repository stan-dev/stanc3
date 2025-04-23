functions {
  matrix K_function(array[] vector x, tuple(int,real,real) p) {
    matrix[p.1, p.1] K = gp_exp_quad_cov(x, p.2, p.3);
    for (i in 1 : p.1)
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
        (x, (n_obs, alpha, rho)));
}
