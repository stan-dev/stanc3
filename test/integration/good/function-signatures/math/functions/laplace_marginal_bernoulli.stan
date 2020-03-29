functions {
  matrix K (vector phi, vector[] x, real[] delta, int[] delta_int) {
    matrix[1, 1] covariance;
    return covariance;
  }

  matrix Km (vector phi, matrix x, real[] delta, int[] delta_int) {
    matrix[1, 1] covariance;
    return covariance;
  }
}

transformed data {
  int y[1];
  int n_samples[1];

  vector[1] phi;
  vector[1] x[1];
  matrix[1, 1] x_m;
  real delta[1];
  int delta_int[1];

  vector[1] theta0;
}

parameters {
  vector[1] phi_v;
  vector[1] theta0_v;
}

model {
  target +=
    laplace_marginal_bernoulli(y, n_samples, K, phi, x, delta, delta_int,
                               theta0);
  target +=
    laplace_marginal_bernoulli(y, n_samples, Km, phi, x_m, delta, delta_int,
                               theta0);
}

generated quantities {
  vector[1] theta_pred
    = laplace_approx_bernoulli_rng(y, n_samples, K, phi, x, delta, delta_int,
                                   theta0);
  theta_pred = laplace_approx_bernoulli_rng(y, n_samples, Km, phi, x_m, delta,
                                            delta_int, theta0);
}
