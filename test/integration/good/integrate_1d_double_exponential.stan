functions {
  real integrand(real x, real xc, vector theta, matrix alpha) {
    return 0.0;
  }
}

parameters {
  vector[10] theta;
  matrix[2,4] alpha;
}
model {
  real y = integrate_1d_double_exponential(integrand, 0, 1, theta, alpha);
  real z =  integrate_1d_double_exponential_tol(integrand, 0, 1, 1e-8, 0.0, 1, theta, alpha);

  y + z ~ normal(0, 1.0);
}
