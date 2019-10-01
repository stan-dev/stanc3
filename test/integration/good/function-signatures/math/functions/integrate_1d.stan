functions {
  real integrand(real x, real xc, real[] theta, real[] x_r, int[] x_i) {
    return 0.0;
  }
}

transformed data {
  real x_r[0];
  int x_i[0];
}

parameters {
  real x[1];
}

model {
  real y = integrate_1d(integrand, 0, 1, x, x_r, x_i);
  real z = integrate_1d(integrand, 0, 1, x, x_r, x_i, 1e-8);

  x ~ normal(y + z, 1.0);
}
