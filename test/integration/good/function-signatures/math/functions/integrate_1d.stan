functions {
  real integrand(real x, real xc, array[] real theta, array[] real x_r,
                 array[] int x_i) {
    return 0.0;
  }
}
transformed data {
  array[0] real x_r;
  array[0] int x_i;
}
parameters {
  array[1] real x;
}
model {
  real y = integrate_1d(integrand, 0, 1, x, x_r, x_i);
  real z = integrate_1d(integrand, 0, 1, x, x_r, x_i, 1e-8);
  
  x ~ normal(y + z, 1.0);
}

