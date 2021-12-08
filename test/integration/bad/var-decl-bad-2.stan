parameters {
  array[3] real x;
  real y[poisson_rng(3)];
}
model {
  y ~ normal(0,1);
}
