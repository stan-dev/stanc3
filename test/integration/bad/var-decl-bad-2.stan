parameters {
  real x[3];
  real y[poisson_rng(3)];
}
model {
  y ~ normal(0,1);
}
