parameters {
  real y;
}
model {
  functions
  real foo_rng(real x) {
      return x + 1;
  }
  y ~ normal(0,1);
}