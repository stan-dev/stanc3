parameters {
  real y;
}
model {
  real x = 0.0;
  functions
  real foo(real z) {
      x = 2;
      return x + z;
  }
  y ~ normal(0,1);
}