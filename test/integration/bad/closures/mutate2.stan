parameters {
  real y;
}
model {
  real x = 0.0;
  functions
  real foo(real z) {
      return x + z;
  }
  x = 2;
  y ~ normal(0,1);
}