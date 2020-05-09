parameters {
  real y;
}
model {
  functions
  real foo(real x);
  functions
  real foo(real x) {
      return x + 1;
  }
  y ~ normal(0,1);
}