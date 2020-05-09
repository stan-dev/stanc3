functions {
  real foo(real x);
  real bar(real y) {
    functions
    real foo(real z) {
      return z - 1;
    }
    return foo(y);
  }
  real foo(real x) {
    return x + 1;
  }
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}