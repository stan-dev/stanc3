functions {
  real foo_cdf(real a, real b, real c) {
    return a + b + c;
  }
}
model {
  target += foo_cdf(1, 2, 3);
}
