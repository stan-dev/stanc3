// theta[1] = a and theta[2] = 1, so only the condition on theta[1] should warn.
// The prior count of a should be 2.
parameters {
  real a;
}
model {
  vector[2] theta;
  theta[1] = a;
  theta[2] = 1;
  // no warning, because theta[2] = 1 does not depend on a
  if (theta[2] > 0) {
    target += 1;
  }
  // warning, because theta[1] = a
  if (theta[1] > 0) {
    target += 1;
  }
  a ~ normal(0, 1);
}
