// A later write that replaces the same element removes the dependence on a.
// Only the condition on y[n] should warn, and the prior count of a should be 2.
data {
  int N;
}
parameters {
  real a;
}
model {
  vector[N] theta;
  vector[N] y;
  theta[1] = a;
  theta[1] = 1;
  // no warning, because theta[1] = 1 replaces theta[1] = a
  if (theta[1] > 0) {
    target += 1;
  }
  for (n in 2:N) {
    theta[n] = a;
    theta[n] = 1;
    // no warning, because theta[n] = 1 replaces theta[n] = a
    if (theta[n] > 0) {
      target += 1;
    }
  }
  for (n in 2:N) {
    theta[n] = 1;
    theta[n] = a;
    y[n] = theta[n - 1];
    // warning, because theta[n - 1] = a ran in the previous iteration
    if (y[n] > 0) {
      target += 1;
    }
  }
  a ~ normal(0, 1);
}
