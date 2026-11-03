// A later write that always replaces an element removes the earlier write.
// A loop-variable index counts only within one iteration.
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
  if (theta[1] > 0) target += 1;    // no warning, because theta[1] = 1 replaces theta[1] = a
  for (n in 2:N) {
    theta[n] = a;
    theta[n] = 1;
    if (theta[n] > 0) target += 1;  // no warning, because theta[n] = 1 replaces theta[n] = a
  }
  for (n in 2:N) {
    theta[n] = 1;
    theta[n] = a;
    y[n] = theta[n - 1];
    if (y[n] > 0) target += 1;      // warning, because theta[n - 1] = a in the previous iteration
  }
  a ~ normal(0, 1);
}
