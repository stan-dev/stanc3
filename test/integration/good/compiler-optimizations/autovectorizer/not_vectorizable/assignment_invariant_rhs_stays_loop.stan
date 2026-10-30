// An assignment loop `v[n] = alpha` whose right-hand side does not depend on
// the loop variable.
// Desired outcome: left as a loop.
// The right-hand side does not widen. The pass then does not have a vector
// expression to assign to the slice.
data {
  int<lower=0> N;
}
parameters {
  real alpha;
}
model {
  vector[N] v;
  for (n in 1 : N) {
    v[n] = alpha;
  }
  target += normal_lpdf(v | 0, 1);
}
