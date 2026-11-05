// An assignment loop `mu[n] = alpha + beta * x[n]` that mixes scalar
// parameters with a data vector. The scalars broadcast and the arithmetic
// widens to whole-vector operations.
// Desired outcome: fully vectorized.
//   `mu[1:N] = alpha + beta * x`
data {
  int<lower=0> N;
  vector[N] x;
}
parameters {
  real alpha;
  real beta;
}
model {
  vector[N] mu;
  for (n in 1 : N) {
    mu[n] = alpha + beta * x[n];
  }
  target += normal_lpdf(mu | 0, 1);
}
