// An assignment loop `v[n] = exp(x[n])` where both the target and `x` are
// real arrays. The widened right-hand side `exp(x)` is also an array. The
// sliced assignment therefore typechecks.
// Desired outcome: fully vectorized.
//   `v[1:N] = exp(x)`
data {
  int<lower=0> N;
  array[N] real x;
}
parameters {
  real<lower=0> sigma;
}
model {
  array[N] real v;
  for (n in 1 : N) {
    v[n] = exp(x[n]);
  }
  target += normal_lpdf(v | 0, sigma);
}
