// The loop assigns `v[n] = x[n] ^ w[n]` with both operands indexed by the loop
// variable. The power operator `^` has a vector signature, so the widened
// operands keep `^` and the result is a vector.
// Desired outcome: fully vectorized.
//   `v[1:N] = x[1:N] ^ w[1:N]`
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] w;
}
parameters {
  real<lower=0> sigma;
}
model {
  vector[N] v;
  for (n in 1 : N) {
    v[n] = x[n] ^ w[n];
  }
  target += normal_lpdf(v | 0, sigma);
}
