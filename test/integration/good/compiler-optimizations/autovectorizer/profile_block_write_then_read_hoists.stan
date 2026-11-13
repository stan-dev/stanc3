// A profile block writes `v[n]` and the next statement reads `v[n]` in a
// density. The profile block is one leaf of the loop dependence graph and the
// density another; the only dependence is a loop-independent true dependence
// from the write to the read.
// Dependences: true S1->S2 {Eq}
// Desired outcome: fully vectorized, in source order.
//   `profile("mu") { v[1:N] = x[1:N] + 1; }` then
//   `target += normal_lpdf(y[1:N] | v[1:N], sigma)`
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real<lower=0> sigma;
}
model {
  vector[N] v;
  for (n in 1 : N) {
    profile("mu") {
      v[n] = x[n] + 1;
    }
    target += normal_lpdf(y[n] | v[n], sigma);
  }
}
