// A profile block writes `v[n]` and the next statement reads `v[n]` in a
// density. The write inside the profile block still counts as a write of `v`
// in the loop.
// Desired outcome: left as a loop.
// The pass does not widen a container that the same loop both writes and
// reads, so neither statement is rewritten.
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
