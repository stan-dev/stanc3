// A density whose loop-invariant argument is a call to `bump_lp`. Each call
// to `bump_lp` adds to the target.
// Desired outcome: left as a loop.
// The vectorized call would evaluate `bump_lp` once instead of N times.
functions {
  real bump_lp(real x) {
    target += x;
    return x;
  }
}
data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real<lower=0> sigma;
}
model {
  for (n in 1 : N) {
    target += normal_lpdf(y[n] | bump_lp(sigma), sigma);
  }
}
