// The loop assigns `v[n] = x[n] + bump_lp(sigma)`, where `bump_lp` adds to
// `target` on every call. A vector statement would call `bump_lp` once instead
// of N times.
// Desired outcome: left as a loop.
// Expressions with side effects cannot be duplicated or merged, so the
// assignment stays.
functions {
  real bump_lp(real x) {
    target += x;
    return x;
  }
}
data {
  int<lower=0> N;
  vector[N] x;
}
parameters {
  real<lower=0> sigma;
}
model {
  vector[N] v;
  for (n in 1 : N) {
    v[n] = x[n] + bump_lp(sigma);
  }
  target += normal_lpdf(v | 0, sigma);
}
