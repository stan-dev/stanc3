// A density whose arguments do not depend on the loop variable. Every
// iteration adds the same value, so the loop becomes one call scaled by the
// number of iterations.
// Desired outcome: fully vectorized.
//   `target += (N - (1 - 1)) * normal_lpdf(0.5, mu, sigma)`
data {
  int<lower=0> N;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  for (n in 1 : N) {
    target += normal_lpdf(0.5 | mu, sigma);
  }
}
