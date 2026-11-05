// A density loop that starts at 3 instead of 1. The vectorizer slices the
// outcome to the iterated range.
// Desired outcome: fully vectorized.
//   `target += normal_lpdf(y[3:N], mu, sigma)`
data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  for (n in 3 : N) {
    target += normal_lpdf(y[n] | mu, sigma);
  }
}
