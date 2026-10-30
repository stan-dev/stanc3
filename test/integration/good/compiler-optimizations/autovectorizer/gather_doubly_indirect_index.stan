// A density with a two-level gather `alpha[county[site[n]]]`. The innermost
// index array widens first and each outer index becomes a multi-index.
// Desired outcome: fully vectorized.
//   `target += normal_lpdf(y, alpha[county[site]], sigma)`
data {
  int<lower=0> N;
  int<lower=1> J;
  array[N] int<lower=1, upper=N> site;
  array[N] int<lower=1, upper=J> county;
  vector[N] y;
}
parameters {
  vector[J] alpha;
  real<lower=0> sigma;
}
model {
  for (n in 1 : N) {
    target += normal_lpdf(y[n] | alpha[county[site[n]]], sigma);
  }
}
