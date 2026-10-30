// A density with a gather `alpha[county[n]]` in a loop that starts at 2.
// The outcome and the index array are both sliced to the iterated range.
// Desired outcome: fully vectorized.
//   `target += normal_lpdf(y[2:N], alpha[county[2:N]], sigma)`
data {
  int<lower=0> N;
  int<lower=1> J;
  array[N] int<lower=1, upper=J> county;
  vector[N] y;
}
parameters {
  vector[J] alpha;
  real<lower=0> sigma;
}
model {
  for (n in 2 : N) {
    target += normal_lpdf(y[n] | alpha[county[n]], sigma);
  }
}
