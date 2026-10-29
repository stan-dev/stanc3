// A mass function over an integer array outcome,
// `y[n] ~ bernoulli_logit(eta[n])`. The int array widens to the whole array
// and the vector parameter to the whole vector.
// Desired outcome: fully vectorized.
//   `target += bernoulli_logit_lupmf(y, eta)`
data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> y;
}
parameters {
  vector[N] eta;
}
model {
  for (n in 1 : N) {
    y[n] ~ bernoulli_logit(eta[n]);
  }
}
