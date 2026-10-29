// A tilde statement `y[n] ~ normal(mu[n], sigma)` inside a loop. The loop
// lowers to `target +=` with the dropping-constants form of the density, and
// the vectorized call keeps that form.
// Desired outcome: fully vectorized.
//   `target += normal_lupdf(y, mu, sigma)`
data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  vector[N] mu;
  real<lower=0> sigma;
}
model {
  for (n in 1 : N) {
    y[n] ~ normal(mu[n], sigma);
  }
}
