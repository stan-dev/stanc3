// A tilde `y[n] ~ normal(mu, sigma)` where `mu` is a whole vector that does
// not depend on the loop variable. Each iteration sums over all of `mu`.
// Desired outcome: left as a loop.
// A vectorized call would pair `mu` with `y` element by element, which also
// typechecks but computes a different value.
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
    y[n] ~ normal(mu, sigma);
  }
}
