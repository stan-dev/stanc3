// A truncated tilde `y[n] ~ normal(mu, sigma) T[0, ]` lowers to a loop body
// with the density increment and an if/else that checks the bound and adds
// the normalizing term. The density statement vectorizes and the bounds check
// stays in a loop.
// Desired outcome: partially vectorized.
//   `target += normal_lupdf(y, mu, sigma)` followed by the bounds-check loop
data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  for (n in 1 : N) {
    y[n] ~ normal(mu, sigma) T[0, ];
  }
}
