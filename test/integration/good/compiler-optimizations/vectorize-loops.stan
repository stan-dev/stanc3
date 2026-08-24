data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
  array[N] int<lower=0, upper=1> z;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
  vector[N] eta;
}
model {
  vector[N] mu = alpha + beta * x;
  for (n in 1 : N) {
    target += normal_lpdf(y[n] | mu[n], sigma);
  }
  for (n in 1 : N) {
    z[n] ~ bernoulli_logit(eta[n]);
  }
  for (n in 3 : N) {
    y[n] ~ normal(alpha, sigma);
  }
}
