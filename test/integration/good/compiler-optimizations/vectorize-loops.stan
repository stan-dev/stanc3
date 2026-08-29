data {
  int<lower=0> N;
  int<lower=1> J;
  vector[N] x;
  vector[N] y;
  array[N] int<lower=0, upper=1> z;
  array[N] int<lower=1, upper=J> county;
}
transformed data {
  int M = N;
  vector[M] copied = x;
  M = N - 1;
  vector[M] shorter;
  for (n in 1 : M) {
    shorter[n] = copied[n];
  }
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
  vector[N] eta;
  vector[J] a;
}
model {
  vector[N] mu;
  vector[N] shrunk;
  vector[N] scratch;
  vector[N] walk;
  for (n in 1 : N) {
    mu[n] = alpha + beta * x[n];
  }
  for (n in 1 : N) {
    shrunk[n] = a[county[n]] + mu[n] / 2;
  }
  for (n in 1 : N) {
    target += normal_lpdf(y[n] | mu[n], sigma);
  }
  for (n in 1 : N) {
    z[n] ~ bernoulli_logit(eta[n]);
  }
  for (n in 3 : N) {
    y[n] ~ normal(alpha, sigma);
  }
  for (n in 1 : N) {
    y[n] ~ normal(a[county[n]], sigma);
  }
  for (n in 1 : N) {
    scratch[n] = alpha + eta[n];
    walk[n] = beta * x[n];
  }
  for (n in 1 : N) {
    profile("vectorized") {
      y[n] ~ normal(scratch[n], sigma);
    }
  }
  for (n in 1 : N) {
    scratch[n] = alpha + eta[n];
    target += normal_lpdf(y[n] | scratch[n], sigma);
  }
  for (n in 2 : N) {
    walk[n] = walk[n - 1] + eta[n];
  }
  for (n in 1 : N) {
    target += normal_lpdf(y[n] | mu, sigma);
  }
  y ~ normal(shrunk, sigma);
}
