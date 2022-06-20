data {
  int<lower=0> N;
  vector[N] context;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  context ~ normal(mu,sigma);
}