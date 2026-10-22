/* $ stanc --auto-format ../assignment-new.stan : [exit 0] */
transformed data {
  real mu;
  real<lower=0> sigma;
  mu = -1;
  sigma = 3;
}
parameters {
  real y;
}
model {
  y ~ normal(mu, sigma);
}

