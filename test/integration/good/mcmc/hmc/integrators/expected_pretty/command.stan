/* $ stanc --auto-format ../command.stan : [exit 0] */
data {
  real y;
}
parameters {
  real mu;
}
model {
  y ~ normal(mu, 1);
}

