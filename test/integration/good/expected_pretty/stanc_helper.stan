/* $ stanc --auto-format ../stanc_helper.stan : [exit 0] */
parameters {
  real y;
}
model {
  y ~ normal(0, 1);
}

