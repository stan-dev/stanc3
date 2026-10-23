/* $ stanc --auto-format ../var-ending-lpdf.stan : [exit 0] */
parameters {
  real mu_lpdf;
}
model {
  target += mu_lpdf;
}

