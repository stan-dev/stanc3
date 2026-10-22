/* $ stanc --auto-format ../keywords-return.stan : [exit 0] */
parameters {
  real returning;
}
model {
  returning ~ normal(0, 1);
}

