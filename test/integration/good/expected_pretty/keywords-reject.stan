/* $ stanc --auto-format ../keywords-reject.stan : [exit 0] */
parameters {
  real rejection;
}
model {
  rejection ~ normal(0, 1);
}

