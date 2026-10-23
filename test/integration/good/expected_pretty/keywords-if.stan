/* $ stanc --auto-format ../keywords-if.stan : [exit 0] */
parameters {
  real iffy;
}
model {
  iffy ~ normal(0, 1);
}

