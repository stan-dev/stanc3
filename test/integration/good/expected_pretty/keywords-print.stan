/* $ stanc --auto-format ../keywords-print.stan : [exit 0] */
parameters {
  real printer;
}
model {
  printer ~ normal(0, 1);
}

