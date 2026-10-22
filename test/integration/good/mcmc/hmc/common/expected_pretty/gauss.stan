/* $ stanc --auto-format ../gauss.stan : [exit 0] */
parameters {
  real x;
}
model {
  x ~ normal(0, 1);
}

