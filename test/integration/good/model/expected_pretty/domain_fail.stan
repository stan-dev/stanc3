/* $ stanc --auto-format ../domain_fail.stan : [exit 0] */
parameters {
  real<lower=0> x;
}
model {
  target += -sqrt(-x);
}

