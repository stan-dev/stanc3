/* $ stanc --auto-format ../illegal_generated_quantities.stan : [exit 0] */
parameters {
  real y;
}
model {
  y ~ normal(0, 1);
}
generated quantities {
  real<lower=0> x;
  x = -1;
}

