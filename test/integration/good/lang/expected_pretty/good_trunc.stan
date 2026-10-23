/* $ stanc --auto-format ../good_trunc.stan : [exit 0] */
data {
  real y;
}
model {
  y ~ normal(0, 1) T[-1, 1];
  y ~ normal(0, 1) T[0, ];
  y ~ normal(0, 1) T[ , 0];
  y ~ normal(0, 1);
}

