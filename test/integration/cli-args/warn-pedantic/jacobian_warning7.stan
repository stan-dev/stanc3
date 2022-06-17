parameters {
  real y;
}
model {
  [log(y)] ~ normal(0,1);
  log([y]) ~ normal(0,1);
}
