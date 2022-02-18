parameters {
  real y;
}
model {
  log(y) ~ normal(0,1);
  // triggers the warning but isn't strictly a ~ statement
  target += normal_lpdf(log(y) | 0, 1);
}
