parameters {
  real x;
  real y;
}
model {
  x ~ normal(0, 1);
  y ~ normal(1, 1);
  x ~ normal(y, 1);
}
