transformed data {
  int n;
  n = !n;
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
