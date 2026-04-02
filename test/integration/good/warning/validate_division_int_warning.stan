transformed data {
  real u;
  int j;
  int k;
  j = 2;
  k = 3;
  u = j / k;
  u = j / 3;
  u = 2 / k;
  u = 2 / 3;
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
