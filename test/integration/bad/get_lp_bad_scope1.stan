transformed data {
  real y;
  y = target();
}
parameters {
  real z;
}
model {
  z ~ normal(0,1);
}
