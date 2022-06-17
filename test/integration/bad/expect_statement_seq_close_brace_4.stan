parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
generated quantities {
  array[2,3] int vs;

