parameters {
  real y;
}
model {
  array[3] int int_1_a;
  int_1_a = { { 1*1 } };  // dim mismatch
  y ~ normal(0,1);
}
