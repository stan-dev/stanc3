parameters {
  real y;
}
model {
  array[3] int int_1_a;
  int_1_a = { };  // cannot be empty
  y ~ normal(0,1);
}
