parameters {
  real y;
}
model {
  array[5] int int_1_a;
  int_1_a = { 1.0, 2.0, 3.0, 4.0 , 5.0 };  // type mismatch
  y ~ normal(0,1);
}
