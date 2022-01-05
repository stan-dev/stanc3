transformed data {
  int tx;
  real ty;
  array[2,2] row_vector[6] twa2;
  tx = twa2 ? 2 : 3;   // BAD
}
parameters {
  real py;
}
model {
  py ~ normal(0,1);
}
