data {
  array[2] real a1;
  real a2 = 1.0; // cannot assign in data block
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}

