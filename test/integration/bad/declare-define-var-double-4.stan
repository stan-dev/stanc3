data {
  array[2] real a1;
}
parameters {
  real y;
  real a2 = 1.0; // cannot assign in parameters block
}
model {
  y ~ normal(0,1);
}

