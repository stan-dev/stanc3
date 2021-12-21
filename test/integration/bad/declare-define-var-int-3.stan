transformed data {
  array[2] int td_a1 = 1;   // array = scalar - bad
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
