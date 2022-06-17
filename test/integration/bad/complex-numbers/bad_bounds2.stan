data {
  array[10] complex<lower=1> x;
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
