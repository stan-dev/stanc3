data {
 complex<offset=1> x;
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
