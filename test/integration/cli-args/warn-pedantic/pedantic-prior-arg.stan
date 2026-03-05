parameters {
  real x;
  real sigma;
}
model {
  x ~ normal(0, sigma);
  sigma ~ exponential(1);
}
