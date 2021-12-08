parameters {
  array[3] real x;
  real y[size(x)];
}
model {
  y ~ normal(0,1);
}
