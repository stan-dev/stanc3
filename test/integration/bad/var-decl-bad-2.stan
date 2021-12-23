parameters {
  array[3] real x;
  array[poisson_rng(3)] real y;
}
model {
  y ~ normal(0,1);
}
