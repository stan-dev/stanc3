parameters {
  array[3] real x;
  array[size(x)] real y;
}
model {
  y ~ normal(0,1);
}
