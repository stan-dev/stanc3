transformed data {
  vector[5] y;
  array[5] real z;
  z = y;
}
parameters {
  real x;
}
model {
  x ~ normal(0, 1);
}
