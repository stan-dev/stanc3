data {
  array[3,3] real y;
}
transformed data {
  array[5] real z;
  z = y;
}
parameters {
  real x;
}
model {
  x ~ normal(0, 1);
}

