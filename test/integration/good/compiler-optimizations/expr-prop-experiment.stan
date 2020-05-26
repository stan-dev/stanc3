data {
  int j;
}
transformed data {
  real z;
  real x;
  real y;
  real i = normal_rng(5, 1);
  z = i * j; 
  x = normal_rng(z, 1);
  i = normal_rng(5, 1);
  y = normal_rng(z, 1);
}
model {
}
