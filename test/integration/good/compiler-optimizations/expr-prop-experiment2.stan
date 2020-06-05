data {
  int j;
}
transformed data {
  real z = 1;
  real y;
  {
    real x = normal_rng(123, 1);
    z = x * 2;
    y = normal_rng(z, 1);
  }
  y = normal_rng(z, 1);
}
model {
}
