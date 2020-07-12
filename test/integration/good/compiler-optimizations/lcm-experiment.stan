data {
  int j;
}
transformed data {
  real z = 1;
  real x;
  {
    x = normal_rng(123, 1);
    z = normal_rng(sqrt(j)*2+1, 1);
  }
  real i = normal_rng(sqrt(j)*2+1, 1);
}
model {
}
