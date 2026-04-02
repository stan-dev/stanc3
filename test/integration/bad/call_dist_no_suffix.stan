parameters {
  real x;
}
model {
  target += std_normal(x);
}