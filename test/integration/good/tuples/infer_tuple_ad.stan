parameters {
  real x;
}
model {
  array[2] tuple(real, real) z = {(1,x),(x,1)};
}
