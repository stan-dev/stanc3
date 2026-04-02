functions {
  real foo_lpmf( ) {
    return -2;
  }
}
parameters {
  real y;
}
model {
  target += foo_lpmf();
}
