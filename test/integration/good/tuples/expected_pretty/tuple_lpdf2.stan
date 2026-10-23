/* $ stanc --auto-format ../tuple_lpdf2.stan : [exit 0] */
functions {
  real foo_lpdf(tuple(real, int) x) {
    return 0;
  }
}
model {
  (1.5, 2) ~ foo();
}

