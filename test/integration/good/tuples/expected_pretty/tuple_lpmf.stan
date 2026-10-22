/* $ stanc --auto-format ../tuple_lpmf.stan : [exit 0] */
functions {
  real foo_lpmf(tuple(int, int) x, real y) {
    return x.1 + x.2 * y;
  }
}
model {
  (1, 3) ~ foo(0.5);
}

