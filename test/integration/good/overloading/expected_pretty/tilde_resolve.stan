/* $ stanc --auto-format ../tilde_resolve.stan : [exit 0] */
// different than test/bad's no_minimum_tilde because of lpdf/lpmf checks

functions {
  real foo_lpdf(real x, int y) {
    return y;
  }
  
  real foo_lpmf(int x, real y) {
    return x / y;
  }
}
model {
  1 ~ foo(3);
}

