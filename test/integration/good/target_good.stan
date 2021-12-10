/**
 * get_lp() allowed in:
 *  - functions ending in _lp
 *  transformed parameter block
 *  model block
 */
functions {
  // allowed in functions ending in _lp
  real foo_lp(real x) {
    return x + target();
  }
}
parameters {
  real y;
}
transformed parameters {
  real z;
  z = target();
}
model {
  real w;
  w = target();
  y ~ normal(0, 1);
}

