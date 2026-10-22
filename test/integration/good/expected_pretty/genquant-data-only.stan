/* $ stanc --auto-format ../genquant-data-only.stan : [exit 0] */
functions {
  real f(data real x) {
    return x + 1;
  }
}
parameters {
  real x;
}
generated quantities {
  real y = f(x);
}

