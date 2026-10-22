/* $ stanc --auto-format ../funs6.stan : [exit 0] */
functions {
  real foo(real x) {
    return x * 2;
  }
}
parameters {
  real y;
}
model {
  foo(y) ~ normal(0, 2);
}

