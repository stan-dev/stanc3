/* $ stanc --auto-format ../funs7.stan : [exit 0] */
functions {
  real foo(real z) {
    real y;
    y = 1 ? z : 1;
    return y;
  }
}
parameters {
  real y;
}
transformed parameters {
  real z = foo(y);
}
model {
  y ~ normal(0, 1);
}

