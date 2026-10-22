/* $ stanc --auto-format ../funs5.stan : [exit 0] */
functions {
  void foo(real x) {
    print("x=", x);
  }
}
parameters {
  real y;
}
model {
  foo(y);
  y ~ normal(0, 1);
}

