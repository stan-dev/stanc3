/* $ stanc --auto-format ../multi_fun.stan : [exit 0] */
functions {
  int foo(int a) {
    return a;
  }
  real foo(real b) {
    return b;
  }
}
parameters {
  real y;
}
model {
  y ~ normal(0, 1);
}

