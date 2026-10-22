/* $ stanc --auto-format ../reject_func_call_generated_quantities.stan : [exit 0] */
functions {
  void foo(real x) {
    reject("user-specified rejection");
  }
}
parameters {
  real y;
}
model {
  y ~ normal(0, 1);
}
generated quantities {
  foo(y);
}

