/* $ stanc --auto-format ../reject_func_call_transformed_parameters.stan : [exit 0] */
functions {
  void foo_bar() {
    reject("user-specified rejection");
  }
}
parameters {
  real y;
}
transformed parameters {
  real x;
  foo_bar();
}
model {
  y ~ normal(0, 1);
}

