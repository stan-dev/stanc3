/* $ stanc --auto-format ../reject_func_call_transformed_data.stan : [exit 0] */
functions {
  void foo(real x) {
    reject("user-specified rejection");
  }
}
transformed data {
  real<lower=0> x;
  foo(x);
}
parameters {
  real y;
}
model {
  y ~ normal(0, 1);
}

