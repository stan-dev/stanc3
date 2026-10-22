/* $ stanc --auto-format ../lp_transformed_param.stan : [exit 0] */
functions {
  real test_lp(real r) {
    target += normal_lpdf(r | 0, 1);
    return r;
  }
  void nr_test_lp(real r) {
    target += normal_lpdf(r | 0, 1);
  }
}
parameters {
  real y;
}
transformed parameters {
  real alpha = test_lp(5.0);
  nr_test_lp(5.0);
}
model {
  y ~ normal(0, 1);
}

/*
warning: Using _lp functions in transformed parameters is deprecated and will be disallowed in the future.
    ┌─ ../lp_transformed_param.stan:14:16
 13 │  transformed parameters { 
 14 │    real alpha = test_lp(5.0);
    │                 ^^^^^^^^^^^^ _lp function call in transformed parameters.
 15 │    nr_test_lp(5.0);
    = Use an _jacobian function instead, as this allows change of variable
      adjustments which are conditionally enabled by the
      algorithms.
warning: Using _lp functions in transformed parameters is deprecated and will be disallowed in the future.
    ┌─ ../lp_transformed_param.stan:15:3
 14 │    real alpha = test_lp(5.0);
 15 │    nr_test_lp(5.0);
    │    ^^^^^^^^^^^^^^^^ _lp function call in transformed parameters.
 16 │  }
    = Use an _jacobian function instead, as this allows change of variable
      adjustments which are conditionally enabled by the
      algorithms.
*/
