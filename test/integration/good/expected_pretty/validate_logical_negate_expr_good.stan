/* $ stanc --auto-format ../validate_logical_negate_expr_good.stan : [exit 0] */
transformed data {
  int n;
  n = !n;
}
parameters {
  real y;
}
model {
  y ~ normal(0, 1);
}

