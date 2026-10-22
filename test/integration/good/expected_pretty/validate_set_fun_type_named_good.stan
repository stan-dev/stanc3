/* $ stanc --auto-format ../validate_set_fun_type_named_good.stan : [exit 0] */
transformed data {
  real x;
  x = exp(x);
}
parameters {
  real y;
}
model {
  y ~ normal(0, 1);
}

