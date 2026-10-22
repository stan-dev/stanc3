/* $ stanc --auto-format ../validate_add_expression_dimss_good.stan : [exit 0] */
transformed data {
  real x;
  vector[3] v;
  x = v[1];
}
parameters {
  real y;
}
transformed parameters {
  real xt;
  vector[3] vt;
  xt = vt[1];
}
model {
  y ~ normal(0, 1);
}

