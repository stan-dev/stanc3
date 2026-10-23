/* $ stanc --auto-format ../validate_int_expr2_good.stan : [exit 0] */
parameters {
  vector[10] y;
}
model {
  for (n in 1 : 10) 
    y ~ normal(0, 1);
}

