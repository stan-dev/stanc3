/* $ stanc --auto-format ../illegal_transformed_parameters.stan : [exit 0] */
transformed parameters {
  real<lower=0> x;
  
  x = -1;
}
model {
  
}

