/* $ stanc --auto-format ../reject_transformed_data.stan : [exit 0] */
transformed data {
  real<lower=0> x;
  reject("user-specified rejection");
}
parameters {
  real y;
}
model {
  y ~ normal(0, 1);
}
generated quantities {
  print("generating quantities");
}

