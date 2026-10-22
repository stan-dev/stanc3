/* $ stanc --auto-format ../print_reject_model_block.stan : [exit 0] */
transformed data {
  print("In transformed data");
}
parameters {
  real y;
}
transformed parameters {
  print("In transformed parameters");
}
model {
  print("In model block.");
  y ~ normal(0, 1);
  print("quitting time");
  reject("user-specified rejection");
}
generated quantities {
  print("In generated quantities");
}

