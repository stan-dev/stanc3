/* $ stanc --auto-format ../print_reject_tparams_block.stan : [exit 0] */
transformed data {
  print("In transformed data");
}
parameters {
  real y;
}
transformed parameters {
  print("In transformed parameters");
  print("quitting time");
  reject("QUIT");
}
model {
  print("In model block.");
  y ~ normal(0, 1);
}
generated quantities {
  print("In generated quantities");
}

