/* $ stanc --auto-format ../keywords-for.stan : [exit 0] */
parameters {
  real force;
}
model {
  force ~ normal(0, 1);
}

