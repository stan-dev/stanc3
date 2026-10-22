/* $ stanc --auto-format ../validate_modulus_good.stan : [exit 0] */
data {
  int i;
  int j;
}
transformed data {
  int k;
  k = i % j; // int, int
}
parameters {
  real y;
}
model {
  int i2;
  int j2;
  int k2;
  k2 = i2 % j2;
  y ~ normal(0, 1);
}
generated quantities {
  int i3;
  int j3;
  int k3;
  k3 = i3 % j3;
}

