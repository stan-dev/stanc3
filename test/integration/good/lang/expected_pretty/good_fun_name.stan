/* $ stanc --auto-format ../good_fun_name.stan : [exit 0] */
parameters {
  real e;
  real pi;
  real sqrt2;
  real log2;
  real log10;
  real not_a_number;
  real positive_infinity;
  real negative_infinity;
  real epsilon;
  real negative_epsilon;
}
model {
  e ~ normal(0, 1);
}

