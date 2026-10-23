/* $ stanc --auto-format ../dataonly-int-return.stan : [exit 0] */
parameters {
  vector[5] x;
}
model {
  x ~ normal(0, linspaced_array(size(x), 1.0, 10.0));
}

