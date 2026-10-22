/* $ stanc --auto-format ../infer_tuple_ad.stan : [exit 0] */
parameters {
  real x;
}
model {
  array[2] tuple(real, real) z = {(1, x), (x, 1)};
}

