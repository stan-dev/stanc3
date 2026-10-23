/* $ stanc --auto-format ../test_transform_inits.stan : [exit 0] */
parameters {
  real<lower=0> nu;
  real<lower=0, upper=(nu == 0 ? 1 : positive_infinity())> lambda;
}

