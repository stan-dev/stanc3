/**
 * this one's for issue #1768, where there was a duplicate fun decl
 * because of the <false> instantiation of propto
 */
functions {
  real n_lpdf(real y);
  
  real n_lpdf(real y) {
    return -0.5 * square(y);
  }
}
parameters {
  real mu;
}
model {
  mu ~ n();
  target += n_lpdf(mu | ); // check both instantiations
}

