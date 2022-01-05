parameters {
  array[3] real y;

}
transformed parameters {
  cholesky_factor_cov[3, size(y)] z;
}
