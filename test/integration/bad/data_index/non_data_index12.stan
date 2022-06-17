parameters {
  array[3] real y;

}
transformed parameters {
  cholesky_factor_corr[size(y)] z;
}
