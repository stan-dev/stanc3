data {
  int K;
}
parameters {
  cholesky_factor_corr[K] L;
}
model {
   L ~ lkj_corr_cholesky(2.0);
}