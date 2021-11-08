data {
  int K;
}
parameters {
  cholesky_factor_corr[K] L;
}
model {
   target += lkj_corr_cholesky_lpdf(L|2.5);
}