transformed data {
  cholesky_factor_corr[2] x;
  {
    cholesky_factor_corr[2] y = x;
    print(y);
  }
}
