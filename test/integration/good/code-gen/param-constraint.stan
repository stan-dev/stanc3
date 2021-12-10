data {
  int nt;
  int NS;
}
parameters {
  array[nt] cholesky_factor_corr[2] L_Omega;
  vector<lower=L_Omega[1, 1, 2]>[NS] z1;
}

