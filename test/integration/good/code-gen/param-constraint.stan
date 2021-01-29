data {
  int nt;
  int NS;
}

parameters {
  cholesky_factor_corr[2] L_Omega[nt];
  vector<lower=L_Omega[1,1,2]>[NS] z1;
}