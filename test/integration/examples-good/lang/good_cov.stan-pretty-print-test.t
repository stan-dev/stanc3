  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//lang/good_cov.stan"
  data {
    vector[5] y[10];
    vector[5] mu[10];
  }
  parameters {
    cov_matrix[5] Sigma[10];
  }
  model {
    for (i in 1 : 10) y[i] ~ multi_normal(mu[i], Sigma[i]);
  }
  

