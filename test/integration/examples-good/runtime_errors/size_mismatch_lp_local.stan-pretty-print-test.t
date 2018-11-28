  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//runtime_errors/size_mismatch_lp_local.stan"
  parameters {
    real x;
  }
  model {
    matrix[2, 3] z = [[1, 2, 3, 4], [4, 5, 6, 8]];
    x ~ normal(0, 5);
  }
  

