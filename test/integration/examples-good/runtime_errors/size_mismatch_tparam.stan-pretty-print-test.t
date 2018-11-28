  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//runtime_errors/size_mismatch_tparam.stan"
  transformed data {
    matrix[2, 3] td_m23 = [[1, 2, 3], [4, 5, 6]];
  }
  transformed parameters {
    matrix[2, 3] tp_m23 = [[1, 2, 3, 4], [4, 5, 6, 8]];
  }
  

