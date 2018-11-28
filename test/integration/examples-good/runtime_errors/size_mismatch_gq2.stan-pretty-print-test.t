  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//runtime_errors/size_mismatch_gq2.stan"
  generated quantities {
    matrix[2, 3] z = [[1, 2, 3], [4, 5, 6]];
    matrix[2, 2] ident = [[1, 0], [0, 1]];
    z += ident;
  }
  

