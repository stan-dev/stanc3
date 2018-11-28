  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//parser-generator/local_var_transformed_data_block.stan"
  transformed data {
    int a1;
    real d1;
    {
      int a2;
      real d2;
      real b[20, 30];
      matrix[40, 50] ar_mat[60, 70];
      ar_mat[1, 1, 1, 1] = 1.0;
      a1 = a2;
      d1 = b[1, 1];
    }
  }
  

