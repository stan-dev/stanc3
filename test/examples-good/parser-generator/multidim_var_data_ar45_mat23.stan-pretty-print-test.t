  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//parser-generator/multidim_var_data_ar45_mat23.stan"
  data {
    matrix<lower=0, upper=1>[2, 3] ar_mat[4, 5];
  }
  transformed data {
    for (i in 1 : 4) {
      for (j in 1 : 5) {
        matrix[2, 3] foo = ar_mat[i, j];
        print("ar dim1: ", i, " ar dim2: ", j, " matrix: ", foo);
      }
    }
  }
  

