  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//lang/good_local_var_array_size.stan"
  data {
    int M[5];
  }
  transformed data {
    int N[2];
    N[1] = 1;
    N[2] = 4;
  }
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
    for (i in 1 : 10) {
      real x[i];
      for (j in 1 : i) x[j] = j * j;
    }
    for (i in 1 : 5) {
      vector[i] v;
      row_vector[i] rv;
      for (j in 1 : 10) {
        matrix[i, j] m;
      }
    }
    for (i in 1 : 5) {
      real x[M[i]];
    }
    for (i in 1 : 2) {
      real x[N[i]];
    }
  }
  

