  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//parser-generator/local_var_function_body.stan"
  functions {
    void foo() {
      int a;
      real b;
      real c[20, 30];
      matrix[40, 50] ar_mat[60, 70];
      ar_mat[1, 1, 1, 1] = b;
    }
  }
  

