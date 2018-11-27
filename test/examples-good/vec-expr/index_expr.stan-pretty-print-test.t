  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//vec-expr/index_expr.stan"
  functions {
    void foo(int N) {
      int c[4];
      int d[3];
      c[2 : 10] = d;
    }
  }
  model {
  
  }
  

