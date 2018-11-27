  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//funs3.stan"
  functions {
    real unit_normal_log(real y) {
      return normal_log(y, 0, 1);
    }
  }
  parameters {
    real y;
  }
  model {
    y ~ unit_normal();
  }
  

