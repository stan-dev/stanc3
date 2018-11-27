  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//lang/reject_func_call_model.stan"
  functions {
    void foo(real x) {
      reject("user-specified rejection");
    }
  }
  parameters {
    real y;
  }
  model {
    foo(y);
    y ~ normal(0, 1);
  }
  

