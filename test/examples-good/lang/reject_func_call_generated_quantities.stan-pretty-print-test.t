  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//lang/reject_func_call_generated_quantities.stan"
  functions {
    void foo(real x) {
      reject("user-specified rejection");
    }
  }
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
  }
  generated quantities {
    foo(y);
  }
  

