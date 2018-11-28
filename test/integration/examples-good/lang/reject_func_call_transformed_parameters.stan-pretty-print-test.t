  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//lang/reject_func_call_transformed_parameters.stan"
  functions {
    void foo_bar() {
      reject("user-specified rejection");
    }
  }
  parameters {
    real y;
  }
  transformed parameters {
    real x;
    foo_bar();
  }
  model {
    y ~ normal(0, 1);
  }
  

