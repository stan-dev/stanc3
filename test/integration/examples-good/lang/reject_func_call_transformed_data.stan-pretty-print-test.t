  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//lang/reject_func_call_transformed_data.stan"
  functions {
    void foo(real x) {
      reject("user-specified rejection");
    }
  }
  transformed data {
    real<lower=0> x;
    foo(x);
  }
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
  }
  

