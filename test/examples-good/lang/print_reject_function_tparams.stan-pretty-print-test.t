  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//lang/print_reject_function_tparams.stan"
  functions {
    void foo(real x) {
      print("quitting time");
      reject("user-specified rejection");
    }
  }
  transformed data {
    print("In transformed data");
  }
  parameters {
    real y;
  }
  transformed parameters {
    real v;
    print("In transformed parameters");
    foo(v);
  }
  model {
    print("In model block.");
    y ~ normal(0, 1);
  }
  generated quantities {
    print("In generated quantities");
  }
  

