  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//lang/print_reject_model_block.stan"
  transformed data {
    print("In transformed data");
  }
  parameters {
    real y;
  }
  transformed parameters {
    print("In transformed parameters");
  }
  model {
    print("In model block.");
    y ~ normal(0, 1);
    print("quitting time");
    reject("user-specified rejection");
  }
  generated quantities {
    print("In generated quantities");
  }
  

