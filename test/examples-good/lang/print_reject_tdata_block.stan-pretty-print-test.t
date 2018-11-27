  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//lang/print_reject_tdata_block.stan"
  transformed data {
    print("In transformed data");
    print("quitting time");
    reject("QUIT");
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
  }
  generated quantities {
    print("In generated quantities");
  }
  

