  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//lang/reject_generated_quantities.stan"
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
  }
  generated quantities {
    real<lower=0> x;
    reject("user-specified rejection");
  }
  

