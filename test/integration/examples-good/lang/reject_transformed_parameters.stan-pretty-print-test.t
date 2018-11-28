  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//lang/reject_transformed_parameters.stan"
  parameters {
    real y;
  }
  transformed parameters {
    real<lower=0> x;
    reject("user-specified rejection");
  }
  model {
    y ~ normal(0, 1);
  }
  generated quantities {
    print("generating quantities");
  }
  

