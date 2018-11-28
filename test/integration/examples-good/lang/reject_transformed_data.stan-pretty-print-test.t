  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//lang/reject_transformed_data.stan"
  transformed data {
    real<lower=0> x;
    reject("user-specified rejection");
  }
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
  }
  generated quantities {
    print("generating quantities");
  }
  

