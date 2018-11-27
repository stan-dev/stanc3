  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//services/test_gq2.stan"
  parameters {
    real<lower=-10, upper=10> y[2];
  }
  model {
    y ~ normal(0, 1);
  }
  generated quantities {
    print("no QoIs");
  }
  

