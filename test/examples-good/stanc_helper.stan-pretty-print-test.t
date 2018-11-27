  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//stanc_helper.stan"
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
  }
  

