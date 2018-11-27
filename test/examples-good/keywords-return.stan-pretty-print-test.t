  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//keywords-return.stan"
  parameters {
    real returning;
  }
  model {
    returning ~ normal(0, 1);
  }
  

