  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//keywords-while.stan"
  parameters {
    real whiley;
  }
  model {
    whiley ~ normal(0, 1);
  }
  

