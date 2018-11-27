  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//keywords-for.stan"
  parameters {
    real force;
  }
  model {
    force ~ normal(0, 1);
  }
  

