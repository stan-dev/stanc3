  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//keywords-reject.stan"
  parameters {
    real rejection;
  }
  model {
    rejection ~ normal(0, 1);
  }
  

