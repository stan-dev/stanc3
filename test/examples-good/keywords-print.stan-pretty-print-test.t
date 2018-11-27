  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//keywords-print.stan"
  parameters {
    real printer;
  }
  model {
    printer ~ normal(0, 1);
  }
  

