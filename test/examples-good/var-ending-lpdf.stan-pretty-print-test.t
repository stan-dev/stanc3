  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//var-ending-lpdf.stan"
  parameters {
    real mu_lpdf;
  }
  model {
    target += mu_lpdf;
  }
  

