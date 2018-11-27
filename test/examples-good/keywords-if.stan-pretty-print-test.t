  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//keywords-if.stan"
  parameters {
    real iffy;
  }
  model {
    iffy ~ normal(0, 1);
  }
  

