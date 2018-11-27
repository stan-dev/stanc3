  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//validate_jacobian_warning1.stan"
  parameters {
    real y;
  }
  model {
    log(y) ~ normal(0, 1);
  }
  

