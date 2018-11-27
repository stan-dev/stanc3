  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//validate_jacobian_warning2.stan"
  parameters {
    real y;
  }
  model {
    (y * y) ~ normal(0, 1);
  }
  

