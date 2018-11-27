  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//validate_jacobian_warning4.stan"
  parameters {
    real y;
  }
  model {
    1 + (y * y) ~ normal(0, 1);
  }
  

