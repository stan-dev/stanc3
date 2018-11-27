  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//validate_jacobian_warning6.stan"
  parameters {
    real y;
  }
  model {
    -(y * y) ~ normal(0, 1);
  }
  

