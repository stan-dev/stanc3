  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//validate_jacobian_warning3.stan"
  parameters {
    vector[5] v;
  }
  model {
    (v' * v) ~ normal(0, 1);
  }
  

