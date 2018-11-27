  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//validate_jacobian_warning5.stan"
  parameters {
    vector[10] y;
  }
  model {
    head((y .* y), 2) ~ normal(0, 1);
  }
  

