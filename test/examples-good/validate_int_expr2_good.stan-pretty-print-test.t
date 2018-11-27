  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//validate_int_expr2_good.stan"
  parameters {
    vector[10] y;
  }
  model {
    for (n in 1 : 10) y ~ normal(0, 1);
  }
  

