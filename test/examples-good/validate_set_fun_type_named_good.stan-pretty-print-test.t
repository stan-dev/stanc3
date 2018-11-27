  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//validate_set_fun_type_named_good.stan"
  transformed data {
    real x;
    x <- exp(x);
  }
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
  }
  

