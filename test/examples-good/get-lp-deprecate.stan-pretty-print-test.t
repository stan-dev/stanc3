  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//get-lp-deprecate.stan"
  parameters {
    real<lower=0> y;
  }
  model {
    print("target=", get_lp());
    y ~ normal(0, 1);
  }
  

