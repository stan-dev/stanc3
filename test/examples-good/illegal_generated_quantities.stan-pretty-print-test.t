  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//illegal_generated_quantities.stan"
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
  }
  generated quantities {
    real<lower=0> x;
    x = -1;
  }
  

