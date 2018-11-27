  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//conditional_condition_good.stan"
  parameters {
    real y;
  }
  model {
    int x;
    if (x) y ~ normal(0, 1);
    else if (!x) y ~ normal(0, 1);
    else y ~ normal(0, 1);
  }
  

