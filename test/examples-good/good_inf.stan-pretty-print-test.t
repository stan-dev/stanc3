  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//good_inf.stan"
  data {
    real L;
    real U;
  }
  parameters {
    real<lower=L, upper=U> infty;
  }
  model {
    infty ~ normal(0, 1);
  }
  

