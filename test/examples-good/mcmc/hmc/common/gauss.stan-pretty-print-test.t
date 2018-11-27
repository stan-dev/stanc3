  $ $TESTDIR/../../..//../../_build/default/stanc.exe --auto-format "$TESTDIR/../../..//mcmc/hmc/common/gauss.stan"
  parameters {
    real x;
  }
  model {
    x ~ normal(0, 1);
  }
  

