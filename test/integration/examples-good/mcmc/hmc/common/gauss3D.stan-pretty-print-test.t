  $ $TESTDIR/../../..//../../_build/default/stanc.exe --auto-format "$TESTDIR/../../..//mcmc/hmc/common/gauss3D.stan"
  parameters {
    real x[3];
  }
  model {
    x ~ normal(0, 1);
  }
  

