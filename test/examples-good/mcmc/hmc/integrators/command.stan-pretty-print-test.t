  $ $TESTDIR/../../..//../../_build/default/stanc.exe --auto-format "$TESTDIR/../../..//mcmc/hmc/integrators/command.stan"
  data {
    real y;
  }
  parameters {
    real mu;
  }
  model {
    y ~ normal(mu, 1);
  }
  

