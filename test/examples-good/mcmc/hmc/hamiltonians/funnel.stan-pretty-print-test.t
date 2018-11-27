  $ $TESTDIR/../../..//../../_build/default/stanc.exe --auto-format "$TESTDIR/../../..//mcmc/hmc/hamiltonians/funnel.stan"
  transformed data {
    int N;
    N <- 10;
  }
  parameters {
    real v;
    vector[N] x;
  }
  model {
    v ~ normal(0, 3);
    x ~ normal(0, exp(v));
  }
  

