  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//model/domain_fail.stan"
  parameters {
    real<lower=0> x;
  }
  model {
    increment_log_prob(-sqrt(-x));
  }
  

