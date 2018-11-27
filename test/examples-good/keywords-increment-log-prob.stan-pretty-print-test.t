  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//keywords-increment-log-prob.stan"
  parameters {
    real increment_log_prob2;
  }
  model {
    increment_log_prob2 ~ normal(0, 1);
  }
  

