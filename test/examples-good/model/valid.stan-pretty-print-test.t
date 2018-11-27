  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//model/valid.stan"
  parameters {
    real x;
  }
  model {
    increment_log_prob(-0.5 * square(x));
  }
  

