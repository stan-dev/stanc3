  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//deprecate-old-prob-funs.stan"
  parameters {
    real y;
  }
  model {
    target += normal_log(y, 0, 1) + normal_cdf_log(2, 0, 1) + normal_ccdf_log(3, 0, 1);
  }
  

