  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//declare-define-var-vec-runtime-error.stan"
  data {
    vector[7] b0;
  }
  transformed data {
    vector[8] td_b2 = b0;
  }
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
  }
  

