  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//old-log-funs.stan"
  transformed data {
    real x;
    x = multiply_log(x, x);
    x = binomial_coefficient_log(x, x);
    x = lmultiply(x, x);
    x = lchoose(x, x);
  }
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
  }
  

