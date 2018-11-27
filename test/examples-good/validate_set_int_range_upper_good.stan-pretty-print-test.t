  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//validate_set_int_range_upper_good.stan"
  data {
    int<upper=1> a;
    int<lower=1, upper=3> b;
  }
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
  }
  

