  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//validate_set_int_range_lower_good.stan"
  data {
    int<lower=1> a;
    int<lower=1, upper=3> b;
  }
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
  }
  

