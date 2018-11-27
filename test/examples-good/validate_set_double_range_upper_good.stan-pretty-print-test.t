  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//validate_set_double_range_upper_good.stan"
  data {
    real<upper=1.2> a;
    real<upper=1> b;
    real<lower=1.2, upper=2> c;
    real<lower=1, upper=1.2> d;
    real<lower=1.1, upper=1.2> e;
    real<lower=1, upper=2> f;
  }
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
  }
  

