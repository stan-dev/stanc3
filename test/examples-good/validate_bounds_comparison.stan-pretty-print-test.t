  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//validate_bounds_comparison.stan"
  data {
    real<lower=if_else(1 < 2, -1, -2)> a;
    real<upper=if_else(1 < 2, -1, -2)> b;
    real<lower=(1 && 3), upper=if_else(1 < 2, -1, -2)> c;
  }
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
  }
  

