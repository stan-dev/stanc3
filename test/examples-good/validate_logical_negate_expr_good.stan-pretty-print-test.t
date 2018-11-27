  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//validate_logical_negate_expr_good.stan"
  transformed data {
    int n;
    real x;
    n <- !n;
    x <- !x;
  }
  parameters {
    real y;
  }
  transformed parameters {
    real xt;
    xt <- !xt;
  }
  model {
    y ~ normal(0, 1);
  }
  

