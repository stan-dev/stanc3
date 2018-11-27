  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//get_lp_good.stan"
  functions {
    real foo_lp(real x) {
      return x + get_lp();
    }
  }
  parameters {
    real y;
  }
  transformed parameters {
    real z;
    z <- get_lp();
  }
  model {
    real w;
    w <- get_lp();
    y ~ normal(0, 1);
  }
  

