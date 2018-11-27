  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//get-lp-target.stan"
  parameters {
    real<lower=0> y;
  }
  transformed parameters {
    print("target = ", target());
    print("get_lp = ", get_lp());
  }
  model {
    print("target = ", target());
    print("get_lp = ", get_lp());
    y ~ normal(0, 1);
  }
  

