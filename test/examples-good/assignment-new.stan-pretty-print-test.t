  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//assignment-new.stan"
  transformed data {
    real mu;
    real<lower=0> sigma;
    mu = -1;
    sigma = 3;
  }
  parameters {
    real y;
  }
  model {
    y ~ normal(mu, sigma);
  }
  

