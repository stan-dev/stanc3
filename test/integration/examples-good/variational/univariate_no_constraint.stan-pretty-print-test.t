  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//variational/univariate_no_constraint.stan"
  transformed data {
    real y[2];
    real mu0;
    real sigma;
    y[1] <- 1.6;
    y[2] <- 1.4;
    mu0 <- 1.5;
    sigma <- 1.0;
  }
  parameters {
    real mu;
  }
  model {
    mu ~ normal(mu0, sigma);
    for (n in 1 : 2) y[n] ~ normal(mu, 1.0);
  }
  

