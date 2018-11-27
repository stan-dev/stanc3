  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//illegal_transformed_parameters.stan"
  transformed parameters {
    real<lower=0> x;
    x <- -1;
  }
  model {
  
  }
  

