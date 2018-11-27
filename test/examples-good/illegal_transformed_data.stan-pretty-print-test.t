  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//illegal_transformed_data.stan"
  transformed data {
    real<lower=0> x;
    x <- -1;
  }
  model {
  
  }
  

