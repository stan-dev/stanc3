  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//rng_loc.stan"
  functions {
    real foo_rng(real mu, real sigma) {
      return normal_rng(mu, sigma);
    }
  }
  transformed data {
    real y;
    y = normal_rng(0, 1);
  }
  parameters {
  
  }
  model {
  
  }
  generated quantities {
    real z;
    z = normal_rng(0, 1);
  }
  

