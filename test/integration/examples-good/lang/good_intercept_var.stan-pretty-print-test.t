  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//lang/good_intercept_var.stan"
  data {
    int<lower=0> m1;
    int<lower=0> m2;
    int<lower=0> m3;
    int<lower=0> m4;
  }
  transformed data {
    int intercept;
    intercept = 5;
  }
  model {
  
  }
  

