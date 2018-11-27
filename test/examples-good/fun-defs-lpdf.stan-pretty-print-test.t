  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//fun-defs-lpdf.stan"
  functions {
    real bar_baz_lpdf(real a, real b) {
      return a / b;
    }
  }
  parameters {
    real y;
  }
  model {
    y ~ bar_baz(3.2);
  }
  

