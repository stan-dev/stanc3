  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//functions-good1.stan"
  functions {
    real foo0() {
      return 0.0;
    }
    real foo1(real x) {
      return 1.0;
    }
    real foo2(real x, real y) {
      return 2.0;
    }
  }
  data {
    int<lower=0> N[6];
  }
  transformed data {
    real a;
    real b;
    real c;
    a <- foo0();
  }
  

