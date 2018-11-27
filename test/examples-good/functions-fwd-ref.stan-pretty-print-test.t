  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//functions-fwd-ref.stan"
  functions {
    int foo(int n);
    int foo(int n) {
      if (n == 0) return 1;
      return n * foo(n - 1);
    }
  }
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
  }
  

