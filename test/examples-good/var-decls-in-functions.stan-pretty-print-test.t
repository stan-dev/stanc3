  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//var-decls-in-functions.stan"
  functions {
    real[] harm_osc_ode(real t, real[] y, real[] theta, real[] x, int[] x_int) {
      real dydt[size(y)];
      return dydt;
    }
  }
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
  }
  

