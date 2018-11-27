  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//keywords-integrate-ode.stan"
  parameters {
    real integrate_ode_foo;
  }
  model {
    integrate_ode_foo ~ normal(0, 1);
  }
  

