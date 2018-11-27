  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//fun_log_forward_decl.stan"
  functions {
    real n_log(real y);
    real n_log(real y) {
      return -0.5 * square(y);
    }
  }
  parameters {
    real mu;
  }
  model {
    mu ~ n();
    increment_log_prob(n_log(mu));
  }
  

