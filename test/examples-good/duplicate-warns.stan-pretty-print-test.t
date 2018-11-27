  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//duplicate-warns.stan"
  model {
    real foo;
    foo <- 1;
    increment_log_prob(0);
    foo = get_lp();
    foo = multiply_log(1, 1);
    foo = binomial_coefficient_log(1, 1);
    foo = normal_log(0.5, 0, 1);
    foo = normal_cdf_log(0.5, 0, 1);
    foo = normal_ccdf_log(0.5, 0, 1);
  }
  

