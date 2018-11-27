  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//fun_as_lhs_sampling.stan"
  transformed data {
    matrix[2, 2] M = rep_matrix(0, 2, 2);
  }
  model {
    to_vector(M) ~ normal(0, 1);
  }
  

