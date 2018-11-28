  $ $TESTDIR/..//../../_build/default/stanc.exe "$TESTDIR/..//lang/bad_cov_exp_quad_vec_rvec_data.stan"
  Semantic error at file ".*/examples-bad/lang/..//lang/bad_cov_exp_quad_vec_rvec_data.stan", line 13, characters 28-75: (re)
  Ill-typed arguments supplied to function cov_exp_quad. Available signatures: 
  (row_vector[], row_vector[], real, real) => matrix
  (vector[], vector[], real, real) => matrix
  (real[], real[], real, real) => matrix
  (row_vector[], real, real) => matrix
  (vector[], real, real) => matrix
  (real[], real, real) => matrix
  Instead supplied arguments of incompatible type: vector[], row_vector[], real, real.
  [1]

