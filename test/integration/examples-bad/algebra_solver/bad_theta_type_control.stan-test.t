  $ $TESTDIR/..//../../_build/default/stanc.exe "$TESTDIR/..//algebra_solver/bad_theta_type_control.stan"
  Semantic error at file ".*/examples-bad/algebra_solver/..//algebra_solver/bad_theta_type_control.stan", line 31, characters 10-78: (re)
  Ill-typed arguments supplied to function algebra_solver. Available signatures: 
  ((vector, vector, data real[], data int[]) => vector, vector, vector, data real[], data int[], data real, data real, data real) => vector
  ((vector, vector, data real[], data int[]) => vector, vector, vector, data real[], data int[]) => vector
  Instead supplied arguments of incompatible type: (real[], vector, real[], int[]) => vector, vector, real, real[], int[], real, real, int.
  [1]

