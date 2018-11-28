  $ $TESTDIR/..//../../_build/default/stanc.exe "$TESTDIR/..//ode/bad_t_type_rk45_control.stan"
  Semantic error at file ".*/examples-bad/ode/..//ode/bad_t_type_rk45_control.stan", line 26, characters 11-351: (re)
  Ill-typed arguments supplied to function integrate_ode_rk45. Available signatures: 
  ((real, real[], real[], data real[], data int[]) => real[], real[], data real, data real[], real[], data real[], data int[], data real, data real, data real) => real[][]
  ((real, real[], real[], data real[], data int[]) => real[], real[], data real, data real[], real[], data real[], data int[]) => real[][]
  Instead supplied arguments of incompatible type: (real, real[], real[], real[], int[]) => real[], real[], matrix, real[], real[], real[], int[], real, real, int.
  [1]

