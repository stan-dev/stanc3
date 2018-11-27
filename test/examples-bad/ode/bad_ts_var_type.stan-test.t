  $ $TESTDIR/..//../../_build/default/stanc.exe "$TESTDIR/..//ode/bad_ts_var_type.stan"
  Semantic error at file ".*/examples-bad/ode/..//ode/bad_ts_var_type.stan", line 26, characters 11-330: (re)
  Ill-typed arguments supplied to function integrate_ode. Available signatures: 
  ((real, real[], real[], data real[], data int[]) => real[], real[], data real, data real[], real[], data real[], data int[]) => real[][]
  Instead supplied arguments of incompatible type: (real, real[], real[], real[], int[]) => real[], real[], real, real[], real[], real[], int[].
  [1]

