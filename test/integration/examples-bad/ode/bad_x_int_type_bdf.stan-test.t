  $ $TESTDIR/..//../../_build/default/stanc.exe "$TESTDIR/..//ode/bad_x_int_type_bdf.stan"
  Semantic error at file ".*/examples-bad/ode/..//ode/bad_x_int_type_bdf.stan", line 26, characters 11-334: (re)
  Ill-typed arguments supplied to function integrate_ode_bdf. Available signatures: 
  ((real, real[], real[], data real[], data int[]) => real[], real[], data real, data real[], real[], data real[], data int[], data real, data real, data real) => real[][]
  ((real, real[], real[], data real[], data int[]) => real[], real[], data real, data real[], real[], data real[], data int[]) => real[][]
  Instead supplied arguments of incompatible type: (real, real[], real[], real[], int[]) => real[], real[], real, real[], real[], real[], int[][].
  [1]

