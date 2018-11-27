  $ $TESTDIR//../../_build/default/stanc.exe "$TESTDIR//functions-bad22-ode.stan"
  Semantic error at file ".*/examples-bad//functions-bad22-ode.stan", line 15, characters 11-82: (re)
  Ill-typed arguments supplied to function integrate_ode_rk45. Available signatures: 
  ((real, real[], real[], data real[], data int[]) => real[], real[], data real, data real[], real[], data real[], data int[], data real, data real, data real) => real[][]
  ((real, real[], real[], data real[], data int[]) => real[], real[], data real, data real[], real[], data real[], data int[]) => real[][]
  Instead supplied arguments of incompatible type: (real, real[], real[], real[], int[]) => real[], real[], real, real[], real[], real[], int[].
  [1]

