  $ $TESTDIR//../../_build/default/stanc.exe "$TESTDIR//op_logical_negation_bad.stan"
  Semantic error at file ".*/examples-bad//op_logical_negation_bad.stan", line 7, characters 7-9: (re)
  Ill-typed arguments supplied to prefix operator !. Available signatures: 
  (real) => int
  (int) => int
  Instead supplied argument of incompatible type: int[].
  [1]

