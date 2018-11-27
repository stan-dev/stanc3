  $ $TESTDIR//../../_build/default/stanc.exe "$TESTDIR//validate_logical_negate_expr_bad.stan"
  Semantic error at file ".*/examples-bad//validate_logical_negate_expr_bad.stan", line 3, characters 7-9: (re)
  Ill-typed arguments supplied to prefix operator !. Available signatures: 
  (real) => int
  (int) => int
  Instead supplied argument of incompatible type: vector.
  [1]

