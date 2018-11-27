  $ $TESTDIR/..//../../_build/default/stanc.exe "$TESTDIR/..//function-signatures/falling_factorial.stan"
  Semantic error at file ".*/examples-bad/function-signatures/..//function-signatures/falling_factorial.stan", line 7, characters 27-60: (re)
  Ill-typed arguments supplied to function falling_factorial. Available signatures: 
  (int, int) => int
  (real, int) => real
  Instead supplied arguments of incompatible type: real, real.
  [1]

