  $ $TESTDIR/..//../../_build/default/stanc.exe "$TESTDIR/..//function-signatures/rising_factorial.stan"
  Semantic error at file ".*/examples-bad/function-signatures/..//function-signatures/rising_factorial.stan", line 7, characters 29-61: (re)
  Ill-typed arguments supplied to function rising_factorial. Available signatures: 
  (int, int) => int
  (real, int) => real
  Instead supplied arguments of incompatible type: real, real.
  [1]

