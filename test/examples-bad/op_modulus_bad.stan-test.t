  $ $TESTDIR//../../_build/default/stanc.exe "$TESTDIR//op_modulus_bad.stan"
  Semantic error at file ".*/examples-bad//op_modulus_bad.stan", line 7, characters 7-12: (re)
  Ill-typed arguments supplied to infix operator %. Available signatures: 
  (int, int) => int
  Instead supplied arguments of incompatible type: int[], matrix.
  [1]

