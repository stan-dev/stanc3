  $ $TESTDIR//../../_build/default/stanc.exe "$TESTDIR//op_minus_bad.stan"
  Semantic error at file ".*/examples-bad//op_minus_bad.stan", line 7, characters 7-9: (re)
  Ill-typed arguments supplied to prefix operator -. Available signatures: 
  (matrix) => matrix
  (row_vector) => row_vector
  (vector) => vector
  (real) => real
  (int) => int
  Instead supplied argument of incompatible type: int[].
  [1]

