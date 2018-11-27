  $ $TESTDIR//../../_build/default/stanc.exe "$TESTDIR//op_transpose_bad.stan"
  Semantic error at file ".*/examples-bad//op_transpose_bad.stan", line 7, characters 7-9: (re)
  Ill-typed arguments supplied to postfix operator '. Available signatures: 
  (matrix) => matrix
  (row_vector) => vector
  (vector) => row_vector
  Instead supplied argument of incompatible type: int[].
  [1]

