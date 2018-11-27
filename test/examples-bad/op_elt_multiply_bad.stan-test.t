  $ $TESTDIR//../../_build/default/stanc.exe "$TESTDIR//op_elt_multiply_bad.stan"
  Semantic error at file ".*/examples-bad//op_elt_multiply_bad.stan", line 7, characters 7-13: (re)
  Ill-typed arguments supplied to infix operator .*. Available signatures: 
  (matrix, matrix) => matrix
  (row_vector, row_vector) => row_vector
  (vector, vector) => vector
  Instead supplied arguments of incompatible type: int[], matrix.
  [1]

