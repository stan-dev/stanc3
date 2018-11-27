  $ $TESTDIR/..//../../_build/default/stanc.exe "$TESTDIR/..//compound-assign/plus_equals_vector_array.stan"
  Semantic error at file ".*/examples-bad/compound-assign/..//compound-assign/plus_equals_vector_array.stan", line 5, characters 2-9: (re)
  Ill-typed arguments supplied to assignment operator +=: lhs has type vector[] and rhs has type vector[]. Available signatures:
  (vector, vector) => void
  (vector, int) => void
  (vector, real) => void
  (row_vector, row_vector) => void
  (row_vector, int) => void
  (real, int) => void
  (matrix, int) => void
  (row_vector, real) => void
  (real, real) => void
  (matrix, real) => void
  (matrix, matrix) => void
  (int, int) => void
  [1]

