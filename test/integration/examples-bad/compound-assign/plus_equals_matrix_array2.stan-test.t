  $ $TESTDIR/..//../../_build/default/stanc.exe "$TESTDIR/..//compound-assign/plus_equals_matrix_array2.stan"
  Semantic error at file ".*/examples-bad/compound-assign/..//compound-assign/plus_equals_matrix_array2.stan", line 7, characters 2-17: (re)
  Ill-typed arguments supplied to assignment operator +=: lhs has type matrix and rhs has type row_vector. Available signatures:
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

