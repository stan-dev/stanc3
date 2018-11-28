  $ $TESTDIR/..//../../_build/default/stanc.exe "$TESTDIR/..//compound-assign/plus_equals_sliced.stan"
  Semantic error at file ".*/examples-bad/compound-assign/..//compound-assign/plus_equals_sliced.stan", line 6, characters 4-27: (re)
  Ill-typed arguments supplied to assignment operator +=: lhs has type vector and rhs has type matrix. Available signatures:
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

