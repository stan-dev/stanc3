  $ $TESTDIR/..//../../_build/default/stanc.exe "$TESTDIR/..//compound-assign/elt_divide_equals_prim.stan"
  Semantic error at file ".*/examples-bad/compound-assign/..//compound-assign/elt_divide_equals_prim.stan", line 4, characters 2-10: (re)
  Ill-typed arguments supplied to assignment operator ./=: lhs has type real and rhs has type real. Available signatures:
  (vector, vector) => void
  (vector, int) => void
  (vector, real) => void
  (row_vector, row_vector) => void
  (row_vector, int) => void
  (matrix, int) => void
  (row_vector, real) => void
  (matrix, real) => void
  (matrix, matrix) => void
  [1]

