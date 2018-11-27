  $ $TESTDIR//../../_build/default/stanc.exe "$TESTDIR//err-minus-types.stan"
  Semantic error at file ".*/examples-bad//err-minus-types.stan", line 4, characters 7-12: (re)
  Ill-typed arguments supplied to infix operator -. Available signatures: 
  (real, matrix) => matrix
  (real, row_vector) => row_vector
  (real, vector) => vector
  (matrix, real) => matrix
  (row_vector, real) => row_vector
  (vector, real) => vector
  (matrix, matrix) => matrix
  (row_vector, row_vector) => row_vector
  (vector, vector) => vector
  (real, real) => real
  (int, int) => int
  Instead supplied arguments of incompatible type: vector, matrix.
  [1]

