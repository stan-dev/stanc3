  $ $TESTDIR//../../_build/default/stanc.exe "$TESTDIR//op_divide_bad.stan"
  Semantic error at file ".*/examples-bad//op_divide_bad.stan", line 7, characters 7-12: (re)
  Ill-typed arguments supplied to infix operator /. Available signatures: 
  (matrix, matrix) => matrix
  (row_vector, matrix) => row_vector
  
  (matrix, real) => matrix
  (row_vector, real) => row_vector
  (vector, real) => vector
  (real, real) => real
  (int, int) => int
  Instead supplied arguments of incompatible type: int[], matrix.
  [1]

