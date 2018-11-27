  $ $TESTDIR//../../_build/default/stanc.exe "$TESTDIR//validate_conditional_op_bad-1.stan"
  Semantic error at file ".*/examples-bad//validate_conditional_op_bad-1.stan", line 5, characters 7-19: (re)
  Ill-typed arguments supplied to ? : operator. Available signatures: 
  (int, matrix[][][][][][][], matrix[][][][][][][]) => matrix[][][][][][][]
  (int, row_vector[][][][][][][], row_vector[][][][][][][]) => row_vector[][][][][][][]
  (int, vector[][][][][][][], vector[][][][][][][]) => vector[][][][][][][]
  (int, int[][][][][][][], int[][][][][][][]) => int[][][][][][][]
  (int, real[][][][][][][], real[][][][][][][]) => real[][][][][][][]
  (int, matrix[][][][][][], matrix[][][][][][]) => matrix[][][][][][]
  (int, row_vector[][][][][][], row_vector[][][][][][]) => row_vector[][][][][][]
  (int, vector[][][][][][], vector[][][][][][]) => vector[][][][][][]
  (int, int[][][][][][], int[][][][][][]) => int[][][][][][]
  (int, real[][][][][][], real[][][][][][]) => real[][][][][][]
  (int, matrix[][][][][], matrix[][][][][]) => matrix[][][][][]
  (int, row_vector[][][][][], row_vector[][][][][]) => row_vector[][][][][]
  (int, vector[][][][][], vector[][][][][]) => vector[][][][][]
  (int, int[][][][][], int[][][][][]) => int[][][][][]
  (int, real[][][][][], real[][][][][]) => real[][][][][]
  (int, matrix[][][][], matrix[][][][]) => matrix[][][][]
  (int, row_vector[][][][], row_vector[][][][]) => row_vector[][][][]
  (int, vector[][][][], vector[][][][]) => vector[][][][]
  (int, int[][][][], int[][][][]) => int[][][][]
  (int, real[][][][], real[][][][]) => real[][][][]
  (int, matrix[][][], matrix[][][]) => matrix[][][]
  (int, row_vector[][][], row_vector[][][]) => row_vector[][][]
  (int, vector[][][], vector[][][]) => vector[][][]
  (int, int[][][], int[][][]) => int[][][]
  (int, real[][][], real[][][]) => real[][][]
  (int, matrix[][], matrix[][]) => matrix[][]
  (int, row_vector[][], row_vector[][]) => row_vector[][]
  (int, vector[][], vector[][]) => vector[][]
  (int, int[][], int[][]) => int[][]
  (int, real[][], real[][]) => real[][]
  (int, matrix[], matrix[]) => matrix[]
  (int, row_vector[], row_vector[]) => row_vector[]
  (int, vector[], vector[]) => vector[]
  (int, int[], int[]) => int[]
  (int, real[], real[]) => real[]
  (int, matrix, matrix) => matrix
  (int, row_vector, row_vector) => row_vector
  (int, vector, vector) => vector
  (int, int, int) => int
  (int, real, real) => real
  Instead supplied arguments of incompatible type: row_vector[][], int, int.
  [1]

