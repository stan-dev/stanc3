  $ $TESTDIR//../../_build/default/stanc.exe "$TESTDIR//validate_conditional_op_bad-2.stan"
  Semantic error at file ".*/examples-bad//validate_conditional_op_bad-2.stan", line 5, characters 7-20: (re)
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
  Instead supplied arguments of incompatible type: int, real, row_vector[][].
  [1]

