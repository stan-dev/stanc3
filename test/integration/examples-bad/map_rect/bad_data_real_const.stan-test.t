  $ $TESTDIR/..//../../_build/default/stanc.exe "$TESTDIR/..//map_rect/bad_data_real_const.stan"
  Semantic error at file ".*/examples-bad/map_rect/..//map_rect/bad_data_real_const.stan", line 20, characters 8-70: (re)
  Ill-typed arguments supplied to function map_rect. Available signatures: 
  ((vector, vector, data real[], data int[]) => vector, vector, vector[], data real[][], data int[][]) => vector
  Instead supplied arguments of incompatible type: (vector, vector, real[], int[]) => vector, vector, vector[], real[][], int[][].
  [1]

