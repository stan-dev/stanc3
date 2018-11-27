  $ $TESTDIR//../../_build/default/stanc.exe "$TESTDIR//signature_function_known.stan"
  Semantic error at file ".*/examples-bad//signature_function_known.stan", line 8, characters 21-50: (re)
  Ill-typed arguments supplied to function bernoulli_logit_log. Available signatures: 
  (int[], row_vector) => real
  (int[], vector) => real
  (int[], real[]) => real
  (int[], real) => real
  (int, row_vector) => real
  (int, vector) => real
  (int, real[]) => real
  (int, real) => real
  Instead supplied arguments of incompatible type: vector, vector.
  [1]

