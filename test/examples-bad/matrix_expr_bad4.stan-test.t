  $ $TESTDIR//../../_build/default/stanc.exe "$TESTDIR//matrix_expr_bad4.stan"
  Syntax error at file ".*/examples-bad//matrix_expr_bad4.stan", line 2, characters 27-44, parsing error: (re)
   >   matrix[3,2] td_mat32 = [ [ 1 , [ [ 2 ] ] ];
  Ill-formed phrase. Found an expression. This can be followed by a ",", a "}", a ")", a "]", a "[" or an infix or postfix operator.
  
  [1]

