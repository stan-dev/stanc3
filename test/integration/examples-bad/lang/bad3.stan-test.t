  $ $TESTDIR/..//../../_build/default/stanc.exe "$TESTDIR/..//lang/bad3.stan"
  Syntax error at file ".*/examples-bad/lang/..//lang/bad3.stan", line 1, characters 15-21, parsing error: (re)
   > data { real a; matrix(2,3) b; } model { a = b; }
  Expected "[" expression "," expression "]" for sizes of matrix.
  
  [1]

