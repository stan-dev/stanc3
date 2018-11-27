  $ $TESTDIR//../../_build/default/stanc.exe "$TESTDIR//local_var_constraint3.stan"
  Syntax error at file ".*/examples-bad//local_var_constraint3.stan", line 5, characters 2-8, parsing error: (re)
   >   matrix<lower=0,upper=1>[3,4] a[5];
  "[" expression "," expression "]" expected after "matrix" in local (or model block) variable declaration. (No transformations/constraints allowed.)
  
  [1]

