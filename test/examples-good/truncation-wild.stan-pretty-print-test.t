  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//truncation-wild.stan"
  model {
    real T[1, 1] = {{42.0}};
    1 ~ normal(0, 1) T[1, T[1, 1]];
    print(T[1, 1]);
  }
  

