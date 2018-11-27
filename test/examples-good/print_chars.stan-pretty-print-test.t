  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//print_chars.stan"
  parameters {
    real y;
  }
  model {
    y ~ normal(0, 1);
    print("a e z A R Z 0 4 9 / ~ ! @ # $ % ^ & * ( ) ` _ + - = { } | [ ] : ; ' < > ? , . / ");
  }
  

