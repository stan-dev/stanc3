  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//int_div_user.stan"
  data {
    int a[4];
    int b[3];
  }
  transformed data {
    int c;
    c <- a[1] / b[2];
  }
  model {
  
  }
  

