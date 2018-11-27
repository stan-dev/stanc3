  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//lp_in_fun.stan"
  functions {
    void foo_lp() {
      print("lp__ = ", get_lp());
    }
  }
  model {
  
  }
  

