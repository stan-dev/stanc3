/* $ stanc --auto-format ../lp_in_fun.stan : [exit 0] */
functions {
  void foo_lp() {
    print("lp__ = ", target());
  }
}
model {
  
}

