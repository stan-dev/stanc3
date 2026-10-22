/* $ stanc --auto-format ../fun_as_stmt.stan : [exit 0] */
// programs to tickle bug 2612
functions {
  void foo_lp() {
    // does nothing
  }
}
model {
  foo_lp();
}

