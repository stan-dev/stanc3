functions {
  // void return type to check function statement, rather than expression
  void foo_jacobian() {
    jacobian += 1;
  }
}
transformed data {
  foo_jacobian();
}
