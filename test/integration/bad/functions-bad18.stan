functions {
  void foo() {
    print("hello, world!");
  }
}
parameters {
  real foo;
}
model {
  foo ~ normal(0.0, 1.0);
  foo();  // the function has been shadowed, this `foo` is a number
}
