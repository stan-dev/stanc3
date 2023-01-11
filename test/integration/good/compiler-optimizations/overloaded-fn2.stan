functions {
  real foo(vector x) {
    return sum(x);
  }
  void bar_lp() {
    // this `foo` is the one below, not above
    target += foo(1.0)[1];
  }
  vector foo(real x) {
    return [x]';
  }
}
model {
  bar_lp();
}