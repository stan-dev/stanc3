functions {
  void foo(tuple(real,) x) {
    return;
  }
}
transformed data {
  tuple(real,) x;
  
  x = (3.5,);
  
  foo(x);
}
