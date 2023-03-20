functions {
  real foo(real x, real y) {
    return x / y;
  }
  
  void bar(real x, real y) {
    print(x / y);
  }
}
transformed data {
  real x = foo(1, 2);
  print("x = ", x);
  bar(1, 2);
}
model {
  real x2 = foo(1, 2);
  print("x2 = ", x2);
  bar(1, 2);
}
