functions {
  real something(int x) {
    return 1.0;
  }
  
  real something(real x) {
    return 2.0;
  }
  
  real something(complex x) {
    return 3.0;
  }
}
model {
  print(something(1));
  print(something(1.0));
  print(something(1i));
}
generated quantities {
  print(something(1));
  print(something(1.0));
  print(something(1i));
}
