functions {
  real foo(){
    return 1.0;
  }
}

model {
  print(foo);
}
