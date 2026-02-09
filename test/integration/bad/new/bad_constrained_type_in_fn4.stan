functions{
  // TODO: would be nice to specialize for this mistake
  void foo(int<lower=0> x){
    print(x);
  }
}
