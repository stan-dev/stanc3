functions {
  int foobar(int x);

  int foobar(int x){
    return 1;
  }
  
  real barfoo(real x);
  // error not defining barfoo
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
