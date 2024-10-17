functions {
  real foo(){
    return 1.0;
  }
}

transformed parameters {
  jacobian += foo;
}
