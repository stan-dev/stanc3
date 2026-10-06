functions {
  real not_endless() {
    for (i in 1:0){
      return 2.0;
    }
  }
}

transformed parameters {
  real a = not_endless();
}
