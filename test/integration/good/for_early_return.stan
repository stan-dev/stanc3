functions {
  real ends() {
    for (i in 1:10){
      return 1;
    }
  }
}

transformed parameters {
  real a = ends();
}
