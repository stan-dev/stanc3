functions {
  real not_endless() {
    while (1 < 0) {
      return 2.0;
    }
  }
}

transformed parameters {
  real a = not_endless();
}
