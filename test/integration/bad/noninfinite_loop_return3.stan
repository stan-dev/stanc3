functions {
  real not_endless() {
    while (1) {
      if (0) return 1.0;
      if (0) break;
      return 2.0;
    }
  }
}

transformed parameters {
  real a = not_endless();
}
