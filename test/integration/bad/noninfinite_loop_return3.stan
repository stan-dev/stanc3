functions {
  real not_endless() {
    while (1) {
      if (0) return 1.0;
      if (0)
      break;
    }
  }
}

transformed parameters {
  real a = not_endless();
}
