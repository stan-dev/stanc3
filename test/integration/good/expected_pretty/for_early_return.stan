/* $ stanc --auto-format ../for_early_return.stan : [exit 0] */
functions {
  real ends() {
    for (i in 1 : 10) {
      return 1;
    }
  }
}
transformed parameters {
  real a = ends();
}

