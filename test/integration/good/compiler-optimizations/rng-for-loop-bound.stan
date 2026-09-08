functions {
  real loops_rng(vector a) {
    real sum = 0;
    for (i in poisson_rng(10) : 40) {
      sum += std_normal_rng();
    }
    return sum;
  }

  real loops2_rng(vector a) {
    real sum = 0;
    for (i in 0 : poisson_rng(10)) {
      sum += std_normal_rng();
    }
    return sum;
  }
}
