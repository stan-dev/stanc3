functions {
  real loops_rng(vector a) {
    vector[poisson_rng(10)] w;
    for (i in 1 : size(w)) {
      w[i] = a[i];
    }
    return sum(w);
  }
}
