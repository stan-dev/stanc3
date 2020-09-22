parameters {
  real y[3];

}
transformed parameters {
  real z[poisson_rng(10)];
}
