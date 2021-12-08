parameters {
  array[3] real y;

}
transformed parameters {
  real z[poisson_rng(10)];
}
