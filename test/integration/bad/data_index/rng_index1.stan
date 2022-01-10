parameters {
  array[3] real y;

}
transformed parameters {
  array[poisson_rng(10)] real z;
}
