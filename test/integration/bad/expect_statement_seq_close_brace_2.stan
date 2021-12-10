parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
generated quantities {
  array[2,3] int vs;
  int z;
  for (v in vs[1]) {
    if (1 == 1) {
      z = 3;




