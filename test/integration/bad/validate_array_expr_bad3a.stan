parameters {
  real y;
}
model {
  array[2,1] real loc_real_dim2 = {{},{}}; // cannot be empty
  y ~ normal(0,1);
}
