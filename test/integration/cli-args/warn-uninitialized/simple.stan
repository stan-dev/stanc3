model {
  real theta;
  real zero;
  theta ~ normal(zero, 1);
  zero = 0;
}
