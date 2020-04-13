functions {
  real reduce_sum_static(real a){
      return a;
  }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, reduce_sum_static(1));
}