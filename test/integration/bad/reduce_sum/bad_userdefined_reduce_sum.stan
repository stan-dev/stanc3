functions {
  real reduce_sum(real a){
      return a;
  }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, reduce_sum(1));
}