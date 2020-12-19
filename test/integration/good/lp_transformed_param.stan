functions { 
  real test_lp(real r) {
    target += normal_lpdf(r | 0, 1);
    return r;
  }
}
parameters { 
  real y; 
}
transformed parameters { 
  real alpha = test_lp(5.0);
}
model {
  y ~ normal(0, 1); 
}