functions {
   real β_lpdf(real θ, real α, real β) {
      return beta_lpdf(θ | α, β);
   }

   void mañana_lp(real µ){
    target += logit(µ);
   }
}

parameters {
   real u;
}

transformed parameters {
  mañana_lp(u);
}

model {
    u ~ β(2, 2);
    target += β_lpdf(u | 2, 2);
}
