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

model {
     mañana_lp(u);
    u ~ β(2, 2);
    target += β_lpdf(u | 2, 2);
}
