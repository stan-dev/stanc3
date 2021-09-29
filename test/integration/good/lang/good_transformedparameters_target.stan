functions {
   void foo_lp(real x){
     target += x;
   }
    void prior_lp(real x){
      x ~ normal(0,1);
    }
}

transformed parameters {
   real x;
   // these two have always worked
   prior_lp(x);  
   foo_lp(1);
   // for consistency we now allow these
   x ~ normal(0,1);
   target += 1;
}