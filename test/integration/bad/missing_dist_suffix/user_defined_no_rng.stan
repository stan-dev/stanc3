functions {
   real bar_lpdf(real x){
     return 1.0;
   }
}
generated quantities {
   real x = bar_rng(10);
}