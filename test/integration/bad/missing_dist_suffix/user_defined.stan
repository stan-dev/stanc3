functions {
   real bar_lpdf(real x){
     return 1.0;
   }
}
model {
   target += bar_lpmf(19.2);
}