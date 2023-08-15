functions {
   real foo_lpdf(real x, int y, real z){
     return y;
   }

   real foo_lpdf(real x, real y, int z){
     return x/y;
   }
}

model {
   1 ~ foo(2,3);
  //  target += foo_lpmf(1,2,3);
}
