functions {
   real foo_log(real x, int y){
     return y;
   }

   real foo_log(int x, real y){
     return x/y;
   }
}

model {
   1 ~ foo(3);
  //  target += foo_log(1,3);
}
