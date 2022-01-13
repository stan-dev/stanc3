functions {
   real foo(int x, real y){
     return y;
   }

   real foo(real x, int y){
     return y;
   }
}

model {
   print(foo(1,2));
}
