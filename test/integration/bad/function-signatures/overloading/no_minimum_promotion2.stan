functions {
   real foo(real x, real y){
     return y;
   }

   real foo(int x, complex y){
     return get_imag(y);
   }
}

model {
   print(foo(1,2));
}
