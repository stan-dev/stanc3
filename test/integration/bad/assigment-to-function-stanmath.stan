functions {
   real foo(real x, real y){
     return 1.0;
   }

   void baz(){
     // NB: putting this in later blocks produce an error due to assigning to a global variable
     foo = pow;
   }
}

model {
     print(foo(2,3));
    baz();
   print(foo(2,3));
}
