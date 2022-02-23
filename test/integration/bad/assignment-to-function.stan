functions {
   real foo(int x){
     return 1.0;
   }

   real bar(int y){
     return 2.0;
   }

   void baz(){
     // NB: putting this in later blocks produce an error due to assigning to a global variable
     foo = bar;
   }
}

model {
     print(foo(1));
    baz();
   print(foo(1));
}
