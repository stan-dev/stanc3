functions {
   real foo_lpmf(int x, int y, real z){
     return y;
   }

   real foo_lpmf(int x, real y, int z){
     return x/y;
   }
}

model {
   1 ~ foo(2,3);
}
