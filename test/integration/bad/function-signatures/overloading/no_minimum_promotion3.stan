functions {
   real foo3(int x, real y, int z){
     return y;
   }

   real foo3(real x, int y, int z){
     return x;
   }

   real foo3(int x, int y, real z){
     return z;
   }

  // red herring - signature should not be listed
   real foo3(real x, real y, real z){
     return x + y + z;
   }
}

model {
   print(foo3(1,2,3));
}
