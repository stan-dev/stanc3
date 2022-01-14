functions {
   void printer(array[] int x){
     print(x);
   }
}

model {
  array[3] real d = {1,2.2,3};
  printer(d);
}
