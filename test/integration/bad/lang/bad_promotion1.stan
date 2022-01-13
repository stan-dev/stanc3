functions {
   void printer(array[] vector x){
     print(x);
   }
}

model {
  array[3] int d = {1,2,3};
  printer(d);
}
