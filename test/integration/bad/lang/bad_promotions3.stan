functions {
   void printer(array[] real x){
     print(x);
   }
}

model {
  array[3] int d = {1,2,3};
  printer({d,d});
}
