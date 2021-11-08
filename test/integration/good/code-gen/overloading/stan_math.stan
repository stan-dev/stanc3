functions {
   vector to_vector(real one, real two, real three){
     vector[3] r = [one, two, three]';
     return r;
   }

   void to_vector(int x){
     return;
   }
}

transformed data {
   vector[3] v1 = to_vector(1,2,3);
   vector[3] v2 = to_vector({1,2,3});
   to_vector(3);
}
