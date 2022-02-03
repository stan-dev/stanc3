functions {
  int is_real(int x);
  int is_real(real x);

   int is_real(real x){
     return 1;
   }

   int is_real(int x){
     return 0;
   }
}

transformed data {
   if (is_real(1.5)){
     print(is_real(0));
   }
}
