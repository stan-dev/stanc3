functions {
   real something(int x){
     return 1.0;
   }


    real something(real x){
      return 2.0;
    }

  real something(complex x){
      return 3.0;
    }

}

model {
   print(something(1));
}
