functions {
   void printer(array[] real x){
     print(x);
   }

   void data_printer(data array[] real x){
     print(x[0]);
   }
   real nested(array[,] complex zs){
     return num_elements(zs);
   }
}

data {
   int N;
   array[N] int xs;
}

parameters {
  real r;
}

transformed parameters {
   array[2,2] real zs = {{2,3},{7,0.5}};
   array[2] complex z1 = {1,3};
   array[2] complex z2 = {1,3.5};
   array[2] complex z3 = {1,3.5i};
   z3 = {3.5i, r};
}

model {
  array[3] int d = {1,2,3};
  printer(d);
  data_printer(xs);
  printer(xs);
  print(nested({d,d}));
  print(nested(zs));
}
