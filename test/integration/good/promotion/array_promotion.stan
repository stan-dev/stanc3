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

transformed parameters {
   array[2,2] real zs = {{2,3},{7,0.5}};
}

model {
  array[3] int d = {1,2,3};
  printer(d);
  data_printer(xs);
  printer(xs);
  print(nested({d,d}));
  print(nested(zs));
}
