functions {
   real foo_lpdf(real x, int y, real z){
     return 1.0;
   }
}

model {
  1.0 ~ foo(3.5,3);
}
