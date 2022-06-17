functions {
   complex ident (complex x){
     return x;
   }

   real foo(array[] real zs){
     return zs[0];
   }
}

generated quantities {
   real z = 1;
   complex zi = ident(z);
   array[4] int zs = {1,2,3,4};
   real x = foo(zs);
}
