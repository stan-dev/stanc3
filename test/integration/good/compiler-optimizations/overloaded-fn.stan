functions {
   real dumb(real x){
     return x + 0.5;
   }

   real dumb(int x){
     return x - 0.5;
   }
}

model {
  // should not be inlined
  target += dumb(1.0);
}
