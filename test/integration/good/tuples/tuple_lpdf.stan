functions {
   real foo_lpdf(tuple(real, real) x, tuple(real,real,real) y) {
     return 0;
   }
}

model {
   (1.5, 2) ~ foo((1.2, 1.5,4.5));
}
