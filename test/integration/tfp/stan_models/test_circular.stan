parameters {
  real p;
} 
model {
  target += von_mises_lpdf(p| 1.1, 2.5); 
} 