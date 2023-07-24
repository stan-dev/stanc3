transformed parameters {
  tuple(real, matrix[10,10]) x;
  x.1 = 0.4;
}
model {
   target += (0.3, 3.5).1;
   target += x.1;
}
