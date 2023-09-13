transformed data {
  int x = 3;
  int y = 4;
  complex z;
  
  (x, y) = (5, 6);
  (x, y) = (y, x);
  (z, x) = (x, y);
}
generated quantities {
  int x_out = x;
  int y_out = y;
}
