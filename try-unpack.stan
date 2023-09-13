transformed data {
  int x = 3;
  int y = 4;
  complex z;

  (x, y) = (5, 6);
  (x, y) = (y, x);
  (z, x) = (x, y);

  matrix[2,4] foo;

  print(((z,x), {1,2}, 3));
  print((x, (1, x)));
  print((foo, 2, 3, x));

}
generated quantities {
  int x_out = x;
  int y_out = y;
}
