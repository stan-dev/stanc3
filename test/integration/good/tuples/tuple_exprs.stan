generated quantities {
  int x = 3;
  int y = 4;
  complex z;
  matrix[2, 4] foo;

  print(((z, x), {1, 2}, 3));
  print((x, (1, x)));
  print((foo, 2, 3, x));
}
