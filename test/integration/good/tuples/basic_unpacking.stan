transformed data {
  int x = 3;
  int y = 4;
  print(x, y);
  (x, y) = (5, 6);
  print(x, y);
  (x, y) = (y, x);
  print(x, y);
}
