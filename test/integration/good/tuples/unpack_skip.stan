transformed data {
  int x = 3;
  int y = 4;
  print(x, y);
  (x, _, y) = (5, 6, 7);
  print(x, y);
  (x, _, _, y) = (y, y, x, x);
  print(x, y);
}
