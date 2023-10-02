transformed data {
  int x = 3;
  int y = 4;
  (x, _, _, y) = (y, x);
  print(x, y);
}
