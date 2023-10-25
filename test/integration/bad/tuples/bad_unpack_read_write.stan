generated quantities {
  int x = 1;
  vector[2] y = [1, 1]';
  (x, y[x]) = (2, 2);
}
