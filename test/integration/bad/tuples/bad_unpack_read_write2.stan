generated quantities {
  tuple(int,int) x2 = (1,1);
  vector[2] y = [1, 1]';
  (x2, y[x2.1]) = ((1,1), 2);
}
