generated quantities {
  tuple(int,int) x2 = (1,1);
  vector[2] y = [1, 1]';
  // one could imagine allowing this case,
  // but it is both an edge case and very complicated to do so
  (x2.1, y[x2.2]) = (2, 2);
}
