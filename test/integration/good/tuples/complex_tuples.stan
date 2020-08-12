functions {
  (int, array[] (int, real)) f(int x, array[,] (array[] (int,int), array[,] int) x2) {}
}
transformed data {
  (int, real) x;
  (array[10] int, real) y;
  array[5] (array[10] (int, array[1,2,3] real), real) y2;
  x.1 = 1;
  int z = (x).1;
  int b = x.1;
  real c = y2[1].1[1].2[1];

  (int, real, array[2] (int, int)) d = (1, 2.5, {(1,2), (3,4)});
}
