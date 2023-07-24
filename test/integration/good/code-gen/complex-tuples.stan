functions {
  tuple(int, array[] tuple(int, real)) f(int x, array[,] tuple(array[] tuple(int,int), array[,] int) x2) {
    return (1, {(2, 3.4)});
  }
}
transformed data {
  tuple(int, real) x;
  tuple(array[10] int, real) y;
  array[5] tuple(array[10] tuple(int, array[1,2,3] real), real) y2;
  x.1 = 1;
  int z = (x).1;
  int b = x.1;
  real c = y2[1].1[1].2[1,1,1];

  tuple(int, real, array[2] tuple(int, int)) d = (1, 2.5, {(1,2), (3,4)});
}
