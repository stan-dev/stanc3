functions {
  void f(tuple(matrix, matrix) x) {
    print(x.1);
    print(x.2);
  }

  void g(tuple(matrix, int, array[] real) x) {
    print(x.1);
    print(x.2);
    print(x.3);
  }
}
data {
  matrix[2, 2] x;
  array[10] real y;
}
parameters {
  matrix[3, 3] m1, m2;
}
model {
  tuple(matrix[2, 2], matrix[3, 3]) temp = (x, m1 * m2);
  f(temp);

  f((x, m1 * m2));

  tuple(matrix[3, 3], int, array[10] real) temp2 = (m1 + m2, 1, y);
  g(temp2);

  g((m1 + m2, 1, y));
}
