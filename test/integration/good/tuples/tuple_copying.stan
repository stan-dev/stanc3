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

  void h(tuple(real, tuple(matrix, array[] real)) x) {
    print(x.1);
    print(x.2.1);
    print(x.2.2);
  }
}
data {
  matrix[2, 2] x;
  array[10] real y;
}
transformed data {
  tuple(matrix[2, 2], array[10] real) data_tuple;
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

  // note: these will have additional copies for now
  tuple(real, tuple(matrix[2, 2], array[10] real)) temp3 = (1, data_tuple);
  h(temp3);
  h((1, data_tuple));
  h((1, (x, y)));
}
