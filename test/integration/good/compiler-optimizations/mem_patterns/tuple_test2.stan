data {
  matrix[2, 2] x;
}
parameters {
  matrix[3, 3] m1, m2;
}
model {
  tuple(matrix[2, 2], matrix[3, 3]) temp = (x, m1 * m2);
  print(temp.1);
  print(temp.2);
}
