data {
  matrix[10, 10] m1;
  matrix[10, 10] m4;
  array[10] int idx;
  array[10] int idy;
}
parameters {
  matrix[10, 10] m2;
  matrix[10, 10] m3;
}
model {
  matrix[10, 10] m5 = m1 * (m2 .* m3[idx,idy]) + m4;
  matrix[10, 10] m6 = m1 .* m2 + m4;
  matrix[10, 10] m7 = m4 + m1 .* m2;
  target += sum(m5) + sum(m6) + sum(m7);
}
