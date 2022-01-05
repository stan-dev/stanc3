parameters {
  matrix[3,3] a;
}
model {
  array[4] int b;
  matrix[3,3] c;
  c = !b;
}
