data {
  tuple(array[3] real, tuple(real, vector[4], array[4] real), int) complicated;
}
model {
  array[3] real x;
  real y;
  vector[4] z;
  array[4] real w;
  int i;

  (x, (y, z, w), i) = complicated;
}
