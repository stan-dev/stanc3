generated quantities {
  array[3] real x = {1.0, 2.0, 3.0};
  array[3] real y = {4.0, 5.0, 6.0};
  x[1] += y[1];
  x += y;
}
