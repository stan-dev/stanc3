generated quantities {
  array[3] row_vector[2] x = {[1,2], [3,4], [5,6]};
  array[3] row_vector[2] y = {[1,2], [3,4], [5,6]};
  x[1] += y[1];
  x += y;
}
