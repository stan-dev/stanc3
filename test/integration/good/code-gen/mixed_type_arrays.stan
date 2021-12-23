transformed data {
  array[3] real x;
}
parameters {
  array[3] real xx;
}
transformed parameters {
  array[3, 3] real y = {x, xx, xx};
  array[3, 3] real w = {{1.0, 2, 3}, xx, xx};
  array[3, 3] real td_arr33 = {{1, 2, 3}, {1, 2., 3}, {1., 2., 3}};
}

