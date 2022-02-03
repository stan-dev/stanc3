functions {
  real foo(array[] real x, int s, int end, real z, int k) {
    return 0.0;
  }

  real foo(array[] real x, int s, int end, int z, real k) {
    return 0.0;
  }
}
transformed data {
  real x = reduce_sum(foo, {1, 2, 3, 4, 5, 6}, 1, 2, 3);
}

