data {
  tuple(array[3] int, array[4] int) x;
}
model {
  array[7] real x_flat;

  // we disallow this because it's not obvious if unique
  // locations are being assigned to
  (x_flat[ : 3], x_flat[3 : ]) = x;
}
