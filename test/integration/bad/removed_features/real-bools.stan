model {
  real z = 3.0;
  real x = target();
  array[5] real xyz;

  while (z) {
    // real as boolean value
    if (1.0) {
      // same
    }
    if (x && !z || xyz[3]) {
      // more boolean reals
    }
  }
}
