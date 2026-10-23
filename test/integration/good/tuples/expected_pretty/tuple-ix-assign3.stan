/* $ stanc --auto-format ../tuple-ix-assign3.stan : [exit 0] */
transformed data {
  array[10] tuple(int, int) x;
  x[1].1 = 5;
}

