/* $ stanc --auto-format ../tuple-ix-assign2.stan : [exit 0] */
transformed data {
  tuple(array[10] int, int) x;
  x.1[1] = 5;
}

