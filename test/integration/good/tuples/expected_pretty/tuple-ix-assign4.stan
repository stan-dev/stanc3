/* $ stanc --auto-format ../tuple-ix-assign4.stan : [exit 0] */
transformed data {
  tuple(array[10] tuple(int, array[100] real), int) x;
  x.1[1].1 = 5;
}

