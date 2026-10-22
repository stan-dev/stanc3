/* $ stanc --auto-format ../simple.stan : [exit 0] */
transformed data {
  tuple(real, int) x = (3.14, 2);
  print(x.1);
}

