/* $ stanc --auto-format ../simple3.stan : [exit 0] */
transformed data {
  tuple(array[2, 3] real, int) x = ({{1.01, 3.14}, {1.01, 3.14},
                                     {1.01, 3.14}},
                                    2);
  print(x.1[2]);
}

