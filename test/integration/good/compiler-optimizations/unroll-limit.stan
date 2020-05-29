generated quantities {
  int x;
  for (i in 1:100) {
    x += 1 + i;
  }
  for (i in 1:20) {
    x += 2 + i;
  }
  for (i in 50:70) {
    x += 3 + i;
  }
  for (i in -10:10) {
    x += 4 + i;
  }
  for (i in -10:40) {
    x += 5 + i;
  }
}