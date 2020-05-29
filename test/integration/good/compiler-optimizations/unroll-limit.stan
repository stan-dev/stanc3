generated quantities {
  int x;
  for (i in 1:100) {
    x += 1;
  }
  for (i in 1:20) {
    x -= 1;
  }
}