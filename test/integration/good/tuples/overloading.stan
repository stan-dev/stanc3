functions {
  int f(tuple(int, int, int) x) {
    return x.3;
  }
  int f(tuple(int, int) x) {
    return x.2;
  }
  int f(real x) {
    return 1;
  }
}
transformed data {
  tuple(int, int, int) x3 = (1, 2, 3);
  tuple(int, int) x2 = (1, 2);
  int y3 = f(x3);
  int y2 = f(x2);
  int y4 = f(4.5);
}
