functions {
  real add_one(real x) {
    return x + 1;
  }
}
model {
  real unused = 5;
  target += add_one(2.0);
}
