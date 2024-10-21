functions {
  real gen(real f) {
    return 1;
  }
}

generated quantities {
  real gen;
  gen = gen(1);
}
