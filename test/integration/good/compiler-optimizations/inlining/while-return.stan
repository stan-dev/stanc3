functions {
  real first_pos(real x) {
    real y = x;
    while (1) {
      if (y > 0) {
        return y;
      }
      y += 1;
    }
    return 0;
  }
}
parameters {
  real theta;
}
model {
  target += first_pos(theta);
}
