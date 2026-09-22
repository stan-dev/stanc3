functions {
  real piecewise(real x) {
    if (x > 0) {
      return x * x;
    }
    return -x;
  }
}
parameters {
  real theta;
}
model {
  target += piecewise(exp(theta));
}
