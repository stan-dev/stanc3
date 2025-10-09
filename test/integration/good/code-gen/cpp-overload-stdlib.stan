functions {
  real clamp(real x, real lo, real hi) {
    return fmin(fmax(x, lo), hi);
  }
  real min(real x, real y) {
    return x < y ? x : y;
  }
  real max(real x, real y) {
    return x > y ? x : y;
  }
  tuple(real, real) minmax(real x, real y) {
    return (min(x, y), max(x, y));
  }
}
transformed data {
  real m1 = clamp(0., 1., 2.);
  real m2 = min(1., 2.);
  real m3 = max(1., 2.);
  (m2, m3) = minmax(1., 2.);
  
}
