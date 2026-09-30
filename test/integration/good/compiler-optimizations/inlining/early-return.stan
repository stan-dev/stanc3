functions {
  void pv(real x) {
    if (x > 0.5) {
      print("big");
      return;
    }
    print("small");
  }
  real pos(real x) {
    if (x > 0) {
      return x;
    }
    reject("negative ", x);
  }
}
parameters {
  real theta;
}
model {
  pv(theta);
  target += pos(theta + 1);
}
