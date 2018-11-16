functions {
  real relative_diff(real x, real y, real max_, real min_) {
    real abs_diff;
    real avg_scale;
    abs_diff = fabs(x - y);
    avg_scale = (fabs(x) + fabs(y)) / 2;
    if ((abs_diff / avg_scale) > max_)
      reject("user-specified rejection, difference above ",max_," x:",x," y:",y);
    if ((abs_diff / avg_scale) < min_)
      reject("user-specified rejection, difference below ",min_," x:",x," y:",y);
    return abs_diff / avg_scale;
  }    
}
transformed data {
  real a =  -9.0;
  real b = -1.0;
  real mx = 1.2;
  real mn = 1.1;
}
parameters {
  real y;
}
model {
  real c;
  c = relative_diff(a,b,mx,mn);
  y ~ normal(0,1);
}
