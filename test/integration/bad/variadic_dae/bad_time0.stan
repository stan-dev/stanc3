functions {
  vector residual(real time, vector state, vector deriv) {
    return [1,2,3]';
  }
}

parameters {
  real p;
}

transformed parameters {
  array[2] vector[3] S;
  S = dae(residual, [1, 1]', [1, -1]', p, {1, 2, 3});

}
