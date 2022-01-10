functions {
  vector lr_lp(vector beta, vector theta, array[] real x, array[] int y) {
    real lp = bernoulli_logit_lpmf(y | beta[1] + to_vector(x) * beta[2]);
    return [lp]';
  }
}
data {
  // N = 12 data points
  array[12] int y;
  array[12] real x;
}
transformed data {
  // K = 3 shards
  array[3, 4] int ys = { y[1:4], y[5:8], y[9:12] };
  array[3, 4] real xs = { x[1:4], x[5:8], x[9:12] };
  array[3] vector[0] theta;
}
parameters {
  vector[2] beta;
}
model {
  target += sum(map_rect(lr_lp, beta, theta, xs, ys));
}
