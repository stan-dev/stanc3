data {
  vector[4] x;
}
parameters {
  vector[4] theta;
}
model {
  target += foo_whatev_lpdf(x, theta);
}
