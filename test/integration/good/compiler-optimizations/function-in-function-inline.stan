functions {
  real inner_lpdf(vector phi) {
    return sum(phi);
  }

  real outer_lpdf(vector phi) {
    return inner_lpdf(phi);
  }
}
data {
  vector[10] phi;  // standardized spatially smoothed spatial effects
}
model {
  real ret = outer_lpdf(phi);
  target += ret;
}
