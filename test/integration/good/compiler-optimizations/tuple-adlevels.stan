data {
  matrix[3, 4] x;
}
parameters {
  real w;
}
transformed parameters {
  print({(w, {x})});
}
