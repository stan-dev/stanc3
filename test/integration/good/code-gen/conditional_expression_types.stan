parameters {
  vector[2] a;
  vector[2] b;
}
transformed parameters {
  vector[2] c = sum(a) > 1 ? 2*a+b : 2*a;
}
