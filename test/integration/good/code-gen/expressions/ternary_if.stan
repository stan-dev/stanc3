data {
  real y;
}
parameters {
  vector[2] a;
  vector[2] b;
}
transformed parameters {
  vector[2] c = sum(a) > 1 ? add(multiply(2.0, a), b) : multiply(2.0, a);
  complex z = 1 ? 3i : 2;
}
model {
}
