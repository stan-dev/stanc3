data {
  int x;
}
parameters {
  vector[4] theta;
}
model {
  x ~ foo_whatev(theta);
}
