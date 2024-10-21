data {
  int x;
}
parameters {
  vector[4] theta;
}
model {
  x ~ extern(theta);
}
