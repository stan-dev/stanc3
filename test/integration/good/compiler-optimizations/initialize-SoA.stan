parameters {
  vector[3] y;
}
transformed parameters {
  vector[3] x = y[1 : 3];
}
model {
  x ~ std_normal();
}
