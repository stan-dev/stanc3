parameters {
  vector[3] y;
  array[3] vector[4] arr_vec;
}
transformed parameters {
  vector[3] x = y[1 : 3];
}
model {
  x ~ std_normal();
  for (i in 1:3) {
    arr_vec[i] ~ std_normal();
  }
}
