transformed data {
  vector[5] inputs[2] = rep_array(zeros_vector(5), 2);
}
parameters {
  real param;
}
transformed parameters {
  vector[5] local[2] = inputs;
  for(i in 1:2) {
    for(j in 1:5) {
      local[i, j] += 1;
    }
  }
}
model {
  param ~ std_normal();
}