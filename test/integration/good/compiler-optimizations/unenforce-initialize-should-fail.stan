transformed data {
  array[2] vector[5] inputs = rep_array(zeros_vector(5), 2);
}
parameters {
  real param;
}
transformed parameters {
  array[2] vector[5] local = inputs;
  for(i in 1:2) {
    for(j in 1:5) {
      local[i, j] += 1;
    }
  }
}
model {
  param ~ std_normal();
}