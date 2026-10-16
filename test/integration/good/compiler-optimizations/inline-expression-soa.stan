functions {
  matrix foo(matrix i) {
    return i * i;
  }
}
parameters {
  matrix[10, 10] p;
}
model {
  to_vector(foo(p + p)) ~ std_normal();
}
