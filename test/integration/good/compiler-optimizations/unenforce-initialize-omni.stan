parameters {
  vector[2] y;
}

model {
  vector[2] x;
  x[:] = y;

  y ~ std_normal();
  // prevent copy-elision and dead-code-elimination on x
  x[1] = 0;
  print(x);
}

