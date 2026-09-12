parameters {
  real y;
}
model {
  real x = 0;
  for (i in 1 : 0)
    x += 1;
  y ~ std_normal();
}
