functions {
  real lb_constrain(real x, real y) {
    return x;
  }
}
parameters {
  real<lower=0> x;
}
model {
  x ~ std_normal();
}
