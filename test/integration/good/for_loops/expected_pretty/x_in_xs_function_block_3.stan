/* $ stanc --auto-format ../x_in_xs_function_block_3.stan : [exit 0] */
functions {
  int foo(int a) {
    array[2, 3] int vs;
    int y;
    for (v in vs[1]) 
      y = 3;
    return 0;
  }
}
parameters {
  real y;
}
model {
  y ~ normal(0, 1);
}

