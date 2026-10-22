/* $ stanc --auto-format --canonicalize strip-comments,braces,parentheses,deprecations ../blocks.stan : [exit 0] */
data {
  int N;
  vector[N] y;
}
model {
  int j = 0;
  
  for (i in 1 : N) {
    target += i;
  }
  for (i in 1 : N) {
    target += i;
  }
  
  while (j < N) {
    j = j + 1;
  }
  while (j < N) {
    j = j + 1;
  }
  while (j < N) {
    j = j + 1;
  }
  
  if (N > 5) {
    y ~ std_normal();
  }
  if (N > 5) {
    y ~ std_normal();
  }
  
  if (N > 5) {
    y ~ std_normal();
  } else {
    y ~ std_normal();
  }
  
  if (N > 5) {
    y ~ std_normal();
  } else {
    y ~ std_normal();
  }
  
  if (N > 5) {
    y ~ std_normal();
  } else {
    y ~ std_normal();
  }
  
  if (N > 5) {
    y ~ std_normal();
  } else {
    y ~ std_normal();
  }
  
  if (N > 5) {
    y ~ std_normal();
  } else {
    y ~ std_normal();
  }
  
  if (N > 5) {
    y ~ std_normal();
  } else if (N > 5) {
    y ~ std_normal();
  }
  
  if (N > 5) {
    y ~ std_normal();
  } else if (N > 5) {
    y ~ std_normal();
  }
  
  if (N > 5) {
    if (N > 5) {
      y ~ std_normal();
    } else {
      y ~ std_normal();
    }
  }
  
  if (N > 5) {
    if (N > 5) {
      y ~ std_normal();
    }
  } else {
    y ~ std_normal();
  }
  
  if (N > 5) {
    y ~ std_normal();
  } else if (N > 5) {
    y ~ std_normal();
  } else if (N > 5) {
    y ~ std_normal();
  }
  
  if (N > 5) {
    y ~ std_normal();
  } else if (N > 5) {
    y ~ std_normal();
  } else if (N > 5) {
    y ~ std_normal();
  }
  
  if (N > 5) {
    y ~ std_normal();
  } else if (N > 5) {
    y ~ std_normal();
  } else if (N > 5) {
    y ~ std_normal();
  }
  
  if (N > 5) {
    y ~ std_normal();
  } else {
    y ~ std_normal();
  }
  
  if (N > 5) {
    y ~ std_normal();
  } else {
    y ~ std_normal();
  }
  
  if (N > 5) {
    y ~ std_normal();
  } else {
    y ~ std_normal();
  }
  
  if (N > 5) {
    y ~ std_normal();
  } else {
    y ~ std_normal();
  }
}

