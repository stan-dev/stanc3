parameters {
  vector[2] y;
  
  array[3, 4, 5] real arr;
}
model {
  vector[2] x;
  x[ : ] = y;
  
  array[3, 4, 5] real arr2;
  arr2[ : ,  : ,  : ] = arr;
  
  y ~ std_normal();
  
  for (i in 1 : 3) {
    for (j in 1 : 4) {
      for (k in 1 : 5) {
        arr[i, j, k] ~ std_normal();
      }
    }
  }
  // prevent copy-elision and dead-code-elimination on x
  x[1] = 0;
  arr2[1, 1, 1] = 0;
  print(x[2], arr2[2, 2, 2]);
}
