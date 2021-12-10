model {
  array[1, 1] real T = {{42.0}};
  1 ~ normal(0, 1) T[1, T[1, 1]];
  print(T[1, 1]);
}

